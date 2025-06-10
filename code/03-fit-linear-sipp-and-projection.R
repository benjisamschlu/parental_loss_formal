##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon
##  Date: June 2024
##------------------------------------------------------------------------------
##
rm(list = ls())

## Load packages ---------------------------------------------------------------
packages <- c("dplyr", "tidyr", "readr", "stringr", "here", "utils", "testthat")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}
options(mc.cores = min(4, parallel::detectCores()))
rstan::rstan_options(auto_write = TRUE)
source("code/utils.R")
source("code/helper-stan.R")

## Load data -------------------------------------------------------------------
races <- c(
    "non-hispanic white",
    "non-hispanic black",
    "hispanic"
)
# projection data
projected_mom_dist <- read_csv(here("data", "projected-mom.csv")) |>
    filter(race %in% races) |>
    mutate(age = str_extract(x, "(?<=\\[)\\d+") |> as.numeric())
projected_dad_dist <- read_csv(here("data", "projected-dad.csv")) |>
    filter(race %in% races) |>
    mutate(age = str_extract(x, "(?<=\\[)\\d+") |> as.numeric())

# SIPP estimates
sipp_pr <- lapply(
    PERIODS,
    function(prd)
        read_csv(here("data", paste0("sipp_pr_", prd, ".csv"))) |>
        filter(sex == "all", race %in% races) |>
        select(-"sex") |>
        mutate(age = str_extract(x, "(?<=\\[)\\d+") |> as.numeric())
)

sipp_mx <- lapply(
    PERIODS,
    function(prd)
        read_csv(here("data", paste0("sipp_mx_", prd, ".csv"))) |>
        filter(sex == "all", race %in% races) |>
        select(-"sex") |>
        mutate(age = str_extract(x, "(?<=\\[)\\d+") |> as.numeric())
)
# US life table for 2020
uslt2020 <- read_csv(here("data", "uslt-age-group.csv")) |>
    filter(sex == "all", race %in% races) |>
    select(-"sex") |>
    mutate(age = str_extract(x, "(?<=\\[)\\d+") |> as.numeric())

## Fit stan models -------------------------------------------------------------
stan_model <- rstan::stan_model(
    file = here("code", "stan", "03-fit-linear-sipp-and-projection.stan"),
    model_name = "SIPP and projection with linear models"
)
stan_data <- lapply(
    seq(length(PERIODS)),
    function(ind) {
        get_stan_data(
            sipp_pr[[ind]], sipp_mx[[ind]], 
            select_race = races,
            projected_mom_dist = projected_mom_dist, 
            projected_dad_dist = projected_dad_dist
        )
    }
)

stan_fit <- lapply(
    stan_data,
    function(stndt)
        rstan::sampling(
            stan_model, stndt,
            warmup = NWARMUP, iter = NITER, chains = NCHAINS,
            control = list(
                adapt_delta = ADAPT_DELTA,
                max_treedepth = MAX_TREEDEPTH
            ),
            algorithm = "NUTS"
        )
)

# quick diagnostics
quick_diagnostics <- mapply(
    extract_diagnostics, stan_fit, PERIODS, SIMPLIFY = FALSE
)

write_csv(
    bind_rows(quick_diagnostics), 
    here("data", "posteriors", "linear-sipp-and-projection", "diagnostics.csv")
)

## Extract and save posteriors -------------------------------------------------
o <- lapply(
    seq(length(PERIODS)),
    function(ind) 
        saveRDS(
            stan_fit[[ind]], 
            here(
                "data", "posteriors", "linear-sipp-and-projection",
                paste0("fitted_", PERIODS[ind], ".rds")
            )
        )
)

lt_post <- lapply(stan_fit, function(fit) compute_lt(fit, uslt2020, races))

o <- lapply(
    seq(length(PERIODS)),
    function(ind) 
        write_csv(
            lt_post[[ind]],
            here(
                "data", "posteriors", "linear-sipp-and-projection",
                paste0("lt_post_", PERIODS[ind], ".csv")
            )
        )
)


sig_post <- lapply(stan_fit, function(fit) extract_sigmas(fit))

o <- lapply(
    seq(length(PERIODS)),
    function(ind) 
        write_csv(
            sig_post[[ind]],
            here(
                "data", "posteriors", "linear-sipp-and-projection",
                paste0("sig_post_", PERIODS[ind], ".csv")
            )
        )
)

post_pred <- lapply(stan_fit, function(fit) extract_pp(fit, races))

o <- lapply(
    seq(length(PERIODS)),
    function(ind) 
        write_csv(
            post_pred[[ind]],
            here(
                "data", "posteriors", "linear-sipp-and-projection",
                paste0("lt_post_pred_", PERIODS[ind], ".csv")
            )
        )
)
