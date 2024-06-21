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
packages <- c("dplyr", "tidyr", "here", "utils", "testthat")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Functions -------------------------------------------------------------------
get_summary_post <- function(
        stan_summary, par_regex, race, x, state) {
    params <- rownames(stan_summary)
    sel <- str_detect(params, par_regex)
    stan_summary[sel, ] |>
        as_tibble() |>
        mutate(
            ind = str_extract(params[sel], "(\\d+,?)+"),
            race = race,
            x = x,
            state = state
        ) |>
        select(ind, race, x, state, everything())
}

## Load data -------------------------------------------------------------------
fx <- readRDS("data_private/fx_US.rds")
pop_deaths <- readRDS("data_private/national_year_age-sex-race_drug-opioid-mortality.RDS") |>
    rename("age" = "age_years")
pop_est <- pop_deaths |> select("age", "year", "sex", "race_eth", "pop")
mort_est <- pop_deaths |> select("age", "year", "sex", "race_eth", "n_deaths")

fx <- fx |>
    filter(sex == "female", race_eth == "white")
pop_est <- pop_est |>
    filter(sex == "female", race_eth == "white")
mort_est <- mort_est |>
    filter(sex == "female", race_eth == "white")

# MAX_AGE <- mort_est |>
#     pull(age) |>
#     max()
MAX_AGE <- 60
MIN_AGE_F <- fx |> # min fertility age
    filter(fx > 0) |> 
    pull(age) |> 
    min()
MAX_AGE_F <- fx |> # max fertility age
    filter(fx > 0) |> 
    pull(age) |> 
    max()
r_fert <- fx |> # we only need >0 fx data in stan
    filter(age %in% (MIN_AGE_F:MAX_AGE_F)) |>
    pivot_wider(id_cols = "year", names_from = "age", values_from = "fx") |>
    select(-"year") |>
    as.matrix()
n_deaths <- mort_est |>
    filter(age <= MAX_AGE, age >= MIN_AGE_F) |> 
    pivot_wider(id_cols = "year", names_from = "age", values_from = "n_deaths") |>
    select(-"year") |>
    as.matrix()
n_pop <- pop_est |>
    filter(age <= MAX_AGE, age >= MIN_AGE_F) |>
    pivot_wider(id_cols = "year", names_from = "age", values_from = "pop") |>
    select(-"year") |>
    as.matrix()

stan_data <- list(
    "MAX_AGE" = MAX_AGE,                # int
    "MIN_AGE_F" = MIN_AGE_F,            # int
    "MAX_AGE_F" = MAX_AGE_F,            # int
    "N_TIME" = n_distinct(mort_est$year), # int
    "r_fert" = r_fert,                  # matrix[1:N_TIME, MAX_AGE_F:MIN_AGE_F]
    "n_deaths" = n_deaths,              # matrix[1:N_TIME, 0:(MAX_AGE - 1)]
    "n_pop" = n_pop                         # matrix[1:N_TIME, 0:(MAX_AGE - 1)]
)

stan_model <- rstan::stan_model(
    file = "code/03-fit-matrix-projection-parent.stan",
    model_name = "Matrix projection of age distribution of parent"
)

options(mc.cores = max(8, parallel::detectCores()))
rstan::rstan_options(auto_write = TRUE)

stan_fit <- rstan::sampling(
    stan_model, stan_data,
    # warmup = 0, iter = 1, chains = 1,
    warmup = 500, iter = 1000, chains = 4,
    # control = list(adapt_delta = .9, max_treedepth = 12),
    algorithm = "NUTS"
)

rstan::check_hmc_diagnostics(stan_fit)
post <- as.array(stan_fit)
np <- bayesplot::nuts_params(stan_fit)
lp <- bayesplot::log_posterior(stan_fit)
bayesplot::mcmc_nuts_acceptance(np, lp)
bayesplot::mcmc_nuts_divergence(np, lp)
bayesplot::mcmc_nuts_treedepth(np, lp)
bayesplot::mcmc_trace(post, np = np, regex_pars = "u")
bayesplot::mcmc_trace(post, np = np, regex_pars = "f")
