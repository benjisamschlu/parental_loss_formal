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
source("code/utils.R")
## Load packages ---------------------------------------------------------------
packages <- c("dplyr", "tidyr", "readr", "stringr", "here", "utils", "testthat")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}
options(mc.cores = max(8, parallel::detectCores()))
rstan::rstan_options(auto_write = TRUE)

## Functions -------------------------------------------------------------------
get_summary_post <- function(
        stan_summary, par_regex, race, par_name) {
    params <- rownames(stan_summary)
    sel <- str_detect(params, par_regex)
    stan_summary[sel, ] |>
        as_tibble() |>
        mutate(
            age = str_extract(params[sel], "\\[\\d+\\]") |>
                str_remove_all("\\[|\\]") |>
                as.numeric(),
            race = race,
            name = par_name
        ) 
}

get_stan_data <- function(fx, mort, pop, sipp_pr, sipp_mx,
                          select_race = "hispanic", MAX_AGE = NULL) {
    ### the function formats input data for stan ###
    sexes <- c(mom = "female", dad = "male")
    fx_parents <- lapply(
        sexes, function(s) filter(fx, sex == s, race_eth == select_race)
    )
    mort_parents <- lapply(
        sexes, function(s) filter(mort, sex == s, race_eth == select_race)
    )
    pop_parents <- lapply(
        sexes, function(s) filter(pop, sex == s, race_eth == select_race)
    )
    # age & time constants
    if (is.null(MAX_AGE)) {
        MAX_AGE <- max(
            unlist(sapply(pop_parents, function(df) df$age))
        )
    }
    MIN_AGE_F <- lapply(
        fx_parents, function(f) filter(f, fx > 0) |> pull("age") |> min()
    )
    MAX_AGE_F <- lapply(
        fx_parents, function(f) filter(f, fx > 0) |> pull("age") |> max()
    )
    N_TIME <- min(
        unlist(sapply(pop_parents, function(p) n_distinct(p$year)))
    )
    # fertility data
    r_fert <- lapply(
        fx_parents, 
        function(f) 
            filter(f, fx > 0) |>
            pivot_wider(
                id_cols = "year", 
                names_from = "age", 
                values_from = "fx"
            ) |>
            mutate(across(everything(), ~ replace_na(.x, 0))) |>
            select(-"year") |>
            as.matrix()
    )
    # mortality data
    n_deaths <- mapply(
        function(m, min_age_f) 
            filter(m, age <= MAX_AGE, age >= min_age_f) |>
            pivot_wider(
                id_cols = "year",
                names_from = "age",
                values_from = "n_deaths"
            ) |>
            mutate(across(everything(), ~ replace_na(.x, 0))) |>
            select(-"year") |>
            as.matrix(),
        mort_parents, MIN_AGE_F
    )
    # population counts
    n_pop <- mapply(
        function(p, min_age_f) 
            filter(p, age <= MAX_AGE, age >= min_age_f) |>
            pivot_wider(
                id_cols = "year", 
                names_from = "age",
                values_from = "pop"
            ) |>
            select(-"year") |>
            mutate(across(everything(), ~ replace_na(.x, 0))) |>
            as.matrix(),
        pop_parents, MIN_AGE_F
    )
    # sipp data
    df_p_a <- sipp_pr |>
        mutate(
            x = str_extract(x, "(?<=\\[)\\d+") |>
                as.numeric()
        ) |>
        filter(x < MAX_AGE, race == select_race)
    p_a_sipp <- df_p_a |>
        select(
            "pr_lost_none",
            "pr_lost_mom",
            "pr_lost_dad",
            "pr_lost_both"
        ) |>
        mutate(across(everything(), ~ replace_na(.x, 0))) |>
        as.matrix()
    p_a_sipp_se <- df_p_a |>
        select(
            "se_pr_lost_none",
            "se_pr_lost_mom",
            "se_pr_lost_dad",
            "se_pr_lost_both"
        ) |>
        mutate(across(everything(), ~ replace_na(.x, 0))) |>
        as.matrix()
        
    df_m_a <- sipp_mx |>
        mutate(
            x = str_extract(x, "(?<=\\[)\\d+") |>
                as.numeric()
        ) |>
        filter(x < MAX_AGE, race == select_race)
    
    m_a_sipp <- df_m_a |>
        select(
            "mx_lost_none_to_lost_mom",
            "mx_lost_none_to_lost_dad",
            "mx_lost_none_to_lost_both",
            "mx_lost_mom_to_lost_both",
            "mx_lost_dad_to_lost_both"
        ) |>
        mutate(across(everything(), ~ replace_na(.x, 0))) |>
        as.matrix()
    m_a_sipp_se <- df_m_a |>
        select(
            "se_mx_lost_none_to_lost_mom",
            "se_mx_lost_none_to_lost_dad",
            "se_mx_lost_none_to_lost_both",
            "se_mx_lost_mom_to_lost_both",
            "se_mx_lost_dad_to_lost_both"
        ) |>
        mutate(across(everything(), ~ replace_na(.x, 0))) |>
        as.matrix()
    # stan data
    list(
        MAX_AGE = MAX_AGE,
        MIN_AGE_F_MOM = MIN_AGE_F[["mom"]],
        MIN_AGE_F_DAD = MIN_AGE_F[["dad"]],
        MAX_AGE_F_MOM = MAX_AGE_F[["mom"]],
        MAX_AGE_F_DAD = MAX_AGE_F[["dad"]],
        N_TIME = N_TIME,
        r_fert_mom = r_fert[["mom"]],
        r_fert_dad = r_fert[["dad"]],
        n_deaths_mom = n_deaths[["mom"]],
        n_deaths_dad = n_deaths[["dad"]],
        n_pop_mom = n_pop[["mom"]],
        n_pop_dad = n_pop[["dad"]],
        p_a_sipp = p_a_sipp,
        p_a_sipp_se = p_a_sipp_se,
        m_a_sipp = m_a_sipp,
        m_a_sipp_se = m_a_sipp_se
    )
}

## Load data -------------------------------------------------------------------
# projection data
fx <- readRDS("data_private/fx_US.rds") |>
    mutate(
        race_eth = case_match(
            race_eth,
            "hispanic" ~ "hispanic",
            "black" ~ "non-hispanic black",
            "white" ~ "non-hispanic white",
            .default = race_eth
        )
    )
pop_deaths <- readRDS("data_private/national_year_age-sex-race_drug-opioid-mortality.RDS") |>
    rename("age" = "age_years") |>
    mutate(
        race_eth = case_match(
            race_eth,
            "hispanic" ~ "hispanic",
            "black" ~ "non-hispanic black",
            "white" ~ "non-hispanic white",
            .default = race_eth
        )
    )
pop_est <- pop_deaths |> select("age", "year", "sex", "race_eth", "pop")
mort_est <- pop_deaths |> select("age", "year", "sex", "race_eth", "n_deaths")
# SIPP estimates
sipp_pr <- read_csv(here("data", "sipp_pr.csv")) |>
    filter(sex == "all")
sipp_mx <- read_csv(here("data", "sipp_mx.csv")) |>
    filter(sex == "all")

## Fit stan models -------------------------------------------------------------
stan_fit <- list()
races <- c("non-hispanic white", "non-hispanic black", "hispanic")
stan_model <- rstan::stan_model(
    file = here("code", "03-fit-parent-matrix-projection.stan"),
    model_name = "Matrix projection of age distribution of parents + SIPP"
)
for (r in races) {
    stan_data <- get_stan_data(
        fx, mort_est, pop_est, sipp_pr, sipp_mx,
        select_race = r, MAX_AGE = 60
    )
    stan_fit[[r]] <- rstan::sampling(
        stan_model, stan_data,
        warmup = 1000, iter = 2000, chains = 4,
        control = list(adapt_delta = .9, max_treedepth = 10),
        algorithm = "NUTS"
    )
}

# # quick diagnostics
# rstan::check_hmc_diagnostics(stan_fit$hispanic)

## Extract and save posteriors -------------------------------------------------
params <- c(
    theta_lost_none_to_lost_mom = "theta_1_2", 
    theta_lost_none_to_lost_dad = "theta_1_3", 
    theta_lost_none_to_lost_both = "theta_1_4", 
    theta_lost_mom_to_lost_both = "theta_2_4", 
    theta_lost_dad_to_lost_both = "theta_3_4", 
    phi_lost_none = "phi_1", 
    phi_lost_mom = "phi_2", 
    phi_lost_dad = "phi_3", 
    phi_lost_both = "phi_4"
)

for (r in races) {
    stan_fit_summary <- rstan::summary(stan_fit[[r]])$summary
    param_post_list <- list()
    for (p in seq(length(params))) {
        param_post_list[[params[p]]] <- get_summary_post(
            stan_fit_summary, params[p], r, names(params)[p]
        )
    }
    param_posts <- bind_rows(param_post_list)
    write_csv(
        param_posts,
        here(
            "data", "posteriors", 
            paste0("parent-matrix-projection-posterior-", r, ".csv")
        )
    )
}

# param_posts |>
#     filter(name == "theta_lost_none_to_lost_mom") |>
#     ggplot(aes(x = age)) +
#     theme_minimal() +
#     geom_line(aes(y = mean)) +
#     geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
#     labs(x = "Age", y = expression({}[1]~m[a](1,2)))
