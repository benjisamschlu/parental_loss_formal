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
options(mc.cores = max(8, parallel::detectCores()))
rstan::rstan_options(auto_write = TRUE)

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

get_stan_data <- function(fx, mort, pop, sipp_est, sipp_se,
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
    df_p_a <- sipp_est |>
        filter(quantity == "pr", race == select_race) |>
        left_join(
            sipp_se, 
            by = c("race", "x", "quantity", "state"),
            suffix = c("", "_se")
        ) |>
        rename(se = "value_se") |>
        mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
        filter(x < MAX_AGE) 
    p_a_sipp <- df_p_a |>
        pivot_wider(
            values_from = "value", 
            names_from = "state", 
            id_cols = "x"
        ) |>
        select(-"x") |>
        mutate(across(everything(), ~ replace_na(.x, -1))) |>
        as.matrix()
    p_a_sipp_se <- df_p_a |>
        pivot_wider(
            values_from = "se", 
            names_from = "state", 
            id_cols = "x"
        ) |>
        select(-"x") |>
        mutate(across(everything(), ~ replace_na(.x, -1))) |>
        as.matrix()
        
    df_m_a <- sipp_est |>
        filter(quantity == "mx", race == select_race)  |>
        left_join(
            sipp_se, 
            by = c("race", "x", "quantity", "state"),
            suffix = c("", "_se")
        ) |>
        rename(se = "value_se", transition = "state") |>
        mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
        filter(x < MAX_AGE) 
    
    m_a_sipp <- df_m_a |>
        pivot_wider(
            values_from = "value", 
            names_from = "transition", 
            id_cols = "x"
        ) |>
        select(-"x") |>
        mutate(across(everything(), ~ replace_na(.x, -1))) |>
        as.matrix()
    m_a_sipp_se <- df_m_a |>
        pivot_wider(
            values_from = "se", 
            names_from = "transition", 
            id_cols = "x"
        ) |>
        select(-"x") |>
        mutate(across(everything(), ~ replace_na(.x, -1))) |>
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
# SIPP + lifetable data
sipp_est <- read_csv(here("data", "multistate-lt.csv")) |>
    mutate(
        quantity = str_remove(name, "_.+"),
        state = str_remove(name, "^[a-z]+_"),
        value = ifelse(value == 0, NA, value)
    ) |>
    select(race, x, quantity, state, value)

sipp_se <- read_csv(here("data", "se-multistate-lt.csv")) |>
    mutate(
        name = str_remove(name, "^se_"),
        quantity = str_remove(name, "_.+"),
        state = str_remove(name, "^[a-z]+_"),
        value = ifelse(value == 0, NA, value)
    ) |>
    select(race, x, quantity, state, value)

# testing with non-hispanic white
stan_data_white <- get_stan_data(
    fx, mort_est, pop_est, sipp_est, sipp_se, 
    select_race = "non-hispanic white", MAX_AGE = 60)

stan_model <- rstan::stan_model(
    file = "code/03-fit-matrix-projection-parent.stan",
    model_name = "Matrix projection of age distribution of parent"
)

stan_fit <- rstan::sampling(
    stan_model, stan_data_white,
    # warmup = 0, iter = 1, chains = 1,
    warmup = 1000, iter = 2000, chains = 4,
    control = list(adapt_delta = .8, max_treedepth = 8),
    algorithm = "NUTS"
)

# quick diagnostics
rstan::check_hmc_diagnostics(stan_fit)
post <- as.array(stan_fit)
np <- bayesplot::nuts_params(stan_fit)
lp <- bayesplot::log_posterior(stan_fit)
# bayesplot::mcmc_nuts_acceptance(np, lp)
# bayesplot::mcmc_nuts_divergence(np, lp)
# bayesplot::mcmc_nuts_treedepth(np, lp)
bayesplot::mcmc_trace(post, np = np, regex_pars = "phi_sipp\\[\\d+,1")
bayesplot::mcmc_trace(post, np = np, regex_pars = "theta_sipp\\[\\d+,3")
bayesplot::mcmc_trace(post, np = np, regex_pars = "phi_mom\\[\\d+")
bayesplot::mcmc_trace(post, np = np, regex_pars = "phi_dad\\[\\d+")

bayesplot::mcmc_pairs(
    post, np = np, regex_pars = "phi_sipp\\[2,"
)
bayesplot::mcmc_pairs(
    post, np = np, regex_pars = "theta_sipp\\[[1-5],1"
)
bayesplot::mcmc_pairs(
    post, np = np, pars = c("phi_sipp[1,2]", "phi_sigma_mom")
)

bayesplot::mcmc_parcoord(
    post, np = np, regex_pars = "theta_sipp\\[\\d+,1"
)
