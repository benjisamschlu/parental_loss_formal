##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon
##  Date: July 2024
##------------------------------------------------------------------------------
## Note on data
## - Missing age-specific population counts for dads in `us_fx_dad.csv` for
##   the following years: 1992, 2001, 2002, 2003, 2011, 2013, 2014

##  Checks ---------------------------------------------------------------------

rm(list = ls())

## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("dplyr", "tidyr", "stringr", "readr", "pracma", 
              "here", "utils", "testthat")
for (p in packages) {
    if (!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}
source("code/utils.R")

## Functions -------------------------------------------------------------------
project_matrix_parentship <- function(fx, n_qx, MAX_AGE = 85, sim = FALSE) {
    # fertile ages only
    fx_ <- fx |> 
        filter(fx > 0, race != "all") |>
        arrange(year, age, race)
    min_age_f <- min(fx_[["age"]])
    n_qx_ <- n_qx |>
        filter(age >= min_age_f, race != "all") |>
        arrange(year, age, race)
    
    if (sim) {
        fx_ <- fx_ |> mutate(fx = fx_sim)
        n_qx_ <- n_qx_ |> mutate(qx = qx_sim)
    }
    
    # dimensions
    n_race <- n_distinct(fx_[["race"]])
    n_time <- n_distinct(fx_[["year"]])
    n_age_f <- n_distinct(fx_[["age"]])
    n_age_p <- n_distinct(n_qx_[["age"]])
    id_cols <- c("year", "race")
    # format matrices
    fx_mat <- fx_ |>
        pivot_wider(
            id_cols = all_of(id_cols),
            names_from = "age",
            values_from = "fx"
        ) |>
        select(-all_of(id_cols)) |>
        as.matrix() |>
        array(dim = c(n_race, n_time, n_age_f))
    qx_mat <- n_qx_ |>
        pivot_wider(
            id_cols = all_of(id_cols),
            names_from = "age",
            values_from = "qx"
        ) |>
        select(-all_of(id_cols)) |>
        as.matrix() |>
        array(dim = c(n_race, n_time, n_age_p))
   n_mat <- n_qx_ |>
       pivot_wider(
           id_cols = all_of(id_cols),
           names_from = "age",
           values_from = "pop"
       ) |>
       mutate(across(everything(), ~ replace_na(.x, 0))) |>
       select(-all_of(id_cols)) |>
       as.matrix() |>
       array(dim = c(n_race, n_time, n_age_p))
   n_births <- fx_mat * n_mat[, , 1:n_age_f]
   # construct survival matrix
   tmp <- vector(mode = "list", length = n_race)
   for (r in 1:n_race) {
       Utilde <- array(0, dim = c(n_time - 1, n_age_p + 1, n_age_p + 1))
       for (t in 1:(n_time - 1)) {
           # survival probabilities 
           Utilde[t, 2:(n_age_p + 1), 1:n_age_p] <- diag(
               1 - qx_mat[r, t, ],
               nrow = n_age_p,
               ncol = n_age_p
           )
           # add death probability in the last row to keep track of annual deaths
           Utilde[t, n_age_p + 1, ] <- c(qx_mat[r, t, ], 0)
       }
       # construct kinship matrix
       K_a_t <- array(0, dim = c(n_time, n_age_p + 1, MAX_AGE + 1)) # include age 0
       for (t in 1:n_time) {
           t_prev <- max(1, t - 1) # to initialize with a stable population
           # distribution of parent age at birth
           K_a_t[t, 1:n_age_f, 1] <- n_births[r, t, ] / sum(n_births[r, t, ])
           # survival projection
           for (a in (1:MAX_AGE) + 1) {
               K_a_t[t, , a] <- Utilde[t_prev, , ] %*% K_a_t[t_prev, , a - 1]
           }
       }
       tmp[[r]] <- data.frame(
           i_lost = K_a_t[n_time, n_age_p + 1, -1],
           p_alive = colSums(K_a_t[n_time, , -1]),
           age = 0:(MAX_AGE - 1)
       ) |>
           mutate(
               x = cut(age, breaks = AGES, right = FALSE)
           ) |>
           summarise(
               .by = "x",
               m_lost = sum(i_lost) / sum(p_alive),
               p_alive = last(p_alive),
               p_lost = 1 - p_live
           ) |>
           mutate(race = r)
   }
   bind_rows(tmp) |>
       mutate(race = sort(unique(fx_[["race"]])[race]))
}

## Load data -------------------------------------------------------------------
us_mort_pop <- read_csv(here("data_private", "us_mort_pop.csv"))
us_fx_mom <- read_csv(here("data_private", "us_fx_mom.csv"))
us_fx_dad <- read_csv(here("data_private", "us_fx_dad.csv"))

## Interpolate and aggregate ---------------------------------------------------
# cubic interpolation of female fx (fx ~ age by year * race)
# the reported fx's last interval is 10 years instead of 5 years
age_i <- seq(9, 55)
us_fx_mom_int <- us_fx_mom |>
    filter(race %in% RACES) |>
    reframe(
        .by = c(year, race, sex),
        fx = interp1(
            c(9, age, 55), 
            c(0, fx, 0),
            xi = age_i,
            method = "cubic"
        )
    ) |>
    mutate(
        .by = c(year, race, sex),
        age = age_i
    ) |>
    filter(!age %in% range(age_i)) # drop the 0 fx ages

# # check 
# library(ggplot2)
# us_fx_mom_int |>
#     ggplot(aes(x = age, y = fx, colour = race)) +
#     theme_minimal() +
#     geom_line() +
#     geom_point(data = us_fx_mom) +
#     facet_wrap(vars(year))

us_pop_qx <- us_mort_pop |>
    filter(race %in% RACES) |>
    mutate(
        mx = n_deaths / pop,
        # earlier age qx are irrelevant since
        # we are only interested in the quantities for parents
        qx = mx / (1 + mx / 2)
    )

## Estimate race-specific male fx ----------------------------------------------
# extrapolate tfr ratio to 2020 based on 2010 to 2015
tfr <- bind_rows(
    us_fx_mom_int |>
        filter(race == "all") |>
        summarise(
            .by = c(year, sex),
            tfr = sum(fx)
        ),
    us_fx_dad |>
        summarise(
            .by = c(year, sex),
            tfr = sum(fx)
        )
    ) |>
    pivot_wider(
        names_from = sex,
        values_from = tfr
    ) |>
    mutate(ratio = male / female)

ratio_lm <- lm(ratio ~ 1 + year, data = tfr |> filter(year %in% 2010:2015))
tfr_ratio_ext <- tfr |>
    mutate(
        ratio = if_else(
            year < 2016, 
            ratio,
            predict(ratio_lm, tfr)
        )
    )

# forecast 2016-2020 male fx using 2010-2015 male fx and tfr/ratio
years201620 <- data.frame(year = 2016:2020)

us_fx_dad_all_ext <- us_fx_dad |>
    filter(year >= 2010) |>
    reframe(
        .by = age,
        fx = lm(log(fx) ~ 1 + year) |> predict(years201620) |> exp(),
        year = years201620$year
    ) |>
    left_join(tfr_ratio_ext |> select(year, female, ratio), by = "year") |>
    # rescale based on female tfr and tfr ratio
    mutate(
        .by = year,
        fx = female * ratio * fx / sum(fx),
        sex = "male"
    ) |>
    # combine with data
    bind_rows(us_fx_dad) |>
    select(year, sex, age, fx)

# race-to-population fx ratio
us_fx_mom_ratio <- us_fx_mom_int |>
    filter(
        race != "all"
    ) |>
    left_join(
        us_fx_mom_int |>
            filter(race == "all") |>
            select(year, age, fx) |>
            rename(fx_all = fx),
        by = c("year", "age")
    ) |>
    mutate(
        # compute race-to-population fx ratios
        .by = c(race, year),
        fx_race_std = fx / sum(fx),
        fx_all_std = fx_all / sum(fx_all),
        ratio = fx_race_std / fx_all_std
    ) |>
    reframe(
        # model race-to-population fx ratios
        .by = c(race, age),
        year = year,
        lm(ratio ~ 1 + year) |> 
            predict(interval = "confidence") |> 
            as_tibble() |>
            select(1:3)
    ) |>
    rename(
        fx_ratio = fit,
        lwr_fx_ratio = lwr,
        upr_fx_ratio = upr
    ) |>
    reframe(
        # use last age group of female for age >= 54
        .by = c(race, year),
        age = c(age, seq(55, 65)),
        fx_ratio = c(
            fx_ratio, 
            rep(last(fx_ratio), 11)
        ),
        lwr_fx_ratio = c(
            lwr_fx_ratio, 
            rep(last(lwr_fx_ratio), 11)
        ),
        upr_fx_ratio = c(
            upr_fx_ratio, 
            rep(last(upr_fx_ratio), 11)
        )
    )
# estimate race-specific male fx
us_fx_dad_est <- us_fx_dad_all_ext |>
    mutate(
        .by = c(year, sex),
        # standardized fx for total population
        fx_all_std = fx / sum(fx)
    ) |>
    left_join(
        us_fx_mom_ratio,
        by = c("year", "age")
    ) |>
    left_join(
        # female tfr
        us_fx_mom_int |>
            filter(race != "all") |>
            summarise(
                .by = c(race, year),
                tfr_female = sum(fx)
            ),
        by = c("race", "year"),
        relationship = "many-to-one"
    ) |>
    left_join(
        # male-to-female tfr ratio
        tfr_ratio_ext |> 
            rename(tfr_ratio = ratio) |> 
            select(year, tfr_ratio),
        by = "year",
        relationship = "many-to-many"
    ) |>
    mutate(
        # estimate race-specific male fx
        fx = fx_ratio * fx_all_std * tfr_female * tfr_ratio,
        lwr_fx = lwr_fx_ratio * fx_all_std * tfr_female * tfr_ratio,
        upr_fx = upr_fx_ratio * fx_all_std * tfr_female * tfr_ratio
    ) |>
    select(
        year, race, sex, age, fx, lwr_fx, upr_fx
    )

# check
# us_fx_dad_est |>
#     ggplot(aes(x = age, y = fx, colour = race)) +
#     geom_line() +
#     theme_minimal() +
#     facet_wrap(vars(year))

## compute dispersion parameter for deaths~ NB ---------------------------------
# sigma^2 = mu + mu^2 / theta (dispersion parameter)
disp_death <- us_pop_qx |>
    filter(race %in% RACES) |>
    summarise(
        .by = c("race", "sex", "age"),
        mu = mean(n_deaths),
        svar = var(n_deaths)
    ) |>
    mutate(
        r_death = mu^2 / (svar - mu),
        # assume Poisson for underdispersed
        r_death = if_else(mu / svar > .9, 9999, r_death)
    )

# disp_death |>
#     filter(r_death < 9999) |>
#     ggplot() +
#     theme_minimal() +
#     geom_point(aes(x = age, y = r_death, colour = sex)) +
#     facet_wrap(vars(race))
    
disp_birth <- bind_rows(
    us_fx_mom_int |> mutate(sex = "female"),
    us_fx_dad_est |> mutate(sex = "male")
) |>
    left_join(us_pop_qx, by = c("year", "race", "sex", "age")) |>
    filter(!is.na(pop)) |>
    mutate(n_birth = fx * pop) |>
    summarise(
        .by = c("race", "sex", "age"),
        mu = mean(n_birth),
        svar = var(n_birth)
    ) |>
    mutate(
        r_birth = mu^2 / (svar - mu),
        # assume Poisson for underdispersed
        r_birth = if_else(mu / svar > .9, 9999, r_birth)
    )

# disp_birth |>
#     filter(r_birth < 9999) |>
#     ggplot() +
#     theme_minimal() +
#     geom_point(aes(x = age, y = r_birth, colour = sex)) +
#     facet_wrap(vars(race))

## project matrix kinship model ------------------------------------------------
projected_mom_dist <- project_matrix_parentship(
    us_fx_mom_int, 
    us_pop_qx |> filter(sex == "female")
)
projected_dad_dist <- project_matrix_parentship(
    us_fx_dad_est, 
    us_pop_qx |> filter(sex == "male")
)

## simulate matrix kinship model to quantify uncertainty -----------------------
set.seed(20240826)
N_SIM <- 1000

future::plan(future::multisession(workers = max(4, parallel::detectCores())))
simulated_mom_dist <- future.apply::future_replicate(
    N_SIM, simplify = FALSE, {
        # simulate fx
        fx_sim <- us_fx_mom_int |>
            left_join(us_pop_qx, by = c("year", "race", "age", "sex")) |>
            left_join(disp_birth, by = c("race", "age", "sex")) |>
            filter(!is.na(mu)) |>
            rowwise() |>
            mutate(
                fx_sim = MASS::rnegbin(1, fx * pop, r_birth) / pop,
                # fx_sim = rpois(1, fx * pop) / pop,
            )
        # simulate qx
        n_qx_sim <- us_pop_qx |>
            filter(sex == "female") |>
            left_join(disp_death, by = c("race", "age", "sex")) |>
            rowwise() |>
            mutate(
                mx = MASS::rnegbin(1, n_deaths, r_death) / pop,
                # mx = rpois(1, n_deaths) / pop,
                qx_sim = mx / (1 + mx / 2)
            )
        # project kinship
        project_matrix_parentship(fx_sim, n_qx_sim, sim = TRUE)
    }
) |>
    bind_rows() |>
    summarise(
        .by = c("x", "race"),
        sim_mean_m_lost = mean(m_lost),
        sim_se_m_lost = sd(m_lost),
        sim_mean_p_lost = mean(p_lost),
        sim_se_p_lost = sd(p_lost)
    ) |> 
    right_join(projected_mom_dist, by = c("x", "race"))

simulated_dad_dist <- future.apply::future_replicate(
    N_SIM, simplify = FALSE, {
        # simulate fx
        fx_sim <- us_fx_dad_est |>
            left_join(us_pop_qx, by = c("year", "race", "age", "sex")) |>
            left_join(disp_birth, by = c("race", "age", "sex")) |>
            filter(!is.na(mu)) |> 
            rowwise() |>
            mutate(
                fx_sim = MASS::rnegbin(
                # fx_sim = rpois(
                    1, 
                    runif( 1, lwr_fx, upr_fx) # sample male fx from 95% CI
                    * pop,
                    r_birth
                ) / pop
            )
        # simulate qx
        n_qx_sim <- us_pop_qx |>
            filter(sex == "male") |>
            left_join(disp_death, by = c("race", "age", "sex")) |>
            rowwise() |>
            mutate(
                mx = MASS::rnegbin(1, n_deaths, r_death) / pop,
                # mx = rpois(1, n_deaths) / pop,
                qx_sim = mx / (1 + mx / 2)
            )
        # project kinship
        project_matrix_parentship(fx_sim, n_qx_sim, sim = TRUE)
    }
)  |>
    bind_rows() |>
    summarise(
        .by = c("x", "race"),
        sim_mean_m_lost = mean(m_lost),
        sim_se_m_lost = sd(m_lost),
        sim_mean_p_lost = mean(p_lost),
        sim_se_p_lost = sd(p_lost)
    ) |> 
    right_join(projected_dad_dist, by = c("x", "race"))

## Save data -------------------------------------------------------------------
write_csv(simulated_mom_dist, here("data", "projected-mom.csv"))
write_csv(simulated_dad_dist, here("data", "projected-dad.csv"))

