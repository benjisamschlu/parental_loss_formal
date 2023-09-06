##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon
##  Date: August 2023
##------------------------------------------------------------------------------
##
##  Notes on data
## --------------
## SIPP cleaned
## 
## age :             age
## sex :             male; female
## race :            hispanic;
##                   non-hispanic white;
##                   non-hispanic black;
##                   non-hispanic asian;
##                   non-hipanic other
## w_s_{{ state }} : weight for {{ state }}
## n_s_{{ state }} : count for {{ state }}
##
##  Checks ---------------------------------------------------------------------

rm(list = ls())

## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("tidyverse", "here", "utils", "testthat")
for (p in packages) {
    if (!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Functions -------------------------------------------------------------------
compute_props_and_rates <- function(df, by) {
    df |>
        summarise(
            .by = by, 
            # states (at the start of the age)
            w_s_lost_none = sum(.data$w_s_lost_none),
            w_s_lost_mom = sum(.data$w_s_lost_mom),
            w_s_lost_dad = sum(.data$w_s_lost_dad),
            w_s_lost_both = sum(.data$w_s_lost_both),
            w_s_total = sum(.data$w_s_total),
            # incidence
            w_i_lost_mom_first = sum(.data$w_i_lost_mom_first),
            w_i_lost_mom_last = sum(.data$w_i_lost_mom_last),
            w_i_lost_dad_first = sum(.data$w_i_lost_dad_first),
            w_i_lost_dad_last = sum(.data$w_i_lost_dad_last),
            w_i_lost_both = sum(.data$w_i_lost_both)
        ) |>
        mutate(
            # proportions
            p_lost_none = .data$w_s_lost_none / .data$w_s_total,
            p_lost_mom = .data$w_s_lost_mom / .data$w_s_total,
            p_lost_dad = .data$w_s_lost_dad / .data$w_s_total,
            p_lost_both = .data$w_s_lost_both / .data$w_s_total,
            # rates
            dx_lost_mom_first = .data$w_i_lost_mom_first / .data$w_s_lost_none,
            dx_lost_dad_first = .data$w_i_lost_dad_first / .data$w_s_lost_none,
            dx_lost_both = .data$w_i_lost_both / .data$w_s_lost_none,
            dx_lost_mom_last = .data$w_i_lost_mom_last / .data$w_s_lost_dad,
            dx_lost_dad_last = .data$w_i_lost_dad_last / .data$w_s_lost_mom
        ) |>
        select(by, starts_with("p_"), starts_with("dx_"))
}
aggregate_lt <- function(df, by) {
    df |>
        summarise(
            .by = by,
            lx = max(lx), dx = sum(dx), Lx = sum(Lx)
        )
}
compute_ex <- function(Lx, lx) {
    rev(cumsum(rev(Lx))) / lx
}
## Load data -------------------------------------------------------------------
races <- c("all", "non-hispanic white", "non-hispanic black", 
           "hispanic", "non-hispanic asian")
age_cuts <- c(0, 1, seq(5, 70, by = 5), Inf) # left closed breaks
sipp2021 <- read_csv(here("data", "sipp_2021.csv")) |>
    mutate(x = cut(age, breaks = age_cuts, right = FALSE))
uslt2020 <- read_csv(here("data", "uslt_2020.csv")) |>
    mutate(x = cut(x, breaks = age_cuts, right = FALSE))

## Compute SIPP ----------------------------------------------------------------
## check cell sizes
df_sipp_n <- sipp2021 |>
    summarise(across(starts_with("n_"), sum),
              .by = c("sex", "race", "x"))
# View(df_sipp_n)
# compute rates
df_rates_race_sex <- sipp2021 |>
    filter(race %in% races) |>
    compute_props_and_rates(c("sex", "race", "x"))
df_rates_race <- sipp2021 |>
    filter(race %in% races) |>
    compute_props_and_rates(c("race", "x")) |>
    mutate(sex = "all")
df_rates_sex <- sipp2021 |>
    filter(race %in% races) |>
    compute_props_and_rates(c("sex", "x")) |>
    mutate(race = "all")
df_rates <- sipp2021 |>
    filter(race %in% races) |>
    compute_props_and_rates("x") |>
    mutate(race = "all", sex = "all")|>
    bind_rows(df_rates_sex, df_rates_race, df_rates_race_sex) |>
    select(sex, race, x, everything())

## Compute US life table -------------------------------------------------------
df_lt <- uslt2020 |>
    filter(race %in% races) |>
    aggregate_lt(c("sex", "race", "x")) |>
    select(sex, race, x, everything())

## Join life table and rates ---------------------------------------------------
df_multistate_lt <- left_join(df_lt, df_rates, by = c("sex", "race", "x")) |>
    # compute Lx_i
    mutate(
        Lx_lost_mom = Lx * p_lost_mom,
        Lx_lost_dad = Lx * p_lost_dad,
        Lx_lost_both = Lx * p_lost_both,
        Lx_lost_none = Lx * p_lost_none
    ) |>
    # compute dx_i
    # assume constant mortality rates
    mutate(
        dx_lost_mom_to_dead = dx * p_lost_mom,
        dx_lost_dad_to_dead = dx * p_lost_dad,
        dx_lost_both_to_dead = dx * p_lost_both,
        dx_lost_none_to_dead = dx * p_lost_none
    ) |>
    # compute lx_i
    mutate(
        lx_lost_mom = lx * p_lost_mom,
        lx_lost_dad = lx * p_lost_dad,
        lx_lost_both = lx * p_lost_both,
        lx_lost_none = lx * p_lost_none
    ) |>
    group_by(sex, race) |>
    # compute ex_i - # years to live in state i above age x for anyone
    mutate(
        ex_lost_none = compute_ex(Lx_lost_none, lx),
        ex_lost_mom = compute_ex(Lx_lost_mom, lx),
        ex_lost_dad = compute_ex(Lx_lost_dad, lx),
        ex_lost_both = compute_ex(Lx_lost_both, lx),
        ex = compute_ex(Lx, lx)
    ) |>
    select(-starts_with("p_"))
# View(df_multistate_lt)

### Data check
test_that(
    "Lx data check.", {
        check <- df_multistate_lt |> mutate(
            check = 
                Lx - (
                    Lx_lost_none 
                    + Lx_lost_mom 
                    + Lx_lost_dad
                    + Lx_lost_both 
                )
        ) |> 
            pull(check)
        for (c in check) {
            expect_equal(c, 0, tolerance = 1e-4)
        }   
    }
)
test_that(
    "dx data check.", {
        check <- df_multistate_lt |> mutate(
            check = 
                dx - (
                    dx_lost_none_to_dead 
                    + dx_lost_mom_to_dead 
                    + dx_lost_dad_to_dead 
                    + dx_lost_both_to_dead
                )
        ) |> 
            pull(check)
        for (c in check) {
            expect_equal(c, 0, tolerance = 1e-4)
        }   
    }
)
test_that(
    "ex data check.", {
        ex_orig <- uslt2020 |>
            filter(race %in% races) |>
            group_by(race, sex, x) |>
            summarise(ex = max(ex)) |>
            arrange(x, race, sex) |>
            pull(ex)
        ex_new <- df_multistate_lt |>
            select(race, sex, x, ex) |>
            arrange(x, race, sex) |>
            pull(ex)
        ex_summed <- df_multistate_lt |>
            select(race, sex, x, starts_with("ex_")) |>
            arrange(x, race, sex) |>
            mutate(
                ex = ex_lost_none + ex_lost_mom + ex_lost_dad + ex_lost_both
            ) |>
            pull(ex)
        expect_equal(ex_orig, ex_new, tolerance = 1e-4)
        expect_equal(ex_orig, ex_summed, tolerance = 1e-4)
    }
)

## Save data -------------------------------------------------------------------
write.csv(
    df_multistate_lt, here("data", "multistate-lt.csv"), row.names = FALSE
)
write.csv(df_sipp_n, here("data", "sipp_n.csv"), row.names = FALSE)
