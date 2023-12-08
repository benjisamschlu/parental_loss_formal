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
## w_i_{{ transition }} : weight for {{ transition }}
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
sum_weights <- function(df) {
    df |>
        summarise(
        .by = c("sex", "race", "age"),
        # weights
        w_i_lost_none_to_lost_mom = sum(
            .data$i_lost_none_to_lost_mom * .data$w, na.rm = TRUE),
        w_i_lost_dad_to_lost_both = sum(
            .data$i_lost_dad_to_lost_both * .data$w, na.rm = TRUE),
        w_i_lost_none_to_lost_dad = sum(
            .data$i_lost_none_to_lost_dad * .data$w, na.rm = TRUE),
        w_i_lost_mom_to_lost_both = sum(
            .data$i_lost_mom_to_lost_both * .data$w, na.rm = TRUE),
        w_i_lost_none_to_lost_both = sum(
            .data$i_lost_none_to_lost_both * .data$w, na.rm = TRUE),
        
        w_s_lost_none = sum(.data$s_lost_none * .data$w, na.rm = TRUE),
        w_s_lost_mom = sum(.data$s_lost_mom * .data$w, na.rm = TRUE),
        w_s_lost_dad = sum(.data$s_lost_dad * .data$w, na.rm = TRUE),
        w_s_lost_both = sum(.data$s_lost_both * .data$w, na.rm = TRUE),
        w_s_lost_none0 = sum(.data$s_lost_none0 * .data$w, na.rm = TRUE),
        w_s_lost_mom0 = sum(.data$s_lost_mom0 * .data$w, na.rm = TRUE),
        w_s_lost_dad0 = sum(.data$s_lost_dad0 * .data$w, na.rm = TRUE),
        w_s_total = sum(.data$w, na.rm = TRUE),
        
        # counts
        n_i_lost_none_to_lost_mom = sum(
            .data$i_lost_none_to_lost_mom, na.rm = TRUE),
        n_i_lost_dad_to_lost_both = sum(
            .data$i_lost_dad_to_lost_both, na.rm = TRUE),
        n_i_lost_none_to_lost_dad = sum(
            .data$i_lost_none_to_lost_dad, na.rm = TRUE),
        n_i_lost_mom_to_lost_both = sum(
            .data$i_lost_mom_to_lost_both, na.rm = TRUE),
        n_i_lost_none_to_lost_both = sum(
            .data$i_lost_none_to_lost_both, na.rm = TRUE),
        
        n_s_lost_none = sum(.data$s_lost_none, na.rm = TRUE),
        n_s_lost_mom = sum(.data$s_lost_mom, na.rm = TRUE),
        n_s_lost_dad = sum(.data$s_lost_dad, na.rm = TRUE),
        n_s_lost_both = sum(.data$s_lost_both, na.rm = TRUE),
        n_s_lost_none0 = sum(.data$s_lost_none0, na.rm = TRUE),
        n_s_lost_mom0 = sum(.data$s_lost_mom0, na.rm = TRUE),
        n_s_lost_dad0 = sum(.data$s_lost_dad0, na.rm = TRUE)
    ) |>
        arrange(.data$age)
}

compute_props_and_rates <- function(df, by) {
    df |>
        summarise(
            .by = by, 
            # states (across the age group)
            w_s_lost_none = sum(.data$w_s_lost_none),
            w_s_lost_mom = sum(.data$w_s_lost_mom),
            w_s_lost_dad = sum(.data$w_s_lost_dad),
            w_s_lost_both = sum(.data$w_s_lost_both),
            w_s_total = sum(.data$w_s_total),
            w_s_lost_none0 = sum(.data$w_s_lost_none0),
            w_s_lost_mom0 = sum(.data$w_s_lost_mom0),
            w_s_lost_dad0 = sum(.data$w_s_lost_dad0),
            # incidence
            w_i_lost_none_to_lost_mom = sum(.data$w_i_lost_none_to_lost_mom),
            w_i_lost_dad_to_lost_both = sum(.data$w_i_lost_dad_to_lost_both),
            w_i_lost_none_to_lost_dad = sum(.data$w_i_lost_none_to_lost_dad),
            w_i_lost_mom_to_lost_both = sum(.data$w_i_lost_mom_to_lost_both),
            w_i_lost_none_to_lost_both = sum(.data$w_i_lost_none_to_lost_both)
        ) |>
        mutate(
            # proportions
            pr_lost_none = .data$w_s_lost_none / .data$w_s_total,
            pr_lost_mom = .data$w_s_lost_mom / .data$w_s_total,
            pr_lost_dad = .data$w_s_lost_dad / .data$w_s_total,
            pr_lost_both = .data$w_s_lost_both / .data$w_s_total,
            # transfer rates
            mx_lost_none_to_lost_mom = .data$w_i_lost_none_to_lost_mom / .data$w_s_lost_none0,
            mx_lost_none_to_lost_dad = .data$w_i_lost_none_to_lost_dad / .data$w_s_lost_none0,
            mx_lost_none_to_lost_both = .data$w_i_lost_none_to_lost_both / .data$w_s_lost_none0,
            mx_lost_dad_to_lost_both = .data$w_i_lost_dad_to_lost_both / .data$w_s_lost_dad0,
            mx_lost_mom_to_lost_both = .data$w_i_lost_mom_to_lost_both / .data$w_s_lost_mom0
        ) |>
        select(by, starts_with("pr_"), starts_with("mx_"))
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
age_cuts <- c(0, seq(5, 60, by = 5), Inf) # left closed breaks
# age_cuts <- c(0, seq(70), Inf) # left closed breaks
sipp2021 <- read_csv(here("data", "sipp_snap_2021.csv")) |>
    sum_weights() |>
    mutate(x = cut(age, breaks = age_cuts, right = FALSE))
uslt2020 <- read_csv(here("data", "uslt_2020.csv")) |>
    mutate(x = cut(x, breaks = age_cuts, right = FALSE))

## Compute SIPP ----------------------------------------------------------------
## check cell sizes
df_sipp_n <- bind_rows(
    sipp2021 |> 
        summarise(across(starts_with("n_"), sum), 
                  .by = c("x")) |>
        mutate(sex = "all", race = "all"),
    sipp2021 |> 
        summarise(across(starts_with("n_"), sum), 
                  .by = c("sex", "x")) |>
        mutate(race = "all"),
    sipp2021 |> 
        filter(race %in% races) |>
        summarise(across(starts_with("n_"), sum), 
                  .by = c("race", "x")) |>
        mutate(sex = "all"),
    sipp2021 |> 
        filter(race %in% races) |>
        summarise(across(starts_with("n_"), sum), 
                  .by = c("sex", "race", "x"))
)
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
    compute_props_and_rates(c("sex", "x")) |>
    mutate(race = "all")
df_rates <- sipp2021 |>
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
    # assume proportions from SIPP
    mutate(
        Lx_lost_mom = Lx * pr_lost_mom,
        Lx_lost_dad = Lx * pr_lost_dad,
        Lx_lost_both = Lx * pr_lost_both,
        Lx_lost_none = Lx * pr_lost_none
    ) |>
    # compute dx_i
    # assume constant mortality rates
    mutate(
        dx_lost_mom_to_dead = dx * pr_lost_mom,
        dx_lost_dad_to_dead = dx * pr_lost_dad,
        dx_lost_both_to_dead = dx * pr_lost_both,
        dx_lost_none_to_dead = dx * pr_lost_none
    ) |>
    # compute dx_ij = mx_ij * Lx_i
    # assume observed mx_ij conditional on the subject being alive
    mutate(
        dx_lost_none_to_lost_mom = mx_lost_none_to_lost_mom * (
            Lx_lost_none - dx_lost_none_to_dead
        ),
        dx_lost_none_to_lost_dad = mx_lost_none_to_lost_dad * (
            Lx_lost_none - dx_lost_none_to_dead
        ),
        dx_lost_none_to_lost_both = mx_lost_none_to_lost_both * (
            Lx_lost_none - dx_lost_none_to_dead
        ),
        dx_lost_mom_to_lost_both = mx_lost_mom_to_lost_both * (
            Lx_lost_mom - dx_lost_mom_to_dead
        ),
        dx_lost_dad_to_lost_both = mx_lost_dad_to_lost_both * (
            Lx_lost_dad - dx_lost_dad_to_dead
        )
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
    # compute lx_i
    mutate(
        lx_lost_mom = replace_na(lag(cumsum(
            dx_lost_none_to_lost_mom 
            - replace_na(dx_lost_mom_to_lost_both, 0)
            - replace_na(dx_lost_mom_to_dead, 0)
        )), 0),
        lx_lost_dad = replace_na(lag(cumsum(
            dx_lost_none_to_lost_dad
            - replace_na(dx_lost_dad_to_lost_both, 0)
            - replace_na(dx_lost_dad_to_dead, 0)
        )), 0),
        lx_lost_both = replace_na(lag(cumsum(
            replace_na(dx_lost_mom_to_lost_both, 0)
            + replace_na(dx_lost_dad_to_lost_both, 0)
            + replace_na(dx_lost_none_to_lost_both, 0)
            - replace_na(dx_lost_both_to_dead, 0)
        )), 0),
        # lx_lost_none = lx - lx_lost_mom - lx_lost_dad - lx_lost_both,
        lx_lost_none = 100000 - replace_na(lag(cumsum(
            replace_na(dx_lost_none_to_dead, 0)
            + replace_na(dx_lost_none_to_lost_mom, 0)
            + replace_na(dx_lost_none_to_lost_dad, 0)
            + replace_na(dx_lost_none_to_lost_both, 0)
        )), 0)
    )
# View(df_multistate_lt)

### Data check
# test_that(
#     "No negatives check.", {
#         expect_false(
#             any(select(ungroup(df_multistate_lt), where(is.numeric)) < 0)
#         )
#     }
# )
test_that(
    "pr data check.", {
        pr_summed <- df_multistate_lt |>
            mutate(
                check = (
                    pr_lost_none 
                    + pr_lost_mom 
                    + pr_lost_dad
                    + pr_lost_both 
                )
            ) |> 
            pull(check)
        expect_equal(pr_summed, rep(1, length(pr_summed)), tolerance = 1e-4)
    }
)
test_that(
    "Lx data check.", {
        Lx <- df_multistate_lt |> pull(Lx)
        Lx_summed <- df_multistate_lt |>
            mutate(
            check = (
                Lx_lost_none 
                + Lx_lost_mom 
                + Lx_lost_dad
                + Lx_lost_both 
            )
        ) |> 
            pull(check)
        expect_equal(Lx, Lx_summed, tolerance = 1e-4)
    }
)
test_that(
    "dx data check.", {
        dx <- df_multistate_lt |> pull(dx)
        dx_summed <- df_multistate_lt |> mutate(
            check = (
                dx_lost_none_to_dead 
                + dx_lost_mom_to_dead 
                + dx_lost_dad_to_dead 
                + dx_lost_both_to_dead
            )
        ) |> 
            pull(check)
        expect_equal(dx, dx_summed, tolerance = 1e-4)
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
test_that(
    "lx data check.", {
        lx_orig <- uslt2020 |>
            filter(race %in% races) |>
            group_by(race, sex, x) |>
            summarise(lx = max(lx)) |>
            arrange(x, race, sex) |>
            pull(lx)
        lx_new <- df_multistate_lt |>
            select(race, sex, x, lx) |>
            arrange(x, race, sex) |>
            pull(lx)
        lx_summed <- df_multistate_lt |>
            select(race, sex, x, starts_with("lx_")) |>
            arrange(x, race, sex) |>
            mutate(
                lx = lx_lost_none + lx_lost_mom + lx_lost_dad + lx_lost_both
            ) |>
            pull(lx)
        expect_equal(lx_orig, lx_new, tolerance = 1e-4)
        expect_equal(lx_orig, lx_summed, tolerance = 1e-4)
    }
)
## Save data -------------------------------------------------------------------
write.csv(
    df_multistate_lt, here("data", "multistate-lt_snap.csv"), row.names = FALSE
)
write.csv(df_sipp_n, here("data", "sipp_n_snap.csv"), row.names = FALSE)
