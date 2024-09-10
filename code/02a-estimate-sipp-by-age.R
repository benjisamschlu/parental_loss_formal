##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon
##  Date: July 2024
##------------------------------------------------------------------------------
##
##  Notes on data
## --------------
## SIPP cleaned
## 
## age :             age
## race :            hispanic;
##                   non-hispanic white;
##                   non-hispanic black;
##                   non-hispanic asian;
##                   non-hipanic other
## i_{{ transition }} : indicator for {{ transition }}
## s_{{ state }} : indicator for {{ state }}
## repwgt0 : final person weight
## repwgt[1-240]+ : replication weights 1 to 240
##
##  Checks ---------------------------------------------------------------------

rm(list = ls())

## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("tidyverse", "here", "utils", "survey", "testthat")
for (p in packages) {
    if (!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
    }
source("code/utils.R")

## Functions -------------------------------------------------------------------
count_samples <- function(df, by) {
    df |>
        summarise(
            .by = all_of({{ by }}),
            n_i_lost_none_to_lost_mom = sum(.data$i_lost_none_to_lost_mom),
            n_i_lost_none_to_lost_dad = sum(.data$i_lost_none_to_lost_dad),
            n_i_lost_none_to_lost_both = sum(.data$i_lost_none_to_lost_both),
            n_i_lost_mom_to_lost_both = sum(.data$i_lost_mom_to_lost_both),
            n_i_lost_dad_to_lost_both = sum(.data$i_lost_dad_to_lost_both),
            
            n_s_lost_none = sum(.data$s_lost_none),
            n_s_lost_mom = sum(.data$s_lost_mom),
            n_s_lost_dad = sum(.data$s_lost_dad),
            n_s_lost_both = sum(.data$s_lost_both)
        ) |>
        arrange(.data$x)
    }
#' Variance estimation using replicate weights using Fay's modified balanced
#' repeated replication method with k = 0.5 as described in 2022 SIPP User Guide
compute_props <- function(svy, by) {
    compute_props_s <- function(svy, var, by) {
        var_fml <- formula(paste("~", paste(var, collapse = " + ")))
        by_fml <- formula(paste("~", paste(by, collapse = " + ")))
        format_svy_df <- function(df, by) {
            df |>
                rename_with(~ gsub("s_", "pr_", .x), starts_with("s_")) |>
                pivot_longer(starts_with("pr_")) |>
                pivot_wider(id_cols = all_of(by),
                            values_from = c("value", "se")) |>
                rename_with(~ gsub("value_", "", .x), starts_with("value_"))
        }
        svyby(
            formula = var_fml,
            by = by_fml,
            design = svy,
            FUN = svymean
        ) |>
            format_svy_df(by)
    }
    pr_lost_none <- compute_props_s(svy, "s_lost_none", by)
    pr_lost_mom <- compute_props_s(svy, "s_lost_mom", by)
    pr_lost_dad <- compute_props_s(svy, "s_lost_dad", by)
    pr_lost_both <- compute_props_s(svy, "s_lost_both", by)
    
    pr_lost_none |>
        full_join(pr_lost_mom, by = by) |>
        full_join(pr_lost_dad, by = by) |>
        full_join(pr_lost_both, by = by)
}

compute_rates <- function(svy, by) {
    compute_rates_s_i <- function(svy, var, denom, by) {
        var_fml <- formula(paste("~", paste(var, collapse = " + ")))
        by_fml <- formula(paste("~", paste(c(by, denom), collapse = " + ")))
        format_svy_df <- function(df, by) {
            df |>
                rename_with(~ gsub("i_", "mx_", .x), starts_with("i_")) |>
                pivot_longer(starts_with("mx_")) |>
                pivot_wider(id_cols = all_of(by),
                            values_from = c("value", "se")) |>
                rename_with(~ gsub("value_", "", .x), starts_with("value_"))
        }
        svyby(
            formula = var_fml,
            by = by_fml,
            design = svy,
            FUN = svymean
        ) |>
            filter(.data[[denom]] == 1) |>
            select(-all_of(denom)) |>
            format_svy_df(by)
    }
    mx_lost_none_to_lost_mom <- compute_rates_s_i(
        svy, "i_lost_none_to_lost_mom", "s_lost_none", by)
    mx_lost_none_to_lost_dad <- compute_rates_s_i(
        svy, "i_lost_none_to_lost_dad", "s_lost_none", by)
    mx_lost_none_to_lost_both <- compute_rates_s_i(
        svy, "i_lost_none_to_lost_both", "s_lost_none", by)
    mx_lost_dad_to_lost_both <- compute_rates_s_i(
        svy, "i_lost_dad_to_lost_both", "s_lost_dad", by)
    mx_lost_mom_to_lost_both <- compute_rates_s_i(
        svy, "i_lost_mom_to_lost_both", "s_lost_mom", by)
    
    mx_lost_none_to_lost_mom |> 
        full_join(mx_lost_none_to_lost_dad, by = by) |>
        full_join(mx_lost_none_to_lost_both, by = by) |>
        full_join(mx_lost_dad_to_lost_both, by = by) |>
        full_join(mx_lost_mom_to_lost_both, by = by)
}

## Load data -------------------------------------------------------------------
years <- 1991:2020 # take the snapshot of 2020
sipp2021 <- read_csv(here("data_private", "sipp_2021.csv")) |>
    filter(race %in% RACES)

sipp <- lapply(
    years,
    function(yr) sipp2021 |>
        mutate(
            year = yr,
            age = yr - birth_year
        ) |>
        filter(age >= 0) |>
        mutate(
            x = cut(age, breaks = AGES, right = FALSE, ordered_result = TRUE),
                i_lost_none_to_lost_mom = (age == x_lost_none_to_lost_mom) * 1,
            i_lost_none_to_lost_dad = (age == x_lost_none_to_lost_dad) * 1,
            i_lost_none_to_lost_both = (age == x_lost_none_to_lost_both) * 1,
            i_lost_mom_to_lost_both = (age == x_lost_mom_to_lost_both) * 1,
            i_lost_dad_to_lost_both = (age == x_lost_dad_to_lost_both) * 1,
            s_lost_none = (age <= x_lost_none_to_lost_mom 
                           & age <= x_lost_none_to_lost_dad 
                           & age <= x_lost_none_to_lost_both) * 1,
            s_lost_mom = (age > x_lost_none_to_lost_mom
                          & age <= x_lost_mom_to_lost_both) * 1,
            s_lost_dad = (age > x_lost_none_to_lost_dad
                          & age <= x_lost_dad_to_lost_both) * 1,
            s_lost_both = (age > x_lost_none_to_lost_both
                           | age > x_lost_mom_to_lost_both
                           | age > x_lost_dad_to_lost_both) * 1
        )
    ) |>
    bind_rows()

test_that(
    "All belong to one state.",
    {
        s_ <- sipp |>
            select(starts_with("s_")) |>
            rowSums()
        expect_true(all(s_ == 1))
    }
)
test_that(
    "Max one incidence per year.",
    {
        i_ <- sipp |>
            select(starts_with("i_")) |>
            rowSums()
        expect_true(max(i_) <= 1)
    }
)

## Estimate proportions and rates from SIPP ------------------------------------
svy <- lapply(
    PERIODS,
    function(prd)
        sipp |>
        filter(max(years) - year < prd) |>
        svrepdesign(
            variables = NULL,
            repweights = "repwgt[1-240]+",
            weights = ~ repwgt0,
            type = "Fay",
            rho = 0.5
        )
)

by_s_r <- c("sex", "race", "x")
by_s <- c("sex", "x")
by_r <- c("race", "x")
by_all <- c("x")

pr_by_s_r <- lapply(svy, function(s) compute_props(s, by_s_r))

pr_by_r <- lapply(
    svy, 
    function(s) 
        compute_props(s, by_r) |>
        mutate(sex = "all")
)

pr_by_s <- lapply(
    svy, 
    function(s) 
        compute_props(s, by_s) |>
        mutate(race = "all")
)

pr_all <- lapply(
    svy, 
    function(s) 
        compute_props(s, by_all) |>
        mutate(sex = "all", race = "all")
)


df_pr <- lapply(
    seq(length(PERIODS)),
    function(ind) {
        bind_rows(
            pr_by_s_r[[ind]],
            pr_by_r[[ind]],
            pr_by_s[[ind]],
            pr_all[[ind]]
        )
    }
)


mx_by_s_r <- lapply(svy, function(s) compute_rates(s, by_s_r))

mx_by_r <- lapply(
    svy, 
    function(s) 
        compute_rates(s, by_r) |>
        mutate(sex = "all")
)

mx_by_s <- lapply(
    svy, 
    function(s) 
        compute_rates(s, by_s) |>
        mutate(race = "all")
)

mx_all <- lapply(
    svy, 
    function(s) 
        compute_rates(s, by_all) |>
        mutate(sex = "all", race = "all")
)


df_mx <- lapply(
    seq(length(PERIODS)),
    function(ind) {
        bind_rows(
            mx_by_s_r[[ind]],
            mx_by_r[[ind]],
            mx_by_s[[ind]],
            mx_all[[ind]]
        )
    }
)

## Check cell sizes ------------------------------------------------------------
n_by_s_r <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_samples(by = by_s_r)
)

n_by_r <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_samples(by = by_r) |>
        mutate(sex = "all")
)

n_by_s <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_samples(by = by_s) |>
        mutate(race = "all")
)

n_all <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_samples(by = by_all) |>
        mutate(sex = "all", race = "all")
)

df_sipp_n <- lapply(
    seq(length(PERIODS)),
    function(ind) {
        bind_rows(
            n_by_s_r[[ind]],
            n_by_r[[ind]],
            n_by_s[[ind]],
            n_all[[ind]]
        )
    }
)

### Data check
test_that(
    "pr data check.", {
        lapply(
            df_pr,
            function(df) {
                pr_summed <- df |>
                    summarise(
                        .by = c("x", "sex", "race"),
                        check = pr_lost_none + pr_lost_mom + pr_lost_dad + pr_lost_both
                    ) |>
                    pull(check)
                expect_equal(pr_summed, rep(1, length(pr_summed)), tolerance = 1e-4)
            }
        )
    }
)

## Save data -------------------------------------------------------------------
res <- lapply(
    seq(length(PERIODS)),
    function(ind) {
        write_csv(
            df_pr[[ind]], 
            here("data", paste0("sipp_pr_", PERIODS[[ind]], ".csv"))
        )
        write_csv(
            df_mx[[ind]], 
            here("data", paste0("sipp_mx_", PERIODS[[ind]], ".csv"))
        )
        write_csv(
            df_sipp_n[[ind]], 
            here("data", paste0("sipp_n_", PERIODS[[ind]], ".csv"))
        )
        invisible(NULL)
    }
)
