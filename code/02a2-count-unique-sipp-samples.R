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
source("code/utils.R")

## Functions -------------------------------------------------------------------
count_unique_samples <- function(df, by) {
    df |>
        select(id, all_of(by)) |>
        distinct() |>
        summarise(
            .by = all_of({{ by }}),
            n = n()
        ) |>
        bind_rows(
            df |>
                select(id, all_of(by)) |>
                mutate(x = "all") |>
                distinct() |>
                summarise(
                    .by = all_of({{ by }}),
                    n = n()
                ) 
        )
}

count_unique_samples_by_state <- function(df, by) {
    df |>
        select(id, all_of(by), starts_with("s_")) |>
        distinct() |>
        summarise(
            .by = all_of({{ by }}),
            n_s_lost_none = sum(.data$s_lost_none),
            n_s_lost_mom = sum(.data$s_lost_mom),
            n_s_lost_dad = sum(.data$s_lost_dad),
            n_s_lost_both = sum(.data$s_lost_both)
        ) |>
        bind_rows(
            df |>
                select(id, all_of(by), starts_with("s_")) |>
                mutate(x = "all") |>
                distinct() |>
                summarise(
                    .by = all_of({{ by }}),
                    n_s_lost_none = sum(.data$s_lost_none),
                    n_s_lost_mom = sum(.data$s_lost_mom),
                    n_s_lost_dad = sum(.data$s_lost_dad),
                    n_s_lost_both = sum(.data$s_lost_both)
                )
        )
}

count_unique_samples_by_incidence <- function(df, by) {
    df |>
        select(id, all_of(by), starts_with("i_")) |>
        distinct() |>
        summarise(
            .by = all_of({{ by }}),
            n_i_lost_none_to_lost_mom = sum(.data$i_lost_none_to_lost_mom),
            n_i_lost_none_to_lost_dad = sum(.data$i_lost_none_to_lost_dad),
            n_i_lost_none_to_lost_both = sum(.data$i_lost_none_to_lost_both),
            n_i_lost_mom_to_lost_both = sum(.data$i_lost_mom_to_lost_both),
            n_i_lost_dad_to_lost_both = sum(.data$i_lost_dad_to_lost_both),
        ) |>
        bind_rows(
            df |>
                select(id, all_of(by), starts_with("i_")) |>
                mutate(x = "all") |>
                distinct() |>
                summarise(
                    .by = all_of({{ by }}),
                    n_i_lost_none_to_lost_mom = sum(.data$i_lost_none_to_lost_mom),
                    n_i_lost_none_to_lost_dad = sum(.data$i_lost_none_to_lost_dad),
                    n_i_lost_none_to_lost_both = sum(.data$i_lost_none_to_lost_both),
                    n_i_lost_mom_to_lost_both = sum(.data$i_lost_mom_to_lost_both),
                    n_i_lost_dad_to_lost_both = sum(.data$i_lost_dad_to_lost_both),
                )
        )
}

## Load data -------------------------------------------------------------------
years <- 1991:2020 # take the snapshot of 2020
sipp2021 <- read_csv(here("data_private", "sipp_2021.csv")) |>
    filter(race %in% RACES) |>
    select(race, sex, birth_year, starts_with("x_")) |>
    mutate(id = row_number())

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

## Count unique samples---------------------------------------------------------
by_s_r <- c("sex", "race", "x")
by_s <- c("sex", "x")
by_r <- c("race", "x")
by_all <- c("x")

# age-specific samples
n_unique_by_s_r <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples(by = by_s_r)
)
n_unique_by_s <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples(by = by_s) |>
        mutate(race = "all")
)
n_unique_by_r <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples(by = by_r) |>
        mutate(sex = "all")
)
n_unique_all <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples(by = by_all) |>
        mutate(sex = "all", race = "all")
)

df_n_unique <- lapply(
    seq(length(PERIODS)),
    function(ind) {
        bind_rows(
            n_unique_by_s_r[[ind]],
            n_unique_by_s[[ind]],
            n_unique_by_r[[ind]],
            n_unique_all[[ind]]
        )
    }
)

# age-specific samples by state
n_unique_state_by_s_r <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples_by_state(by = by_s_r)
)
n_unique_state_by_s <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples_by_state(by = by_s) |>
        mutate(race = "all")
)
n_unique_state_by_r <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples_by_state(by = by_r) |>
        mutate(sex = "all")
)
n_unique_state_all <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples_by_state(by = by_all) |>
        mutate(sex = "all", race = "all")
)

df_n_state_unique <- lapply(
    seq(length(PERIODS)),
    function(ind) {
        bind_rows(
            n_unique_state_by_s_r[[ind]],
            n_unique_state_by_s[[ind]],
            n_unique_state_by_r[[ind]],
            n_unique_state_all[[ind]]
        )
    }
)

# age-specific samples by incidence
n_unique_incidence_by_s_r <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples_by_incidence(by = by_s_r)
)
n_unique_incidence_by_s <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples_by_incidence(by = by_s) |>
        mutate(race = "all")
)
n_unique_incidence_by_r <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples_by_incidence(by = by_r) |>
        mutate(sex = "all")
)
n_unique_incidence_all <- lapply(
    PERIODS,
    function(prd) 
        sipp |>
        filter(max(years) - year < prd) |>
        count_unique_samples_by_incidence(by = by_all) |>
        mutate(sex = "all", race = "all")
)

df_n_incidence_unique <- lapply(
    seq(length(PERIODS)),
    function(ind) {
        bind_rows(
            n_unique_incidence_by_s_r[[ind]],
            n_unique_incidence_by_s[[ind]],
            n_unique_incidence_by_r[[ind]],
            n_unique_incidence_all[[ind]]
        )
    }
)

## Save data -------------------------------------------------------------------
res <- lapply(
    seq(length(PERIODS)),
    function(ind) {
        write_csv(
            df_n_unique[[ind]], 
            here("data", paste0("sipp_n_unique_", PERIODS[[ind]], ".csv"))
        )
        write_csv(
            df_n_state_unique[[ind]], 
            here("data", paste0("sipp_n_unique_state_", PERIODS[[ind]], ".csv"))
        )
        write_csv(
            df_n_incidence_unique[[ind]], 
            here("data", paste0("sipp_n_unique_incidence_", PERIODS[[ind]], ".csv"))
        )
        invisible(NULL)
    }
)

