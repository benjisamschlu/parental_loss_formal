##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon
##  Date: July 2024
##------------------------------------------------------------------------------
##

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
aggregate_lt <- function(df, by) {
    df |>
        summarise(
            .by = all_of(by),
            lx = max(lx), dx = sum(dx), Lx = sum(Lx)
        ) |>
        ungroup()
}
compute_ex <- function(Lx, lx) {
    rev(cumsum(rev(Lx))) / lx
}

## Load data -------------------------------------------------------------------
uslt2020 <- read_csv(here("data", "uslt_2020.csv")) |>
    filter(
        sex %in% SEXES,
        race %in% RACES
    ) |>
    mutate(x = cut(x, breaks = AGES, right = FALSE))

## Aggregate US life table -------------------------------------------------------
by_s_r <- c("race", "sex", "x")
df_lt <- uslt2020 |>
    aggregate_lt(by_s_r) |>
    select(all_of(by_s_r), everything())

## Save data -------------------------------------------------------------------
write.csv(
    df_lt, here("data", "uslt-age-group.csv"), row.names = FALSE
)
