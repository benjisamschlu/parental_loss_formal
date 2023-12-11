##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon
##  Date: December 2023
##------------------------------------------------------------------------------
##
##  Notes on data
## --------------
## Multistate life table 
## 
## race :                    all;
##                           hispanic;
##                           non-hispanic white;
##                           non-hispanic black;
##                           non-hispanic asian;
## x :                       age group
## lx:  number of surviving to age x
## dx:  number of dying in age group x
## Lx:  person-years lived between ages x and x + 1
## ex:  expectation of live at age x
## 
##  Checks ---------------------------------------------------------------------

rm(list = ls())

## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("tidyverse", "here", "utils", "testthat",
              "gam", "rstan")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Functions -------------------------------------------------------------------

## Load data -------------------------------------------------------------------
us_mlt <- read_csv(here("data", "multistate-lt_snap_rep.csv")) |>
    mutate(
        quantity = str_remove(name, "_.+"),
        state = str_remove(name, "^[a-z]+_"),
        value = ifelse(value == 0, NA, value)
    ) |>
    select(race, x, quantity, state, value)

se_us_mlt <- read_csv(here("data", "se-multistate-lt_snap_rep.csv")) |>
    mutate(
        name = str_remove(name, "^se_"),
        quantity = str_remove(name, "_.+"),
        state = str_remove(name, "^[a-z]+_"),
        value = ifelse(value == 0, NA, value)
    ) |>
    select(race, x, quantity, state, value)
df_pr <- us_mlt |>
    filter(quantity == "pr") |>
    left_join(
        se_us_mlt, 
        by = c("race", "x", "quantity", "state"),
        suffix = c("", "_se")
    ) |>
    rename(se = "value_se") |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+")))
df_mx <- us_mlt |>
    filter(quantity == "mx")  |>
    left_join(
        se_us_mlt, 
        by = c("race", "x", "quantity", "state"),
        suffix = c("", "_se")
    ) |>
    rename(se = "value_se") |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+")))

## Fit -------------------------------------------------------------------------
ggplot(df_mx, aes(x = x, y = log(value), colour = race)) +
    theme_minimal() +
    geom_point() +
    theme(legend.position = "none") +
    facet_wrap(vars(state))


fitglm <- glm(value ~ x * state + race, df_mx, family = gaussian(link = "log"))
plot(fitglm)
summary(fitglm)

predglm <- predict(
    fitglm, newdata = df_mx |> select(x, race, state), type = "link")
df_mx |>
    mutate(pred = predglm) |>
    ggplot(aes(x = x, y = pred, colour = race)) +
    theme_minimal() +
    geom_point(aes( y = log(value))) +
    geom_line() +
    theme(legend.position = "none") +
    facet_wrap(vars(state))


fitgam <- gam(value ~ s(x, df = 3) * state + race, df_mx, family = gaussian(link = "log"))
# plot(fitgam)
summary(fitgam)

predgam <- predict(
    fitgam, newdata = df_mx |> select(x, race, state), type = "link")
df_mx |>
    mutate(pred = predgam) |>
    ggplot(aes(x = x, y = pred, colour = race)) +
    theme_minimal() +
    geom_point(aes( y = log(value))) +
    geom_line() +
    theme(legend.position = "none") +
    facet_wrap(vars(state))

