##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon
##  Date: August 2023
##------------------------------------------------------------------------------
##
##  Checks ---------------------------------------------------------------------

rm(list = ls())

## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("tidyverse", "here", "utils", "testthat")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Functions -------------------------------------------------------------------

## Load data -------------------------------------------------------------------
races <- c("all", "non-hispanic white", "non-hispanic black", 
           "hispanic", "non-hispanic asian")
sipp2021 <- read_csv(here("data", "sipp_2021.csv")) 
uslt2020 <- read_csv(here("data", "uslt_2020.csv"))
age_cuts <- c(0, 1, seq(5, 70, by = 5), 998) # left closed breaks

## Compute SIPP ----------------------------------------------------------------
## check cell sizes
df_sipp_n <- sipp2021 |>
    mutate(
        x = cut(loss_age, breaks = age_cuts, right = FALSE)
    ) |>
    filter(!is.na(x)) |>
    group_by(sex, race, parent, parent_status, x) |>
    summarise(n = sum(n, na.rm = TRUE), .groups = "keep") |>
    pivot_wider(names_from = c("parent", "parent_status"),
                values_from = "n")

# compute rates
df_age_cut <- sipp2021 |>
    mutate(
        x = cut(loss_age, breaks = age_cuts, right = FALSE)
    ) |>
    filter(!is.na(x)) 
df_rates_race_sex <- df_age_cut |>
    filter(race %in% races) |>
    group_by(sex, race, parent, parent_status, x) |>
    summarise(w = sum(w, na.rm = TRUE), .groups = "keep")
df_rates_race <- df_age_cut |>
    filter(race %in% races) |>
    group_by(race, parent, parent_status, x) |>
    summarise(w = sum(w, na.rm = TRUE), .groups = "keep") |>
    mutate(sex = "all")
df_rates_sex <- df_age_cut |>
    group_by(sex, parent, parent_status, x) |>
    summarise(w = sum(w, na.rm = TRUE), .groups = "keep") |>
    mutate(race = "all")
df_rates <- df_age_cut |>
    group_by(parent, parent_status, x) |>
    summarise(w = sum(w, na.rm = TRUE), .groups = "keep") |>
    mutate(race = "all", sex = "all")|>
    bind_rows(df_rates_sex, df_rates_race, df_rates_race_sex) |>
    pivot_wider(names_from = c("parent", "parent_status"),
                values_from = "w") |>
    mutate(
        # TODO: some cells have missing values
        #       set to 0 for now
        #       note rates based on small or missing cells?
        across(everything(), function(x) replace_na(x, 0)),
        rate_mom_loss = mom_dead / (mom_dead + mom_alive),
        rate_dad_loss = dad_dead / (dad_dead + dad_alive),
        rate_both_loss = both_both_dead / (
            both_both_dead + both_one_alive),
        rate_no_loss = both_both_alive / (
            both_both_dead + both_both_alive)
    ) |>
    select(
        sex, race, x,
        rate_mom_loss, rate_dad_loss, rate_both_loss, rate_no_loss
    )

## Compute US life table -------------------------------------------------------
df_lt <- uslt2020 |>
    mutate(
        x = cut(x, breaks = age_cuts, right = FALSE)
    ) |>
    filter(race %in% races) |>
    group_by(sex, race, x) |>
    summarise(lx = max(lx), dx = sum(dx), Lx = sum(Lx), .groups = "keep")

## Join life table and rates ---------------------------------------------------
df_multistate_lt <- left_join(df_lt, df_rates, by = c("sex", "race", "x")) |>
    mutate(
        Lx_lost_mom = Lx * rate_mom_loss,
        Lx_lost_dad = Lx * rate_dad_loss,
        Lx_lost_both = Lx * rate_both_loss,
        Lx_lost_neither = Lx * rate_no_loss
    ) 
View(df_multistate_lt)
## Save data -------------------------------------------------------------------
# res <- sapply(seq(length(years)), function(i) {
#     f <- paste0("uslt_", years[i], ".csv")
#     df <- df_uslt_by_year[[i]]
#     write.csv(df, here("data", f), row.names = FALSE)
# })
