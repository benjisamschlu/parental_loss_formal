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

## Functions -------------------------------------------------------------------
count_samples <- function(df, by) {
    df |>
        summarise(
            .by = {{ by }},
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
           "hispanic", "non-hispanic asian", "non-hispanic other")
years <- 2020 # only take the snapshot of 2020 for now
age_cuts <- c(0, seq(5, 60, by = 5), Inf) # left closed age breaks
sipp2021 <- read_csv(here("data_private", "sipp_2021-by-year.csv"))

tmp <- lapply(years, function(y) {
    d <- sipp2021 |>
        filter(birth_year <= y) |>
        mutate(
            year = y,
            age = y - birth_year,
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
        ) |>
        select(-"birth_year")
    test_that(
        "All belong to one state.",
        {
            s_ <- d |>
                select(starts_with("s_")) |>
                rowSums()
            expect_true(all(s_ == 1))
        }
    )
    test_that(
        "Max one incidence per year.",
        {
            i_ <- d |>
                select(starts_with("i_")) |>
                rowSums()
            expect_true(max(i_) <= 1)
        }
    )
    return(d)
    })

sipp <- bind_rows(tmp)|>
    select(-"year") |> # only 2020 for now
    mutate(x = cut(age, breaks = age_cuts, right = FALSE))
uslt2020 <- read_csv(here("data", "uslt_2020.csv")) |>
    filter(sex == "all") |>
    mutate(x = cut(x, breaks = age_cuts, right = FALSE))

## Estimate proportions and rates from SIPP ------------------------------------
svy <- sipp |>
    svrepdesign(
        variables = NULL,
        repweights = "repwgt[1-240]+",
        weights = ~ repwgt0,
        type = "Fay",
        rho = 0.5
    )

by_s_r <- c("sex", "race", "x")
by_s <- c("sex", "x")
by_r <- c("race", "x")
by_all <- c("x")

pr_by_s_r <- compute_props(svy, by_s_r)
pr_by_r <- compute_props(svy, by_r) |>
    mutate(sex = "all")
pr_by_s <- compute_props(svy, by_s) |>
    mutate(race = "all")
pr_all <- compute_props(svy, by_all) |>
    mutate(sex = "all", race = "all")

df_pr <- bind_rows(pr_all, pr_by_s, pr_by_r, pr_by_s_r)

mx_by_s_r <- compute_rates(svy, by_s_r)
mx_by_r <- compute_rates(svy, by_r) |>
    mutate(sex = "all")
mx_by_s <- compute_rates(svy, by_s) |>
    mutate(race = "all")
mx_all <- compute_rates(svy, by_all) |>
    mutate(sex = "all", race = "all")

df_mx <- bind_rows(mx_all, mx_by_s, mx_by_r, mx_by_s_r)

## Check cell sizes ------------------------------------------------------------
n_by_s_r <- count_samples(sipp, by = by_s_r)
n_by_r <- count_samples(sipp, by = by_r) |>
    mutate(sex = "all")
n_by_s <- count_samples(sipp, by = by_s) |>
    mutate(race = "all")
n_all <- count_samples(sipp, by = by_all) |>
    mutate(sex = "all", race = "all")

df_sipp_n <- bind_rows(n_all, n_by_s, n_by_r, n_by_s_r)


## Compute US life table -------------------------------------------------------
df_lt <- uslt2020 |>
    filter(race %in% races) |>
    aggregate_lt(by_s_r) |>
    select(all_of(by_s_r), everything())

## Join life table and rates ---------------------------------------------------
multistate_lt <- left_join(df_lt, df_pr, by = by_s_r) |>
    left_join(df_mx, by = by_s_r) |>
    select(-starts_with("se_")) |>
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
        dx_lost_none_to_lost_mom = (Lx - dx) * pr_lost_none * mx_lost_none_to_lost_mom,
        dx_lost_none_to_lost_dad = (Lx - dx) * pr_lost_none * mx_lost_none_to_lost_dad,
        dx_lost_none_to_lost_both = (Lx - dx) * pr_lost_none * mx_lost_none_to_lost_both,
        dx_lost_mom_to_lost_both = (Lx- dx) * pr_lost_mom * mx_lost_mom_to_lost_both,
        dx_lost_dad_to_lost_both = (Lx - dx) * pr_lost_dad * mx_lost_dad_to_lost_both
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
    ) |>
    pivot_longer(-by_s_r)

# transform standard errors
se_multistate_lt <- left_join(df_lt, df_pr, by = by_s_r) |>
    left_join(df_mx, by = by_s_r) |>
    mutate(
        se_Lx_lost_mom = sqrt(Lx^2 * se_pr_lost_mom^2),
        se_Lx_lost_dad = sqrt(Lx^2 * se_pr_lost_dad^2),
        se_Lx_lost_both = sqrt(Lx^2 * se_pr_lost_both^2),
        se_Lx_lost_none = sqrt(Lx^2 * se_pr_lost_none^2)
    ) |>
    mutate(
        se_dx_lost_mom_to_dead = sqrt(dx^2 * se_pr_lost_mom^2),
        se_dx_lost_dad_to_dead = sqrt(dx^2 * se_pr_lost_dad^2),
        se_dx_lost_both_to_dead = sqrt(dx^2 * se_pr_lost_both^2),
        se_dx_lost_none_to_dead = sqrt(dx^2 * se_pr_lost_none^2)
    ) |>
    select(all_of(by_s_r), starts_with("se_")) |>
    pivot_longer(-by_s_r)

### Data check
test_that(
    "pr data check.", {
        pr_summed <- multistate_lt |>
            filter(str_detect(name, "^pr_")) |>
            ungroup() |>
            summarise(
                .by = c("x", "race"),
                check = sum(value)
            ) |>
            pull(check)
        expect_equal(pr_summed, rep(1, length(pr_summed)), tolerance = 1e-4)
    }
)
test_that(
    "Lx data check.", {
        Lx <- multistate_lt |> filter(name == "Lx") |> pull(value)
        Lx_summed <- multistate_lt |>
            filter(str_detect(name, "^Lx_")) |>
            ungroup() |>
            summarise(
                .by = c("x", "race"),
                check = sum(value)
            ) |>
            pull(check)
        expect_equal(Lx, Lx_summed, tolerance = 1e-4)
    }
)
test_that(
    "dx data check.", {
        dx <- multistate_lt |> filter(name == "dx") |> pull(value)
        dx_summed <- multistate_lt |> 
            filter(str_detect(name, "^dx_.*_to_dead")) |>
            ungroup() |>
            summarise(
                .by = c("x", "race"),
                check = sum(value)
            ) |>
            pull(check)
        expect_equal(dx, dx_summed, tolerance = 1e-4)
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
        lx_new <- multistate_lt |>
            ungroup() |>
            arrange(x, race) |>
            filter(name == "lx") |>
            pull(value)
        lx_summed <- multistate_lt |>
            ungroup() |>
            arrange(x, race) |>
            filter(str_detect(name, "^lx_")) |>
            summarise(
                .by = c("x", "race"),
                check = sum(value)
            ) |>
            pull(check)
        expect_equal(lx_orig, lx_new, tolerance = 1e-4)
        expect_equal(lx_orig, lx_summed, tolerance = 1e-4)
    }
)
test_that(
    "ex data check.", {
        ex_orig <- uslt2020 |>
            filter(race %in% races) |>
            group_by(race, x) |>
            summarise(ex = max(ex)) |>
            arrange(x, race) |>
            pull(ex)
        ex_new <- multistate_lt |>
            ungroup() |>
            arrange(x, race) |>
            filter(name == "ex") |>
            pull(value)
        ex_summed <- multistate_lt |>
            ungroup() |>
            arrange(x, race) |>
            filter(str_detect(name, "^ex_")) |>
            summarise(
                .by = c("x", "race"),
                check = sum(value)
            ) |>
            pull(check)
        expect_equal(ex_orig, ex_new, tolerance = 1e-4)
        expect_equal(ex_orig, ex_summed, tolerance = 1e-4)
    }
)
test_that(
    "est and se match", {
        nms <- str_remove(unique(se_multistate_lt$name), "se_")
        est_nrow <- multistate_lt |>
            filter(name %in% nms) |>
            nrow()
        se_nrow <- se_multistate_lt |>
            nrow()
        expect_equal(est_nrow, se_nrow)
    }
)

## Save data -------------------------------------------------------------------
write.csv(
    multistate_lt, here("data", "multistate-lt.csv"), row.names = FALSE
)
write.csv(
    se_multistate_lt, here("data", "se-multistate-lt.csv"), 
    row.names = FALSE
)
write.csv(df_sipp_n, here("data", "sipp_n.csv"), row.names = FALSE)
