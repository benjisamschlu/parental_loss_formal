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
            n_s_lost_both = sum(.data$s_lost_both, na.rm = TRUE)
        ) |>
        arrange(.data$x)
}
#' Variance estimation using replicate weights using Fay's modified balanced
#' repeated replication method with k = 0.5 as described in 2022 SIPP User Guide
compute_props <- function(svy, var, by) {
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
compute_rates <- function(svy, var, denom, by) {
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

aggregate_lt <- function(df, by) {
    df |>
        summarise(
            .by = by,
            lx = max(lx), dx = sum(dx), Lx = sum(Lx)
        )
}

# compute_var_prod <- function(e1, e2, se1, se2) {
#     (se1 * se2)^2 + (se1 * e2)^2 + (se2 * e1)^2
# }

compute_ex <- function(Lx, lx) {
    rev(cumsum(rev(Lx))) / lx
}
# compute_ex_var <- function(Lx, lx) {
#     rev(cumsum(rev(Lx^2))) / lx^2
# }
## Load data -------------------------------------------------------------------
races <- c("all", "non-hispanic white", "non-hispanic black", 
           "hispanic", "non-hispanic asian")
age_cuts <- c(0, seq(5, 60, by = 5), Inf) # left closed breaks
# age_cuts <- c(0, seq(70), Inf) # left closed breaks
sipp2021 <- read_csv(here("data", "sipp_snap_2021.csv")) |>
    mutate(x = cut(age, breaks = age_cuts, right = FALSE)) 
uslt2020 <- read_csv(here("data", "uslt_2020.csv")) |>
    filter(sex == "all") |>
    mutate(x = cut(x, breaks = age_cuts, right = FALSE))

## Estimate proportions and rates from SIPP ------------------------------------
svy <- sipp2021 |>
    svrepdesign(
        variables = NULL,
        repweights = "repwgt[1-240]+",
        weights = ~ repwgt0,
        type = "Fay",
        rho = 0.5
    )
pr_lost_none_by_race <- compute_props(svy, "s_lost_none", c("x", "race"))
pr_lost_mom_by_race <- compute_props(svy, "s_lost_mom", c("x", "race"))
pr_lost_dad_by_race <- compute_props(svy, "s_lost_dad", c("x", "race"))
pr_lost_both_by_race <- compute_props(svy, "s_lost_both", c("x", "race"))

pr_by_race <- full_join(
    pr_lost_none_by_race, pr_lost_mom_by_race, by = c("x", "race")
) |>
    full_join(
        pr_lost_dad_by_race, by = c("x", "race")
    ) |>
    full_join(
        pr_lost_both_by_race, by = c("x", "race")
    )

pr_lost_none_all_race <- compute_props(svy, "s_lost_none", "x")
pr_lost_mom_all_race <- compute_props(svy, "s_lost_mom", "x")
pr_lost_dad_all_race <- compute_props(svy, "s_lost_dad", "x")
pr_lost_both_all_race <- compute_props(svy, "s_lost_both", "x")

pr_all_race <- full_join(
    pr_lost_none_all_race, pr_lost_mom_all_race, by = "x"
) |>
    full_join(pr_lost_dad_all_race, by = "x") |>
    full_join(pr_lost_both_all_race, by = "x")

df_pr <- bind_rows(
    pr_all_race |> mutate(race = "all"),
    pr_by_race
)

mx_lost_none_to_lost_mom_by_race <- compute_rates(
    svy, "i_lost_none_to_lost_mom", "s_lost_none0", c("x", "race"))
mx_lost_none_to_lost_dad_by_race <- compute_rates(
    svy, "i_lost_none_to_lost_dad", "s_lost_none0", c("x", "race"))
mx_lost_none_to_lost_both_by_race <- compute_rates(
    svy, "i_lost_none_to_lost_both", "s_lost_none0", c("x", "race"))
mx_lost_dad_to_lost_both_by_race <- compute_rates(
    svy, "i_lost_dad_to_lost_both", "s_lost_dad0", c("x", "race"))
mx_lost_mom_to_lost_both_by_race <- compute_rates(
    svy, "i_lost_mom_to_lost_both", "s_lost_mom0", c("x", "race"))

mx_by_race <- full_join(
    mx_lost_none_to_lost_mom_by_race, mx_lost_none_to_lost_dad_by_race,
    by = c("x", "race")
) |>
    full_join(mx_lost_none_to_lost_both_by_race, by = c("x", "race")) |>
    full_join(mx_lost_dad_to_lost_both_by_race, by = c("x", "race")) |>
    full_join(mx_lost_mom_to_lost_both_by_race, by = c("x", "race"))

mx_lost_none_to_lost_mom_all_race <- compute_rates(
    svy, "i_lost_none_to_lost_mom", "s_lost_none0", "x")
mx_lost_none_to_lost_dad_all_race <- compute_rates(
    svy, "i_lost_none_to_lost_dad", "s_lost_none0", "x")
mx_lost_none_to_lost_both_all_race <- compute_rates(
    svy, "i_lost_none_to_lost_both", "s_lost_none0", "x")
mx_lost_dad_to_lost_both_all_race <- compute_rates(
    svy, "i_lost_dad_to_lost_both", "s_lost_dad0", "x")
mx_lost_mom_to_lost_both_all_race <- compute_rates(
    svy, "i_lost_mom_to_lost_both", "s_lost_mom0", "x")

mx_all_race <- full_join(
    mx_lost_none_to_lost_mom_all_race, mx_lost_none_to_lost_dad_all_race,
    by = "x"
) |>
    full_join(mx_lost_none_to_lost_both_all_race, by = "x") |>
    full_join(mx_lost_dad_to_lost_both_all_race, by = "x") |>
    full_join(mx_lost_mom_to_lost_both_all_race, by = "x")

df_mx <- bind_rows(
    mx_all_race |> mutate(race = "all"),
    mx_by_race
)

n_by_race <- count_samples(sipp2021, by = c(x, race))
n_all_race <- count_samples(sipp2021, by = x)

df_sipp_n <- bind_rows(
    n_all_race |> mutate(race = "all"),
    n_by_race
)


## Compute US life table -------------------------------------------------------
df_lt <- uslt2020 |>
    filter(race %in% races) |>
    aggregate_lt(c("race", "x")) |>
    select(race, x, everything())

## Join life table and rates ---------------------------------------------------
multistate_lt <- left_join(df_lt, df_pr, by = c("race", "x")) |>
    left_join(df_mx, by = c("race", "x")) |>
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
    group_by(race) |>
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
    pivot_longer(-c(race, x))

# transform standard errors
se_multistate_lt <- left_join(df_lt, df_pr, by = c("race", "x")) |>
    left_join(df_mx, by = c("race", "x")) |>
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
    select(x, race, starts_with("se_")) |>
    pivot_longer(-c(race, x))

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
    multistate_lt, here("data", "multistate-lt_snap_rep.csv"), row.names = FALSE
)
write.csv(
    se_multistate_lt, here("data", "se-multistate-lt_snap_rep.csv"), 
    row.names = FALSE
)
write.csv(df_sipp_n, here("data", "sipp_n_snap.csv"), row.names = FALSE)
