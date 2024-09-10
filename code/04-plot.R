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
packages <- c("dplyr", "tidyr", "readr", "stringr", "ggplot2", 
              "here", "utils", "testthat")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}
source("code/utils.R")

## Functions -------------------------------------------------------------------

## Load data -------------------------------------------------------------------
# SIPP estimates
races <- c(
    "non-hispanic white",
    "non-hispanic black",
    "hispanic"
)

sipp_pr <- lapply(
    PERIODS,
    function(prd) {
        read_csv(here("data", paste0("sipp_pr_", prd, ".csv"))) |>
            filter(sex == "all", race %in% races) |>
            mutate(
                age = str_extract(x, "(?<=\\[)\\d+") |> as.numeric(),
                sipp_period = prd
            )
    }
) |>
    bind_rows() |>
    select(-"sex")

sipp_mx <- lapply(
    PERIODS,
    function(prd) {
        read_csv(here("data", paste0("sipp_mx_", prd, ".csv"))) |>
            filter(sex == "all", race %in% races) |>
            mutate(
                age = str_extract(x, "(?<=\\[)\\d+") |> as.numeric(),
                sipp_period = prd
            )
    }
) |>
    bind_rows() |>
    select(-"sex")

projected <- bind_rows(
    read_csv(here("data", "projected-mom.csv")) |>
        rename_with(~ str_replace(.x, "_lost", "_lost_mom")) |>
        pivot_longer(
            ends_with("lost_mom"),
            names_to = "param",
            values_to = "projected"
        ),
    read_csv(here("data", "projected-dad.csv")) |>
        rename_with(~ str_replace(.x, "_lost", "_lost_dad")) |>
        pivot_longer(
            ends_with("lost_dad"),
            names_to = "param",
            values_to = "projected"
        ),
) |>
    mutate(
        age = str_extract(x, "(?<=\\[)\\d+") |> as.numeric(),
        data = "SIPP + projection"
    )

projected <- bind_rows(
    projected,
    projected |> mutate(data = "SIPP + simulated projection")
)

df_sipp_only <- lapply(
    PERIODS,
    function(prd)
        read_csv(
            here(
                "data", "posteriors", "loglin", 
                paste0("lt_post_", prd, ".csv")
            )
        ) |>
        mutate(data = "SIPP only", sipp_period = prd)
) |>
    bind_rows()

df_sipp_proj <- lapply(
    PERIODS,
    function(prd)
        read_csv(
            here(
                "data", "posteriors", "projection-and-loglin", 
                paste0("lt_post_", prd, ".csv")
            )
        ) |>
        mutate(data = "SIPP + projection", sipp_period = prd)
) |>
    bind_rows()

df_sipp_proj_sim <- lapply(
    PERIODS,
    function(prd)
        read_csv(
            here(
                "data", "posteriors", "projection-sim-and-loglin", 
                paste0("lt_post_", prd, ".csv")
            )
        ) |>
        mutate(data = "SIPP + simulated projection", sipp_period = prd)
) |>
    bind_rows()

df <- bind_rows(df_sipp_only, df_sipp_proj, df_sipp_proj_sim)

df_pr <- df |>
    filter(grepl("^phi", param)) |>
    mutate(
        state = str_remove_all(param, "(phi_)"),
        state = STATES[state]
    ) |> 
    bind_rows(
        df |>
            filter(grepl("^unc_phi", param)) |> 
            mutate(
                state = paste0(
                    "lost_", str_remove(param, "unc_phi_"), "_or_both"
                )
            ) |>
            left_join(
                projected |>
                    filter(grepl("^p_", param)) |>
                    mutate(state = paste0(str_remove(param, "p_"), "_or_both")) |>
                    select(race, age, state, projected, data),
                by = c("race", "age", "state", "data")
            ) |>
            left_join(
                projected |>
                    filter(grepl("^sim_se_p_", param),
                           grepl("simulate", data)) |>
                    mutate(state = paste0(str_remove(param, "sim_se_p_"), "_or_both")) |>
                    rename(se_projected = projected) |>
                    select(race, age, state, se_projected, data),
                by = c("race", "age", "state", "data")
            )
    ) |>
    left_join(
        sipp_pr |>
            pivot_longer(c(starts_with("pr_"), starts_with("se_"))) |>
            mutate(
                state = str_extract(name, "[a-z]+_[a-z]+$"),
                type = str_extract(name, "^[a-z]{2}")
            ) |>
            pivot_wider(
                id_cols = c(x, race, age, state, sipp_period),
                values_from = value,
                names_from = type
            ) |>
            rename(value = "pr"),
        by = c("race", "age", "state", "sipp_period")
    )

df_mx <- df |>
    filter(grepl("^theta", param)) |>
    mutate(
        from = str_remove_all(param, "(theta_)|(_\\d$)"),
        to = str_remove(param, "theta_\\d_"),
        from = STATES[from],
        to = STATES[to]
    ) |> 
    bind_rows(
        df |>
            filter(grepl("^unc_theta", param)) |> 
            mutate(
                from = "unconditional",
                to = paste0("lost_", str_remove(param, "unc_theta_"), "_or_both")
            ) |>
            left_join(
                projected |>
                    filter(grepl("^m_lost", param)) |>
                    mutate(to = paste0(str_remove(param, "^m_"), "_or_both")) |>
                    select(race, age, to, projected, data),
                by = c("race", "age", "to", "data")
            ) |>
            left_join(
                projected |>
                    filter(grepl("^sim_se_m_", param),
                           grepl("simulate", data)) |>
                    mutate(to = paste0(str_remove(param, "^sim_se_m_"), "_or_both")) |>
                    rename(se_projected = projected) |>
                    select(race, age, to, se_projected, data),
                by = c("race", "age", "to", "data")
            )
    ) |>
    left_join(
        sipp_mx |>
            pivot_longer(c(starts_with("mx_"), starts_with("se_"))) |>
            mutate(
                from = str_extract(name, "^[a-z_]+to") |>
                    str_remove_all(("(se_)|(mx_)|(_to)")),
                to = str_extract(name, "[a-z]+_[a-z]+$"),
                type = str_extract(name, "^[a-z]{2}")
            ) |>
            pivot_wider(
                id_cols = c(x, race, age, from, to, sipp_period),
                values_from = value,
                names_from = type
            ) |>
            rename(value = "mx"),
        by = c("race", "age", "from", "to", "sipp_period")
    )

df_dx <- df |>
    filter(grepl("^dx", param)) |>
    mutate(
        terminal = !str_detect(param, "\\d_\\d$"),
        from = if_else(
            terminal, 
            str_remove_all(param, "(dx_)"),
            str_remove_all(param, "(dx_)|(_\\d$)")
        ),
        to = str_remove(param, "theta_\\d_"),
        from = STATES[from],
        to = if_else(
            terminal,
            "dead",
            STATES[to]
        )
    )

df_ex <- df |>
    filter(grepl("^ex_", param)) |>
    mutate(
        state = str_remove(param, "ex_"),
        state = STATES[state]
    )

df_lx <- df |>
    filter(grepl("^lx", param)) |>
    mutate(
        state = str_remove(param, "lx_"),
        state = STATES[state]
    )

df_Lx <- df |>
    filter(grepl("^Lx", param)) |>
    mutate(
        state = str_remove(param, "Lx_"),
        state = STATES[state]
    )

## Plot ------------------------------------------------------------------------
df_pr |>
    filter(sipp_period == 20) |>
    ggplot(aes(x = age, y = q500, colour = race, fill = race)) +
    theme_minimal() +
    geom_ribbon(aes(ymin = q250, ymax = q750),
                alpha = .2, color = NA, show.legend = FALSE) +
    geom_line() +
    geom_linerange(aes(ymin = value - se, ymax = value + se), show.legend = FALSE) +
    geom_linerange(aes(ymin = projected - se_projected, ymax = projected + se_projected), show.legend = FALSE) +
    geom_point(aes(y = value), shape = 1) +
    geom_point(aes(y = projected), shape = 4) +
    # scale_y_continuous(transform = "logit") +
    facet_grid(vars(param), vars(data), scales = "free") +
    labs(title = "pr")
    

df_ex |>
    filter(grepl("simulated", data)) |>
    ggplot(aes(x = age, colour = state, fill = state)) +
    theme_minimal() +
    geom_line(aes(y = q500), position = position_stack()) +
    geom_area(aes(y = q500), position = position_stack(), alpha = .2,
              show.legend = FALSE) +
    facet_grid(vars(race), vars(sipp_period), scales = "free") +
    theme(legend.position = "bottom") +
    labs(title = "ex")

df_mx |>
    filter(sipp_period == 20, age < 65) |>
    ggplot(aes(x = age, colour = race, fill = race)) +
    theme_minimal() +
    # geom_ribbon(aes(ymin = q025, ymax = q975),
    #             alpha = .2, color = NA, show.legend = FALSE) +
    geom_ribbon(aes(ymin = q250, ymax = q750),
                alpha = .2, color = NA, show.legend = FALSE) +
    geom_line(aes(y = q500), show.legend = FALSE) +
    geom_point(aes(y = value, shape = se > 0)) +
    geom_point(aes(y = projected), shape = 4) +
    geom_linerange(aes(ymin = value - se, ymax = value + se), show.legend = FALSE) +
    geom_linerange(aes(ymin = projected - se_projected, ymax = projected + se_projected), show.legend = FALSE) +
    facet_grid(vars(param), vars(data), scales = "free") +
    # scale_y_continuous(transform = "log") +
    scale_shape_manual(values = c(`TRUE` = 1, `FALSE` = NA)) +
    theme(legend.position = "bottom") +
    labs(title = "mx")

df_mx |>
    filter(grepl("1_4", param), age < 65) |>
    ggplot(aes(x = age, colour = race, fill = race)) +
    theme_minimal() +
    # geom_ribbon(aes(ymin = q025, ymax = q975),
    #             alpha = .2, color = NA, show.legend = FALSE) +
    geom_ribbon(aes(ymin = q250, ymax = q750),
                alpha = .2, color = NA, show.legend = FALSE) +
    geom_line(aes(y = q500), show.legend = FALSE) +
    geom_point(aes(y = value, shape = se > 0)) +
    geom_linerange(aes(ymin = value - se, ymax = value + se), show.legend = FALSE) +
    facet_grid(vars(data), vars(sipp_period), scales = "free") +
    scale_y_continuous(transform = "log") +
    scale_shape_manual(values = c(`TRUE` = 1, `FALSE` = NA)) +
    theme(legend.position = "bottom") +
    labs(title = "mx(1,4)")

# df_ex |>
#     filter(grepl("lost_none|lost_mom", state), 
#            sipp_period == 20, age == 25
#            ) |>
#     ggplot(aes(x = age, colour = race, fill = race)) +
#     theme_minimal() +
#     geom_point(aes(y = q500), position = position_dodge(width = 3)) +
#     geom_linerange(aes(ymin = q250, ymax = q750), position = position_dodge(width = 3)) +
#     facet_grid(vars(state), vars(data, sipp_period), scales = "free") +
#     # scale_y_continuous(limits = c(0, 100000), oob = scales::squish) +
#     theme(legend.position = "bottom") +
#     labs(title = "ex with both parents at age 20")

df_lx |>
    filter(sipp_period == 20) |>
    ggplot(aes(x = age, colour = race, fill = race)) +
    theme_minimal() +
    # geom_ribbon(aes(ymin = q025, ymax = q975),
    #             alpha = .2, color = NA, show.legend = FALSE) +
    geom_ribbon(aes(ymin = q250, ymax = q750),
                alpha = .2, color = NA) +
    geom_line(aes(y = q500), show.legend = FALSE) +
    facet_grid(vars(param), vars(data), scales = "free") +
    # scale_y_continuous(transform = "log") +
    scale_shape_manual(values = c(`TRUE` = 1, `FALSE` = NA)) +
    theme(legend.position = "bottom") +
    labs(title = "mx")
