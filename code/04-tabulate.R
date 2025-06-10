##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon
##  Date: May 2025
##------------------------------------------------------------------------------
##
rm(list = ls())

## Load packages ---------------------------------------------------------------
packages <- c("dplyr", "tidyr", "readr", "stringr", 
              "knitr", "kableExtra",
              "here", "utils", "testthat")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}
source("code/utils.R")

## Functions and variables -----------------------------------------------------
sipp_period_selected <- 30
races <- c(
    "non-hispanic white",
    "non-hispanic black",
    "hispanic"
)
params <- c(
    "Lx_1", "Lx_2", "Lx_3", "Lx_4",
    "lx_1", "lx_2", "lx_3", "lx_4",
    "dx_1", "dx_2", "dx_3", "dx_4", 
    "dx_1_2", "dx_1_3", "dx_1_4", "dx_2_4", "dx_3_4",
    "ex_1", "ex_2", "ex_3", "ex_4"
)
## Load data -------------------------------------------------------------------
df <- read_csv(here(
    "data", "posteriors", "linear-rw-sipp-and-projection", 
    paste0("lt_post_", sipp_period_selected, ".csv")
)) 
## Tabulate --------------------------------------------------------------------
df_table <- df |> 
    filter(param %in% params) |>
    select(race, age, param, q025, q500, q975) |>
    mutate(
        state = gsub("^[a-zA-Z]*_", "", param),
        to = gsub("^\\d_?", "", state),
        state = gsub("_\\d$", "", state),
        param = gsub("_.*", "", param),
        name = case_match(
            param,
            "Lx" ~ paste0("${}_5 L_x(", state, ")$"),
            "lx" ~ paste0("$\\ell_x(", state, ")$"),
            "ex" ~ paste0("$e_x(", state, ")$"),
            "dx" ~ paste0(
                "${}_5 d_x(", state, if_else(nchar(to) > 0, ",", ""), to, ")$"
            )
        ),
        value = paste0(
            format(round(q500, 1), scienfific = FALSE, big.mark = ",", trim = TRUE), " (", 
            format(round(q025, 1), scienfific = FALSE, big.mark = ",", trim = TRUE), "-", 
            format(round(q975, 1), scienfific = FALSE, big.mark = ",", trim = TRUE), ")"
        ),
        value = if_else(
            age == 65 & param != "lx",
            format(round(q500), scienfific = FALSE, big.mark = ",", trim = TRUE),
            value
        )
    )

save_table <- function(df, select_race, select_state, caption) {
    age_col <- unique(df$age)
    df_wide <- lapply(
        select_state,
        function(s) {
            df |>
                filter(race == select_race, state == s) |>
                pivot_wider(id_cols = c(race, age, state)) |>
                select(-c(age, race, state)) |>
                select(
                    starts_with("$\\ell"), starts_with("${}_5 L"),
                    starts_with("${}_5 d"), starts_with("$e_")
                )
        }
    ) |>
        bind_cols()
    df_print <- bind_cols("$x$" = age_col, df_wide)
    kable(
        df_print, format = "latex", escape = FALSE, booktabs = TRUE,
        caption = caption
    ) |>
        cat(file = here(
            "tables", 
            paste0(
                select_race, "-", 
                paste(select_state, collapse = "-"), 
                ".tex"
            )
        ))
}

save_table(
    df_table, "non-hispanic black", "1",
    paste(
        "Multistate life table by parent mortality status for",
        "the non-Hispanic black population in the United States, 2020.",
        "State 1: Lost none."
    )
)

save_table(
    df_table, "non-hispanic black", "2",
    paste(
        "Multistate life table by parent mortality status for",
        "the non-Hispanic black population in the United States, 2020.",
        "State 2: Lost mother."
    )
)

save_table(
    df_table, "non-hispanic black", "3",
    paste(
        "Multistate life table by parent mortality status for",
        "the non-Hispanic black population in the United States, 2020.",
        "State 3: Lost father."
    )
)

save_table(
    df_table, "non-hispanic black", "4",
    paste(
        "Multistate life table by parent mortality status for",
        "the non-Hispanic black population in the United States, 2020.",
        "State 4: Lost both."
    )
)

save_table(
    df_table, "non-hispanic white", "1",
    paste(
        "Multistate life table by parent mortality status for",
        "the non-Hispanic white population in the United States, 2020.",
        "State 1: Lost none."
    )
)

save_table(
    df_table, "non-hispanic white", "2",
    paste(
        "Multistate life table by parent mortality status for",
        "the non-Hispanic white population in the United States, 2020.",
        "State 2: Lost mother."
    )
)

save_table(
    df_table, "non-hispanic white", "3",
    paste(
        "Multistate life table by parent mortality status for",
        "the non-Hispanic white population in the United States, 2020.",
        "State 3: Lost father."
    )
)

save_table(
    df_table, "non-hispanic white", "4",
    paste(
        "Multistate life table by parent mortality status for",
        "the non-Hispanic white population in the United States, 2020.",
        "State 4: Lost both"
    )
)


save_table(
    df_table, "hispanic", "1",
    paste(
        "Multistate life table by parent mortality status for",
        "the Hispanic population in the United States, 2020.",
        "State 1: Lost none."
    )
)

save_table(
    df_table, "hispanic", "2",
    paste(
        "Multistate life table by parent mortality status for",
        "the Hispanic population in the United States, 2020.",
        "State 2: Lost mother."
    )
)

save_table(
    df_table, "hispanic", "3",
    paste(
        "Multistate life table by parent mortality status for",
        "the Hispanic population in the United States, 2020.",
        "State 3: Lost father."
    )
)

save_table(
    df_table, "hispanic", "4",
    paste(
        "Multistate life table by parent mortality status for",
        "the Hispanic population in the United States, 2020.",
        "State 4: Lost both"
    )
)
