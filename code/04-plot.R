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
packages <- c("dplyr", "tidyr", "readr", "stringr", 
              "ggplot2", "grid", "gridExtra", "ggh4x",
              "here", "utils", "testthat")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}
source("code/utils.R")

## Functions and variables -----------------------------------------------------
sipp_period_selected <- 30
get_legend <- function(plt) {
    tmp <- ggplot_gtable(ggplot_build(plt))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}
races <- c(
    "non-hispanic white",
    "non-hispanic black",
    "hispanic"
)
labels_races <- c(
    `non-hispanic black` = "Non-Hispanic black", 
    `non-hispanic white` = "Non-Hispanic white",
    `hispanic` = "Hispanic"
)
labels_races_abc <- paste0(letters[1:3], ") ", labels_races)
cmap_races <- c(
    `non-hispanic black` = "#1b9e77", 
    `non-hispanic white` = "grey40",
    `hispanic` = "#7570b3"
)
cmap_races_abc <- cmap_races
names(cmap_races_abc) <- labels_races_abc
labels_states <- c(
    `lost_none` = "Lost none", 
    `lost_mom` = "Lost mother only", 
    `lost_dad` = "Lost father only", 
    `lost_both` = "Lost both"
)
labels_states_abc <- c(
    `lost_none` = "a) Lost none", 
    `lost_mom` = "b) Lost mother only", 
    `lost_dad` = "c) Lost father only", 
    `lost_both` = "d) Lost both"
)
labels_states_unc <- c(
    `lost_mom_or_both` = "Lost mother or both", 
    `lost_dad_or_both` = "Lost father or both"
)
cmap_states <- c(
    "grey40", 
    "#fc8d62",
    "#e78ac3",
    "#8da0cb"
)
cmap_periods <- c(
    `1` = "grey40",
    `10` = "#7bccc4",
    `20` = "#43a2ca",
    `30` = "#0868ac"
)
names(cmap_states) <- names(labels_states)
font_family <- "Times"
## Load data -------------------------------------------------------------------
# SIPP estimates
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
        select(-starts_with("sim_mean_")) |>
        rename_with(~ str_replace(.x, "_lost", "_lost_mom")) |>
        pivot_longer(
            ends_with("lost_mom"),
            names_to = "param",
            values_to = "projected"
        ),
    read_csv(here("data", "projected-dad.csv")) |>
        select(-starts_with("sim_mean_")) |>
        rename_with(~ str_replace(.x, "_lost", "_lost_dad")) |>
        pivot_longer(
            ends_with("lost_dad"),
            names_to = "param",
            values_to = "projected"
        ),
) |>
    mutate(
        age = str_extract(x, "(?<=\\[)\\d+") |> as.numeric(),
        augmented = TRUE
    )

df_sipp <- lapply(
    PERIODS,
    function(prd)
        read_csv(
            here(
                "data", "posteriors", "linear-rw-sipp", 
                paste0("lt_post_", prd, ".csv")
            )
        ) |>
        mutate(augmented = FALSE, sipp_period = prd)
) |>
    bind_rows()

df_proj <- lapply(
    PERIODS,
    function(prd)
        read_csv(
            here(
                "data", "posteriors", "linear-rw-sipp-and-projection", 
                paste0("lt_post_", prd, ".csv")
            )
        ) |>
        mutate(augmented = TRUE, sipp_period = prd)
) |>
    bind_rows()

df <- bind_rows(df_sipp, df_proj)

df_pr <- df |>
    filter(grepl("^p_a_", param)) |>
    mutate(
        state = str_remove_all(param, "(p_a_)"),
        state = STATES[state]
    ) |> 
    bind_rows(
        df |>
            filter(grepl("^unc_p_a_", param)) |> 
            mutate(
                state = paste0(
                    "lost_", str_remove(param, "unc_p_a_"), "_or_both"
                )
            ) |>
            left_join(
                projected |>
                    filter(grepl("^p_", param)) |>
                    mutate(state = paste0(str_remove(param, "p_"), "_or_both")) |>
                    select(race, age, state, projected, augmented),
                by = c("race", "age", "state", "augmented")
            ) |>
            left_join(
                projected |>
                    filter(grepl("^sim_se_p_", param)) |>
                    mutate(state = paste0(str_remove(param, "sim_se_p_"), "_or_both")) |>
                    rename(se_projected = projected) |>
                    select(race, age, state, se_projected, augmented),
                by = c("race", "age", "state", "augmented")
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
    filter(grepl("^m_a_", param)) |>
    mutate(
        from = str_remove_all(param, "(m_a_)|(_\\d$)"),
        to = str_remove(param, "m_a_\\d_"),
        from = STATES[from],
        to = STATES[to]
    ) |> 
    bind_rows(
        df |>
            filter(grepl("^unc_m_a_", param)) |> 
            mutate(
                from = "unconditional",
                to = paste0("lost_", str_remove(param, "unc_m_a_"), "_or_both")
            ) |>
            left_join(
                projected |>
                    filter(grepl("^m_lost", param)) |>
                    mutate(to = paste0(str_remove(param, "^m_"), "_or_both")) |>
                    select(race, age, to, projected, augmented),
                by = c("race", "age", "to", "augmented")
            ) |>
            left_join(
                projected |>
                    filter(grepl("^sim_se_m_", param)) |>
                    mutate(to = paste0(str_remove(param, "^sim_se_m_"), "_or_both")) |>
                    rename(se_projected = projected) |>
                    select(race, age, to, se_projected, augmented),
                by = c("race", "age", "to", "augmented")
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
        to = str_remove(param, "dx_\\d_"),
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
### inspect prevalence ---------------------------------------------------------
plot_p_a <- function(df, race_select, title, se_multiplier = 1,
                     point_size = 1.2) {
    df <- df |> 
        filter(race == race_select) |>
        mutate(
            sipp_period = as.character(sipp_period),
            value = if_else(value > 0, value, NA),
            projected = if_else(projected > 0, projected, NA),
            augmented = if_else(
                augmented, 
                "Survey & projection", 
                "Survey"
            )
        )
    
    df |>
        ggplot(aes(x = age)) +
        theme_minimal() +
        geom_ribbon(
            aes(ymin = q250, ymax = q750, fill = sipp_period),
            alpha = .5, 
            colour = NA
        ) +
        geom_line(aes(y = q500, color = sipp_period), linewidth = .2) +
        geom_linerange(
            aes(
                ymin = value - se_multiplier * se,
                ymax = value + se_multiplier * se,
                color = sipp_period
            ),
            alpha = .5, linewidth = .5,
            position = position_dodge2(width = 1.5)
        ) +
        geom_point(
            aes(y = value, color = sipp_period), 
            position = position_dodge2(width = 1.5),
            shape = 1, size = point_size
        ) +
        geom_linerange(
            aes(
                ymin = projected - se_multiplier * se_projected,
                ymax = projected + se_multiplier * se_projected
            ),
            alpha = .5, linewidth = .5,
            data = df |> filter(sipp_period == "1")
        ) +
        geom_point(
            aes(y = projected), 
            shape = 4, alpha = .5, size = point_size,
            data = df |> filter(sipp_period == "1")
        ) +
        scale_y_continuous(n.breaks = 3) +
        scale_x_continuous(
            breaks = c(0, 30, 60),
            labels = c("[0,5)", "[30,35)", "[60,65)")
        ) +
        scale_colour_manual(
            values = cmap_periods, 
            aesthetics = c("colour", "fill")
        ) +
        facet_grid(vars(param), vars(augmented), scales = "free") +
        labs(
            y = NULL, 
            title = title,
            x = "Age (years)", 
        )  +
        theme(
            legend.position = "none",
            plot.title = element_text(
                hjust = .5, size = 12, family = font_family
            ),
            plot.title.position = "plot",
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 10, family = font_family),
            panel.spacing = unit(1, "line"),
            strip.text.y = element_blank(),
            strip.text.x = element_text(
                hjust = .5, size = 10, family = font_family
            )
        )
}

df_p_a <- df_pr |>
    filter(age < 65) |>
    mutate(
        param = factor(
            param,
            levels = c(
                "p_a_1", 
                "p_a_2", 
                "p_a_3", 
                "p_a_4", 
                "unc_p_a_mom", 
                "unc_p_a_dad"
            )
        )
    )

plt_p_a_b <- plot_p_a(df_p_a, "non-hispanic black", "a) Non-Hispanic black") +
    scale_y_facet(param == "p_a_2", limits = c(0, 0.15), breaks = c(0, .1)) +
    scale_y_facet(param == "p_a_3", limits = c(0, 0.4), breaks = c(0, .2, .4))
plt_p_a_w <- plot_p_a(df_p_a, "non-hispanic white", "b) Non-Hispanic white") +
    scale_y_facet(param == "p_a_2", limits = c(0, 0.15), breaks = c(0, .1)) +
    scale_y_facet(param == "p_a_3", limits = c(0, 0.4), breaks = c(0, .2, .4))
plt_p_a_h <- plot_p_a(df_p_a, "hispanic", "c) Hispanic") +
    scale_y_facet(param == "p_a_2", limits = c(0, 0.15), breaks = c(0, .1)) +
    scale_y_facet(param == "p_a_3", limits = c(0, 0.4), breaks = c(0, .2, .4))
p_a_legend <- get_legend(
    ggplot(df_p_a, aes(x = age)) +
        theme_minimal() +
        geom_bar(aes(y = q500, fill = as.character(sipp_period)), stat = "identity") +
        geom_point(aes(y = value, shape = "Survey")) +
        geom_point(aes(y = projected, shape = "Matrix projection")) +
        scale_colour_manual(
            name = "Survey life span period (years)",
            values = cmap_periods, 
            aesthetics = c("colour", "fill")
        ) +
        scale_shape_manual(
            name = "Data source",
            breaks = c("Survey", "Matrix projection"),
            values = c(1, 4),
        ) +
        theme(
            legend.position = "bottom",
            legend.justification = "right",
            legend.key.height = unit(.1, "pt"),
            legend.text = element_text(family = font_family),
            legend.title = element_text(family = font_family)
        )
)


p_a_labels <- data.frame(
    param = unique(df_p_a$param),
    aug = " ",
    labels = c(
        "italic({}[5]~p[x](1))",
        "italic({}[5]~p[x](2))",
        "italic({}[5]~p[x](3))",
        "italic({}[5]~p[x](4))",
        "italic({}[5]~p[x](m))",
        "italic({}[5]~p[x](f))"
    )
) |>
    ggplot() +
    theme_minimal() +
    geom_text(
        aes(x = 0, y = 0, label = labels), 
        parse = TRUE, 
        size = 4,
        family = "serif",
        hjust = 1,
        vjust = .5
    ) +
    facet_grid(vars(param), vars(aug)) +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_x_continuous(limits = c(-1, 0), breaks = 0, labels = " ") +
    labs(x = " ", y = NULL, title = " ")  +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = .5, size = 12),
        plot.title.position = "plot",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 10),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "line"),
        strip.text.y = element_blank()
    )

g_p_a <- grid.arrange(
    p_a_labels,
    plt_p_a_b, plt_p_a_w, plt_p_a_h, 
    p_a_legend,
    heights = c(1, .02), 
    widths = c(.25, 1, 1, 1),
    layout_matrix = rbind(
        c(1, 2, 3, 4),
        c(5, 5, 5, 5)
    )
)

ggsave(
    here("plots", "parent-p_a.png"), g_p_a, 
    device = "png", width = 10, height = 6, dpi = 400
)

### inspect incidence ----------------------------------------------------------
df_m_a <- df_mx |>
    filter(age < 65) |>
    mutate(
        param = factor(
            param,
            levels = c(
                "m_a_1_2", 
                "m_a_1_3", 
                "m_a_1_4", 
                "m_a_2_4",
                "m_a_3_4", 
                "unc_m_a_mom", 
                "unc_m_a_dad"
            )
        )
    )

plt_m_a_b <- plot_p_a(df_m_a, "non-hispanic black", "a) Non-Hispanic black") +
    scale_y_facet(param == "m_a_1_2", limits = c(0, .1), breaks = c(0, .05, .1)) +
    scale_y_facet(param == "m_a_1_3", limits = c(0, .14), breaks = c(0, .1)) +
    scale_y_facet(
        param == "m_a_1_4", 
        breaks = c(.0001, .001, .01),
        labels = c("0.0001", "0.001", "0.01"), 
        limits = c(0.000001, .1), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_2_4", 
        breaks = c(.001, .01, .1),
        labels = c("0.001", "0.01", "0.1"), 
        limits = c(0.0001, .3), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_3_4", 
        breaks = c(.001, .01),
        labels = c("0.001", "0.01"), 
        limits = c(0.0001, .3), 
        transform = "log",
        oob = scales::oob_squish
    ) +
    scale_y_facet(param == "unc_m_a_mom", limits = c(0, .14), breaks = c(0, .05, .1)) +
    scale_y_facet(param == "unc_m_a_dad", limits = c(0, .14), breaks = c(0, .05, .1))
plt_m_a_w <- plot_p_a(df_m_a, "non-hispanic white", "b) Non-Hispanic white") +
    scale_y_facet(param == "m_a_1_2", limits = c(0, .1), breaks = c(0, .05, .1)) +
    scale_y_facet(param == "m_a_1_3", limits = c(0, .14), breaks = c(0, .1)) +
    scale_y_facet(
        param == "m_a_1_4", 
        breaks = c(.0001, .001, .01),
        labels = c("0.0001", "0.001", "0.01"), 
        limits = c(0.000001, .1), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_2_4", 
        breaks = c(.001, .01, .1),
        labels = c("0.001", "0.01", "0.1"), 
        limits = c(0.0001, .3), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_3_4", 
        breaks = c(.001, .01),
        labels = c("0.001", "0.01"), 
        limits = c(0.0001, .3), 
        transform = "log",
        oob = scales::oob_squish
    ) +
    scale_y_facet(param == "unc_m_a_mom", limits = c(0, .14), breaks = c(0, .05, .1)) +
    scale_y_facet(param == "unc_m_a_dad", limits = c(0, .14), breaks = c(0, .05, .1))
plt_m_a_h <- plot_p_a(df_m_a, "hispanic", "c) Hispanic") +
    scale_y_facet(param == "m_a_1_2", limits = c(0, .1), breaks = c(0, .05, .1)) +
    scale_y_facet(param == "m_a_1_3", limits = c(0, .14), breaks = c(0, .1)) +
    scale_y_facet(
        param == "m_a_1_4", 
        breaks = c(.0001, .001, .01),
        labels = c("0.0001", "0.001", "0.01"), 
        limits = c(0.000001, .1), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_2_4", 
        breaks = c(.001, .01, .1),
        labels = c("0.001", "0.01", "0.1"), 
        limits = c(0.0001, .3), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_3_4", 
        breaks = c(.001, .01),
        labels = c("0.001", "0.01"), 
        limits = c(0.0001, .3), 
        transform = "log",
        oob = scales::oob_squish
    ) +
    scale_y_facet(param == "unc_m_a_mom", limits = c(0, .14), breaks = c(0, .05, .1)) +
    scale_y_facet(param == "unc_m_a_dad", limits = c(0, .14), breaks = c(0, .05, .1))

m_a_labels <- data.frame(
    param = unique(df_m_a$param),
    aug = " ",
    labels = c(
        "italic({}[5]~m[x](1,2))",
        "italic({}[5]~m[x](1,3))",
        "italic({}[5]~m[x](1,4))",
        "italic({}[5]~m[x](2,4))",
        "italic({}[5]~m[x](3,4))",
        "italic({}[5]~m[x](m))",
        "italic({}[5]~m[x](f))"
    )
) |>
    ggplot() +
    theme_minimal() +
    geom_text(
        aes(x = 0, y = 0, label = labels), 
        parse = TRUE, 
        size = 3.2,
        family = "serif",
        hjust = 1,
        vjust = .5
    ) +
    facet_grid(vars(param), vars(aug)) +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_x_continuous(limits = c(-1, 0), breaks = 0, labels = " ") +
    labs(x = " ", y = NULL, title = " ")  +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = .5, size = 12),
        plot.title.position = "plot",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 10),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "line"),
        strip.text.y = element_blank()
    )

g_m_a <- grid.arrange(
    m_a_labels,
    plt_m_a_b, plt_m_a_w, plt_m_a_h, 
    p_a_legend,
    heights = c(1, .02), 
    widths = c(.25, 1, 1, 1),
    layout_matrix = rbind(
        c(1, 2, 3, 4),
        c(5, 5, 5, 5)
    )
)
ggsave(
    here("plots", "parent-m_a.png"), g_m_a, 
    device = "png", width = 10, height = 8, dpi = 400
)

df_m_a_sub <- df_m_a |>
    filter(grepl("_4$", param))


plt_m_a_b_sub <- plot_p_a(df_m_a_sub, "non-hispanic black", "a) Non-Hispanic black") +
    scale_y_facet(
        param == "m_a_1_4", 
        breaks = c(.0001, .001, .01),
        labels = c("0.0001", "0.001", "0.01"), 
        limits = c(0.000001, .1), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_2_4", 
        breaks = c(.001, .01, .1),
        labels = c("0.001", "0.01", "0.1"), 
        limits = c(0.0001, .3), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_3_4", 
        breaks = c(.001, .01),
        labels = c("0.001", "0.01"), 
        limits = c(0.0001, .3), 
        transform = "log",
        oob = scales::oob_squish
    )
plt_m_a_w_sub <- plot_p_a(df_m_a_sub, "non-hispanic white", "b) Non-Hispanic white") +
    scale_y_facet(
        param == "m_a_1_4", 
        breaks = c(.0001, .001, .01),
        labels = c("0.0001", "0.001", "0.01"), 
        limits = c(0.000001, .1), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_2_4", 
        breaks = c(.001, .01, .1),
        labels = c("0.001", "0.01", "0.1"), 
        limits = c(0.0001, .3), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_3_4", 
        breaks = c(.001, .01),
        labels = c("0.001", "0.01"), 
        limits = c(0.0001, .3), 
        transform = "log",
        oob = scales::oob_squish
    )
plt_m_a_h_sub <- plot_p_a(df_m_a_sub, "hispanic", "c) Hispanic") +
    scale_y_facet(
        param == "m_a_1_4", 
        breaks = c(.0001, .001, .01),
        labels = c("0.0001", "0.001", "0.01"), 
        limits = c(0.000001, .1), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_2_4", 
        breaks = c(.001, .01, .1),
        labels = c("0.001", "0.01", "0.1"), 
        limits = c(0.0001, .3), 
        transform = "log"
    ) +
    scale_y_facet(
        param == "m_a_3_4", 
        breaks = c(.001, .01),
        labels = c("0.001", "0.01"), 
        limits = c(0.0001, .3), 
        transform = "log",
        oob = scales::oob_squish
    )

m_a_legend_sub <- get_legend(
    ggplot(df_m_a_sub, aes(x = age)) +
        theme_minimal() +
        geom_bar(aes(y = q500, fill = as.character(sipp_period)), stat = "identity") +
        scale_colour_manual(
            name = "Survey life span period (years)",
            values = cmap_periods, 
            aesthetics = c("colour", "fill")
        ) +
        theme(
            legend.position = "bottom",
            legend.justification = "right",
            legend.key.height = unit(.1, "pt"),
            legend.text = element_text(family = font_family),
            legend.title = element_text(family = font_family)
        )
)

m_a_labels_sub <- data.frame(
    param = unique(df_m_a_sub$param),
    aug = " ",
    labels = c(
        "italic({}[5]~m[x](1,4))",
        "italic({}[5]~m[x](2,4))",
        "italic({}[5]~m[x](3,4))"
    )
) |>
    ggplot() +
    theme_minimal() +
    geom_text(
        aes(x = 0, y = 0, label = labels), 
        parse = TRUE, 
        size = 3.2,
        family = "serif",
        hjust = 1,
        vjust = .5
    ) +
    facet_grid(vars(param), vars(aug)) +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_x_continuous(limits = c(-1, 0), breaks = 0, labels = " ") +
    labs(x = " ", y = NULL, title = " ")  +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = .5, size = 12),
        plot.title.position = "plot",
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 10),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "line"),
        strip.text.y = element_blank()
    )
g_m_a_sub <- grid.arrange(
    m_a_labels_sub,
    plt_m_a_b_sub, plt_m_a_w_sub, plt_m_a_h_sub, 
    m_a_legend_sub,
    heights = c(1, .05), 
    widths = c(.25, 1, 1, 1),
    layout_matrix = rbind(
        c(1, 2, 3, 4),
        c(5, 5, 5, 5)
    )
)
ggsave(
    here("plots", "parent-m_a_sub.png"), g_m_a_sub, 
    device = "png", width = 10, height = 6, dpi = 400
)



### plot lx --------------------------------------------------------------------
plot_lx <- function(df) {
    df |>
        ggplot(aes(x = age, y = q500, colour = race, fill = race)) +
        theme_minimal() +
        geom_ribbon(aes(ymin = q025, ymax = q975),
                    alpha = .2, color = NA) +
        geom_line() +
        scale_colour_manual(
            values = cmap_races, 
            labels = labels_races,
            aesthetics = list("colour", "fill"),
            guide = guide_legend(
                title = NULL, 
                override.aes = list(alpha = 1)
            )
        ) +
        scale_x_continuous(
            breaks = c(0, 30, 60),
            labels = c("[0,5)", "[30,35)", "[60,65)")
        ) +
        labs(x = "Age (years)", y = "Life table persons (1,000s)") +
        facet_wrap(vars(state), scales = "free", nrow = 1) +
        theme(
            legend.position = "bottom",
            legend.justification = "right",
            legend.key.height = unit(.1, "pt"),
            legend.text = element_text(family = font_family),
            legend.title = element_text(family = font_family),
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 10, family = font_family),
            panel.spacing = unit(1, "line"),
            strip.text.y = element_blank(),
            strip.text.x = element_text(
                hjust = .5, size = 10, family = font_family
            )
        )
}
df_lx_selected <- df_lx |>
    filter(!is.na(state), sipp_period == sipp_period_selected, augmented) |>
    mutate(
        state = factor(
            state,
            levels = names(labels_states),
            labels = labels_states_abc
        ),
        race = factor(
            race,
            levels = names(labels_races)
        )
    )
plt_lx <- plot_lx(df_lx_selected) +
    scale_y_facet(
        state == labels_states_abc[1], 
        limits = c(0, 100000), 
        labels = ~ .x / 1000
    ) +
    scale_y_facet(
        state %in% labels_states_abc[2:4],
        limits = c(0, 47000),
        labels = ~ .x / 1000
    )

ggsave(
    here("plots", "parent-lx.png"), plt_lx, 
    device = "png", width = 9, height = 3, dpi = 400
)

df_lx_sub <- df_lx |>
    filter(state == "lost_none", !is.na(state)) |>
    mutate(
        state = factor(
            state,
            levels = names(labels_states),
            labels = labels_states
        ),
        race = factor(
            race,
            levels = names(labels_races),
            labels = labels_races_abc
        ),
        augmented = if_else(augmented, "Survey & projection", "Survey"),
        sipp_period = as.character(sipp_period),
        q500 = if_else(q500 < 0, NA, q500)
    )
df_lx_sub_0 <- df_lx |>
    filter(state == "lost_none", !is.na(state), q500 < 0) |>
    mutate(
        race = factor(
            race,
            levels = names(labels_races),
            labels = labels_races_abc
        ),
        augmented = if_else(augmented, "Survey & projection", "Survey"),
        sipp_period = as.character(sipp_period)
    ) |>
    add_row(
        race = "c) Hispanic", 
        age = 50, 
        q500 = 0, 
        sipp_period = "1", 
        augmented = "Survey"
    )
plt_lx_by_sipp_period <- df_lx_sub |>
    mutate() |>
    ggplot(aes(
        x = age, y = q500,
        colour = sipp_period, fill = sipp_period)
    ) +
    theme_minimal() +
    geom_ribbon(aes(ymin = q025, ymax = q975),
                alpha = .2, color = NA) +
    geom_line() +
    geom_line(linetype = "dotted", data = df_lx_sub_0) +
    scale_colour_manual(
        values = cmap_periods,
        aesthetics = c("colour", "fill"),
        guide = guide_legend(
            title = "Survey life span period (years)",
            override.aes = list(alpha = 1)
        )
    ) +
    scale_x_continuous(
        breaks = c(0, 30, 60),
        labels = c("[0,5)", "[30,35)", "[60,65)")
    ) +
    scale_y_continuous(
        labels = ~ .x / 1000, 
        limits = c(0, 100000),
        oob = scales::oob_squish
    ) +
    labs(x = "Age (years)", y = "Life table persons (1,000s)", title = NULL) +
    facet_grid(vars(augmented), vars(race), scales = "free", switch = "y") +
    theme(
        legend.position = "bottom",
        legend.justification = "right",
        legend.key.height = unit(.1, "pt"),
        legend.text = element_text(family = font_family),
        legend.title = element_text(family = font_family),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10, family = font_family),
        panel.spacing = unit(1, "line"),
        strip.text = element_text(
            hjust = .5, size = 10, family = font_family
        ),
        strip.placement = "outisde"
    )

ggsave(
    here("plots", "parent-lx-by-sipp.png"), plt_lx_by_sipp_period, 
    device = "png", width = 9, height = 4, dpi = 400
)


### plot ex --------------------------------------------------------------------
plot_ex <- function(df) {
    df |>
        ggplot(aes(x = age, y = q500, colour = state, fill = state)) +
        theme_minimal() +
        geom_line(aes(y = q500), position = position_stack()) +
        geom_area(aes(y = q500), position = position_stack(), alpha = .5) +
        scale_colour_manual(
            values = cmap_states, 
            labels = labels_states,
            aesthetics = list("colour", "fill"),
            guide = guide_legend(
                title = NULL,
                override.aes = list(alpha = 1)
            )
        ) +
        scale_x_continuous(
            breaks = c(0, 30, 60),
            labels = c("[0,5)", "[30,35)", "[60,65)")
        ) +
        labs(x = "Age (years)", y = "Expected life remaining (years)") +
        facet_wrap(vars(race), scales = "free", nrow = 1) +
        theme(
            legend.position = "bottom",
            legend.justification = "right",
            legend.key.height = unit(.1, "pt"),
            legend.text = element_text(family = font_family),
            legend.title = element_text(family = font_family),
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 10, family = font_family),
            panel.spacing = unit(1, "line"),
            strip.text.y = element_blank(),
            strip.text.x = element_text(
                hjust = .5, size = 10, family = font_family
            )
        )
}
df_ex_selected <- df_ex |>
    filter(sipp_period == sipp_period_selected, !is.na(state), augmented) |>
    mutate(
        race = factor(
            race,
            levels = names(labels_races),
            labels = labels_races_abc
        )
    )
plt_ex <- plot_ex(df_ex_selected)
ggsave(
    here("plots", "parent-ex.png"), plt_ex, 
    device = "png", width = 9, height = 3, dpi = 400
)

df_mx |>
    filter(grepl("^m_a_", param), augmented) |>
    mutate(value = if_else(value > 0, value, NA)) |>
    summarise(
        .by = c(sipp_period, param, race),
        missing = sum(is.na(value)),
        total = n()
    ) |>
    View()


df_mx |>
    filter(grepl("^m_a_", param)) |>
    mutate(value = if_else(value > 0, value, NA)) |>
    summarise(
        .by = c(sipp_period),
        missing = sum(is.na(value)) / n()
    )
