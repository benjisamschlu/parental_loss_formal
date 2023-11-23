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
## Multistate life table 
## 
## sex :                     male; female
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
## Checks ---------------------------------------------------------------------

rm(list = ls())

## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("tidyverse", "gridExtra", "grid", "here", "utils", "testthat")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Functions -------------------------------------------------------------------
plot_ex_in_state_area <- function(df, colours, my_theme,
                                  xlimits = c(0, 70),
                                  xbreaks = c(0, 20, 40, 60),
                                  ylimits = c(0, 90),
                                  ybreaks = c(0, 25, 50, 75),
                                  use_step = FALSE) {
    if (use_step) {
        df <- bind_rows(
            old = df,
            new = df |> 
                group_by(.data$state_in) |> 
                mutate(value = lag(.data$value)),
            .id = "source"
        ) |>
            filter(!is.na(.data$value)) |>
            arrange(.data$x, .data$source) |>
            filter(!(.data$source == "new" & .data$x == 0))
    }
    
    df |>
        mutate(
            state_in = factor(
                .data$state_in,
                levels = c("lost_both", "lost_dad", "lost_mom", "lost_none")
            )
        ) |>
        ggplot(aes(.data$x, .data$value)) +
        scale_fill_manual(
            breaks = c("lost_both", "lost_dad", "lost_mom", "lost_none"),
            labels = c("Lost both", "Lost father only",
                       "Lost mother only", "Lost neither"),
            values = colours,
            guide = guide_legend(
                title = NULL, 
                keywidth = unit(1, "lines"), 
                keyheight = unit(.5, "lines")
            )
        ) +
        scale_x_continuous(breaks = xbreaks) +
        scale_y_continuous(breaks = ybreaks) +
        coord_cartesian(xlim = xlimits, ylim = ylimits, clip = "off") +
        geom_area(aes(fill = .data$state_in)) +
        my_theme
}
get_legend <- function(plt) {
    tmp <- ggplot_gtable(ggplot_build(plt))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

plot_line_comp <- function(df, my_theme, linetype = "dotted", shape = 20, 
                           linewidth = .2, size = 1) {
    df |> 
        ggplot(aes(x = x, y = value, colour = source)) +
        my_theme +
        geom_line(linetype = linetype, linewidth = linewidth) +
        geom_point(shape = shape, size = size)
}


## Load data -------------------------------------------------------------------
us_mlt <- read_csv(here("data", "multistate-lt.csv")) |>
    pivot_longer(-c(race, sex, x)) |>
    mutate(
        quantity = str_remove(name, "_.+"),
        state_to = str_extract(name, "(?<=to_).+"),
        state_from = str_extract(name, "(?<=^[a-zA-Z]{2}_)[a-z]+_[a-z]+"),
        state_from = if_else(is.na(state_from), "total", state_from),
        state_in = ifelse(is.na(state_to), state_from, NA),
        state_from = ifelse(is.na(state_to), NA, state_from)
    ) |>
    select(sex, race, x, quantity, state_in, state_from, state_to, value)
df_ex <- us_mlt |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    filter(quantity == "ex", state_in != "total") |>
    select(-c(state_from, state_to))
df_lx <- us_mlt |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    filter(quantity == "lx", state_in != "total") |>
    select(-c(state_from, state_to))
df_pr <- us_mlt |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    filter(quantity == "pr") |>
    select(-c(state_from, state_to)) |>
    mutate(value = ifelse(value == 0, NA, value))
df_mx <- us_mlt |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    filter(quantity == "mx") |>
    mutate(value = ifelse(value == 0, NA, value))

us_mlt_snap <- read_csv(here("data", "multistate-lt_snap.csv")) |>
    pivot_longer(-c(race, sex, x)) |>
    mutate(
        quantity = str_remove(name, "_.+"),
        state_to = str_extract(name, "(?<=to_).+"),
        state_from = str_extract(name, "(?<=^[a-zA-Z]{2}_)[a-z]+_[a-z]+"),
        state_from = if_else(is.na(state_from), "total", state_from),
        state_in = ifelse(is.na(state_to), state_from, NA),
        state_from = ifelse(is.na(state_to), NA, state_from)
    ) |>
    select(sex, race, x, quantity, state_in, state_from, state_to, value) 
df_ex_snap <- us_mlt_snap |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    filter(quantity == "ex", state_in != "total") |>
    select(-c(state_from, state_to))
df_lx_snap <- us_mlt_snap |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    filter(quantity == "lx", state_in != "total") |>
    select(-c(state_from, state_to))
df_pr_snap <- us_mlt_snap |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    filter(quantity == "pr") |>
    select(-c(state_from, state_to)) |>
    mutate(value = ifelse(value == 0, NA, value))
df_mx_snap <- us_mlt_snap|>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    filter(quantity == "mx") |>
    mutate(value = ifelse(value == 0, NA, value))

# combine
df_pr_comp <- bind_rows(
    df_pr |> mutate(source = "lifetime"),
    df_pr_snap |> mutate(source = "snapshot")
)
df_mx_comp <- bind_rows(
    df_mx |> mutate(source = "lifetime"),
    df_mx_snap |> mutate(source = "snapshot")
)
df_lx_comp <- bind_rows(
    df_lx |> mutate(source = "lifetime"),
    df_lx_snap |> mutate(source = "snapshot")
)
df_ex_comp <- bind_rows(
    df_ex |> mutate(source = "lifetime"),
    df_ex_snap |> mutate(source = "snapshot")
)
## Visualize -------------------------------------------------------------------
bg_color <- "#ffffff"
text_color <- "black"
line_color <- "darkgrey"
font_family <- "Times"
my_theme <- theme(
    plot.caption = element_text(
        colour = text_color, size = 10, family = font_family, hjust = .5,
        margin = margin(10, 5.5, 5.5, 0)),
    plot.caption.position = "plot",
    legend.position = "top",
    legend.justification = "right",
    legend.text = element_text(
        colour = text_color, size = 8, family = font_family
    ),
    legend.key = element_blank(),
    rect = element_rect(fill = bg_color, colour = bg_color),
    panel.background = element_rect(fill = bg_color, colour = bg_color),
    panel.grid.major.y = element_line(colour = line_color, linewidth = .25),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.text = element_text(
        colour = text_color, size = 8, family = font_family
    ),
    axis.title = element_text(
        colour = text_color, size = 8, family = font_family,
        margin = margin(0, 0, 0, .2, unit = "in")
    ),
    axis.line.x = element_line(colour = line_color),
    axis.line.y = element_blank(),
    axis.ticks = element_blank()
)
my_theme_no_legend <- my_theme +
    theme(legend.position = "none")
legend_sources <- (
    df_pr_comp |> 
        ggplot(aes(x = x, y = value, colour = source)) +
        my_theme +
        geom_point() +
        scale_color_brewer(name = "", palette = "Dark2",
                           labels = c("Lifetime until 2021", "Snapshot of 2021"))
) |> get_legend()

#### SIPP parent status proportions --------------------------------------------
plot_pr_comparisons <- function(df, race_f, label) {
    df <- df |>
        filter(
            .data$race == race_f, 
            .data$sex == "all"
        )
    plt_none <- df |> 
        filter(.data$state_in == "lost_none") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, 1), 
                           breaks = c(0, .5, 1),
                           labels = scales::percent) +
        labs(x = "Age", y = NULL, caption = "Lost none")
    plt_mom <- df |>
        filter(state_in == "lost_mom") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, .11), 
                           breaks = c(0, .05, .1),
                           labels = scales::percent) +
        labs(x = "Age", y = NULL, caption = "Lost mother only")
    plt_dad <- df |>
        filter(state_in == "lost_dad") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, .4), 
                           breaks = c(0, .2, .4),
                           labels = scales::percent) +
        labs(x = "Age", y = NULL, caption = "Lost father only")
    plt_both <- df |>
        filter(state_in == "lost_both") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, 1), 
                           breaks = c(0, .5, 1),
                           labels = scales::percent) +
        labs(x = "Age", y = NULL, caption = "Lost both")
    
    grid.arrange(
        plt_none, plt_mom, plt_dad, plt_both,
        legend_sources,
        heights = c(.12, 1, 1),
        layout_matrix = rbind(
            c(NA, NA, 5, 5),
            c(1, 1, 4, 4),
            c(2, 2, 3, 3)
        ),
        top = label
    )
}
# plot
g_pr_comp_all <- plot_pr_comparisons(
    df_pr_comp, "all", 
    grid.text(expression("Proportions by parent mortality states,"~{}[italic(n)]~italic(p)[italic(x)]~"- All races")))
g_pr_comp_hispanic <- plot_pr_comparisons(
    df_pr_comp, "hispanic", 
    grid.text(expression("Proportions by parent mortality states,"~{}[italic(n)]~italic(p)[italic(x)]~"- Hispanic")))
g_pr_comp_black <- plot_pr_comparisons(
    df_pr_comp, "non-hispanic black", 
    grid.text(expression("Proportions by parent mortality states,"~{}[italic(n)]~italic(p)[italic(x)]~"- Non-Hispanic black")))
g_pr_comp_white <- plot_pr_comparisons(
    df_pr_comp, "non-hispanic white", 
    grid.text(expression("Proportions by parent mortality states,"~{}[italic(n)]~italic(p)[italic(x)]~"- Non-Hispanic white")))
g_pr_comp_asian <- plot_pr_comparisons(
    df_pr_comp, "non-hispanic asian", 
    grid.text(expression("Proportions by parent mortality states,"~{}[italic(n)]~italic(p)[italic(x)]~"- Non-Hispanic Asian")))
# save
png(here("plots", "compare", "pr_all.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_pr_comp_all)
dev.off()
png(here("plots", "compare", "pr_hispanic.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_pr_comp_hispanic)
dev.off()
png(here("plots", "compare", "pr_black.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_pr_comp_black)
dev.off()
png(here("plots", "compare", "pr_white.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_pr_comp_white)
dev.off()
png(here("plots", "compare", "pr_asian.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_pr_comp_asian)
dev.off()


# postscript(here("plots", "lifetime", "pr_line_by_race.eps"), onefile = FALSE,
#            family = "Times", width = 6, height = 4, horizontal = FALSE)
# grid.draw(g_pr_line_by_race)
# dev.off()


#### SIPP parent mortality rates  ----------------------------------------------
plot_mx_comparisons <- function(df, race_f, label) {
    df <- df |>
        filter(
            .data$race == race_f, 
            .data$sex == "all"
        )
    plt_none_to_mom <- df |> 
        filter(.data$state_from == "lost_none", state_to == "lost_mom") |>
        plot_line_comp(my_theme_no_legend) +
        scale_y_continuous(limits = c(0, .12), 
                           breaks = c(0, .05, .1),
                           labels = c(0, 50, 100),
                           oob = scales::squish_infinite) +
        scale_color_brewer(name = "", palette = "Dark2") +
        labs(x = "Age", y = "(per 1 000)", caption = "Lost mother first")
    plt_none_to_dad <- df |> 
        filter(.data$state_from == "lost_none", state_to == "lost_dad") |>
        plot_line_comp(my_theme_no_legend) +
        scale_y_continuous(limits = c(0, .12), 
                           breaks = c(0, .05, .1),
                           labels = c(0, 50, 100),
                           oob = scales::squish_infinite) +
        scale_color_brewer(name = "", palette = "Dark2") +
        labs(x = "Age", y = "(per 1 000)", caption = "Lost father first")
    plt_none_to_both <- df |> 
        filter(.data$state_from == "lost_none", state_to == "lost_both") |>
        plot_line_comp(my_theme_no_legend) +
        scale_y_continuous(limits = c(0, .035),
                           breaks = c(0, .015, .03),
                           labels = c(0, 15, 30),
                           oob = scales::squish_infinite) +
        scale_color_brewer(name = "", palette = "Dark2") +
        labs(x = "Age", y = "(per 1 000)", caption = "Lost both at once")
    plt_mom_to_both <- df |> 
        filter(.data$state_from == "lost_mom", state_to == "lost_both") |>
        plot_line_comp(my_theme_no_legend) +
        scale_y_continuous(limits = c(0, .35),
                           breaks = c(0, .15, .3),
                           labels = c(0, 150, 300),
                           oob = scales::squish_infinite) +
        scale_color_brewer(name = "", palette = "Dark2") +
        labs(x = "Age", y = "(per 1 000)", caption = "Lost dad last")
    plt_dad_to_both <- df |> 
        filter(.data$state_from == "lost_dad", state_to == "lost_both") |>
        plot_line_comp(my_theme_no_legend) +
        scale_y_continuous(limits = c(0, .35),
                           breaks = c(0, .15, .3),
                           labels = c(0, 150, 300),
                           oob = scales::squish_infinite) +
        scale_color_brewer(name = "", palette = "Dark2") +
        labs(x = "Age", y = "(per 1 000)", caption = "Lost mom last")
    
    grid.arrange(
        plt_none_to_mom, plt_none_to_dad, plt_none_to_both,
        plt_mom_to_both, plt_dad_to_both,
        legend_sources,
        heights = c(.12, 1, 1, 1, 1),
        
        layout_matrix = rbind(
            c(NA, NA, NA, 6, 6, 6),
            c(1, 1, NA, NA, 4, 4),
            c(1, 1, 3, 3, 4, 4),
            c(2, 2, 3, 3, 5, 5),
            c(2, 2, NA, NA, 5, 5)
        ),
        top = label
    )
}
# plot
g_mx_comp_all <- plot_mx_comparisons(
    df_mx_comp, "all", 
    grid.text(expression("Parent mortality rates,"~{}[italic(n)]~italic(m)[italic(x)]~"- All races")))
g_mx_comp_hispanic <- plot_mx_comparisons(
    df_mx_comp, "hispanic", 
    grid.text(expression("Parent mortality rates,"~{}[italic(n)]~italic(m)[italic(x)]~"- Hispanic")))
g_mx_comp_black <- plot_mx_comparisons(
    df_mx_comp, "non-hispanic black",
    grid.text(expression("Parent mortality rates,"~{}[italic(n)]~italic(m)[italic(x)]~"- Non-Hispanic black")))
g_mx_comp_white <- plot_mx_comparisons(
    df_mx_comp, "non-hispanic white", 
    grid.text(expression("Parent mortality rates,"~{}[italic(n)]~italic(m)[italic(x)]~"- Non-Hispanic white")))
g_mx_comp_asian <- plot_mx_comparisons(
    df_mx_comp, "non-hispanic asian", 
    grid.text(expression("Parent mortality rates,"~{}[italic(n)]~italic(m)[italic(x)]~"- Non-Hispanic Asian")))
# save
png(here("plots", "compare", "mx_all.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_mx_comp_all)
dev.off()
png(here("plots", "compare", "mx_hispanic.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_mx_comp_hispanic)
dev.off()
png(here("plots", "compare", "mx_black.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_mx_comp_black)
dev.off()
png(here("plots", "compare", "mx_white.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_mx_comp_white)
dev.off()
png(here("plots", "compare", "mx_asian.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_mx_comp_asian)
dev.off()


#### Survivor curves -----------------------------------------------------------
plot_lx_comparisons <- function(df, race_f, label) {
    df <- df |>
        filter(
            .data$race == race_f, 
            .data$sex == "all"
        )
    plt_none <- df |> 
        filter(.data$state_in == "lost_none") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, 1e5),
                           breaks = c(0, 5e4, 1e5),
                           labels = c(0, 50, 100),
                           oob = scales::squish_infinite) +
        labs(x = "Age", y = "(1 000s)", caption = "Lost none")
    plt_mom <- df |>
        filter(state_in == "lost_mom") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, 1.2e4),
                           breaks = c(0, 5e3, 1e4),
                           labels = c(0, 5, 10),
                           oob = scales::squish_infinite) +
        labs(x = "Age", y = "(1 000s)", caption = "Lost mother only")
    plt_dad <- df |>
        filter(state_in == "lost_dad") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, 3e4),
                           breaks = c(0, 1.5e4, 3e4),
                           labels = c(0, 15, 30),
                           oob = scales::squish_infinite) +
        labs(x = "Age", y = "(1 000s)", caption = "Lost father only")
    plt_both <- df |>
        filter(state_in == "lost_both") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, 5.1e4),
                           breaks = c(0, 2.5e4, 5e4),
                           labels = c(0, 25, 50),
                           oob = scales::squish_infinite) +
        labs(x = "Age", y = "(1 000s)", caption = "Lost both")
    
    grid.arrange(
        plt_none, plt_mom, plt_dad, plt_both,
        legend_sources,
        heights = c(.12, 1, 1),
        layout_matrix = rbind(
            c(NA, NA, 5, 5),
            c(1, 1, 4, 4),
            c(2, 2, 3, 3)
        ),
        top = label
    )
}
# plot
g_lx_comp_all <- plot_lx_comparisons(
    df_lx_comp, "all", 
    grid.text(expression("Survival curve,"~italic(l)[italic(x)]~"- All races")))
g_lx_comp_hispanic <- plot_lx_comparisons(
    df_lx_comp, "hispanic", 
    grid.text(expression("Survival curve,"~italic(l)[italic(x)]~"- Hispanic")))
g_lx_comp_black <- plot_lx_comparisons(
    df_lx_comp, "non-hispanic black", 
    grid.text(expression("Survival curve,"~italic(l)[italic(x)]~"- Non-Hispanic black")))
g_lx_comp_white <- plot_lx_comparisons(
    df_lx_comp, "non-hispanic white", 
    grid.text(expression("Survival curve,"~italic(l)[italic(x)]~"- Non-Hispanic white")))
g_lx_comp_asian <- plot_lx_comparisons(
    df_lx_comp, "non-hispanic asian", 
    grid.text(expression("Survival curve,"~italic(l)[italic(x)]~"- Non-Hispanic Asian")))
# save
png(here("plots", "compare", "lx_all.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_lx_comp_all)
dev.off()
png(here("plots", "compare", "lx_hispanic.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_lx_comp_hispanic)
dev.off()
png(here("plots", "compare", "lx_black.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_lx_comp_black)
dev.off()
png(here("plots", "compare", "lx_white.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_lx_comp_white)
dev.off()
png(here("plots", "compare", "lx_asian.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_lx_comp_asian)
dev.off()

#### Life expectancy curves ----------------------------------------------------
plot_ex_comparisons <- function(df, race_f, label) {
    df <- df |>
        filter(
            .data$race == race_f, 
            .data$sex == "all"
        )
    plt_none <- df |> 
        filter(.data$state_in == "lost_none") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, 50),
                           breaks = c(0, 25, 50),
                           oob = scales::squish_infinite) +
        labs(x = "Age", y = "(years)", caption = "Lost none")
    plt_mom <- df |>
        filter(state_in == "lost_mom") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, 4),
                           breaks = c(0, 2, 4),
                           oob = scales::squish_infinite) +
        labs(x = "Age", y = "(years)", caption = "Lost mother only")
    plt_dad <- df |>
        filter(state_in == "lost_dad") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, 12),
                           breaks = c(0, 5, 10),
                           oob = scales::squish_infinite) +
        labs(x = "Age", y = "(years)", caption = "Lost father only")
    plt_both <- df |>
        filter(state_in == "lost_both") |>
        plot_line_comp(my_theme_no_legend) +
        scale_color_brewer(name = "", palette = "Dark2") +
        scale_y_continuous(limits = c(0, 25),
                           breaks = c(0, 10, 20),
                           oob = scales::squish_infinite) +
        labs(x = "Age", y = "(years)", caption = "Lost both")
    
    grid.arrange(
        plt_none, plt_mom, plt_dad, plt_both,
        legend_sources,
        heights = c(.12, 1, 1),
        layout_matrix = rbind(
            c(NA, NA, 5, 5),
            c(1, 1, 4, 4),
            c(2, 2, 3, 3)
        ),
        top = label
    )
}
# plot
g_ex_comp_all <- plot_ex_comparisons(
    df_ex_comp, "all", 
    grid.text(expression("Life expectancy,"~italic(e)[italic(x)]~"- All races")))
g_ex_comp_hispanic <- plot_ex_comparisons(
    df_ex_comp, "hispanic", 
    grid.text(expression("Life expectancy,"~italic(e)[italic(x)]~"- Hispanic")))
g_ex_comp_black <- plot_ex_comparisons(
    df_ex_comp, "non-hispanic black", 
    grid.text(expression("Life expectancy,"~italic(e)[italic(x)]~"- Non-hispanic black")))
g_ex_comp_white <- plot_ex_comparisons(
    df_ex_comp, "non-hispanic white", 
    grid.text(expression("Life expectancy,"~italic(e)[italic(x)]~"- Non-hispanic white")))
g_ex_comp_asian <- plot_ex_comparisons(
    df_ex_comp, "non-hispanic asian", 
    grid.text(expression("Life expectancy,"~italic(e)[italic(x)]~"- Non-hispanic Asian")))
# save
png(here("plots", "compare", "ex_all.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_comp_all)
dev.off()
png(here("plots", "compare", "ex_hispanic.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_comp_hispanic)
dev.off()
png(here("plots", "compare", "ex_black.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_comp_black)
dev.off()
png(here("plots", "compare", "ex_white.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_comp_white)
dev.off()
png(here("plots", "compare", "ex_asian.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_comp_asian)
dev.off()

#### Life expectancy areas -----------------------------------------------------

df <- df_ex |> filter(race == "all", sex == "all")
colours <- c(
    lost_both = "#440154ff", 
    lost_mom = "#dce319ff",
    lost_dad = "#29a387ff", 
    lost_none = "#b3b3b3"
)
legend_parents <- plot_ex_in_state_area(df_ex, colours, my_theme) |> 
    get_legend()
plot_ex_comparisons_area <- function(df, colours, race_f, label) {
    df <- df |>
        filter(
            .data$race == race_f, 
            .data$sex == "all"
        )
    plt_lifetime <- df |> 
        filter(.data$source == "lifetime") |>
        plot_ex_in_state_area(colours, my_theme_no_legend) +
        labs(x = "Age", y = "(years)", caption = "Using lifetime until 2021")
    plt_snapshot <- df |>
        filter(source == "snapshot") |>
        plot_ex_in_state_area(colours, my_theme_no_legend) +
        labs(x = "Age", y = "(years)", caption = "Using snapshot of 2021")
    
    grid.arrange(
        plt_lifetime, plt_snapshot, 
        legend_parents,
        heights = c(.05, 1),
        layout_matrix = rbind(
            c(NA, NA, 3, 3),
            c(1, 1, 2, 2)
        ),
        top = label
    )
}

g_ex_comp_area_all <- plot_ex_comparisons_area(
    df_ex_comp, colours, "all", 
    grid.text(expression("Life expectancy,"~italic(e)[italic(x)]~"- All races")))
g_ex_comp_area_hispanic <- plot_ex_comparisons_area(
    df_ex_comp, colours, "hispanic", 
    grid.text(expression("Life expectancy,"~italic(e)[italic(x)]~"- Hispanic")))
g_ex_comp_area_black <- plot_ex_comparisons_area(
    df_ex_comp, colours, "non-hispanic black", 
    grid.text(expression("Life expectancy,"~italic(e)[italic(x)]~"- Non-Hispanic black")))
g_ex_comp_area_white <- plot_ex_comparisons_area(
    df_ex_comp, colours, "non-hispanic white", 
    grid.text(expression("Life expectancy,"~italic(e)[italic(x)]~"- Non-Hispanic white")))
g_ex_comp_area_asian <- plot_ex_comparisons_area(
    df_ex_comp, colours, "non-hispanic asian", 
    grid.text(expression("Life expectancy,"~italic(e)[italic(x)]~"- Non-Hispanic Asian")))
# save
png(here("plots", "compare", "ex_area_all.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_comp_area_all)
dev.off()
png(here("plots", "compare", "ex_area_hispanic.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_comp_area_hispanic)
dev.off()
png(here("plots", "compare", "ex_area_black.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_comp_area_black)
dev.off()
png(here("plots", "compare", "ex_area_white.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_comp_area_white)
dev.off()
png(here("plots", "compare", "ex_area_asian.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_comp_area_asian)
dev.off()
