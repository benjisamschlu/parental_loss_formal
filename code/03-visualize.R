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
##  Checks ---------------------------------------------------------------------

rm(list = ls())

## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("tidyverse", "gridExtra", "here", "utils", "testthat")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Functions -------------------------------------------------------------------
plot_ex_in_state_area <- function(df, colours, my_theme,
                                  xlimits = c(0, 70),
                                  xbreaks = c(0, 25, 50, 75),
                                  ylimits = c(0, 80),
                                  ybreaks = c(0, 20, 40)) {
    # df_step <- bind_rows(
    #     old = df,
    #     new = df |> group_by(.data$name) |> mutate(value = lag(.data$value)),
    #     .id = "source"
    # ) |>
    #     filter(!is.na(.data$value)) |>
    #     arrange(.data$x, .data$source) |>
    #     filter(!(.data$source == "new" & .data$x == 0))
    
    # df_step |>
    df |>
        mutate(
            in_state = factor(
                .data$in_state, levels = c("none", "dad", "mom", "both")
            )
        ) |>
        ggplot(aes(.data$x, .data$value)) +
        scale_fill_manual(
            breaks = c("both", "mom", "dad", "none"),
            labels = c("Lost both", "Lost mom only", "Lost dad only",
                       "Lost neither"),
            values = colours,
            guide = guide_legend(
                title = NULL, 
                keywidth = unit(1, "lines"), 
                keyheight = unit(.5, "lines")
            )
        ) +
        coord_cartesian(xlim = xlimits, ylim = ylimits, clip = "off") +
        geom_area(aes(fill = .data$in_state)) +
        my_theme
}

plot_line <- function(df, colours, my_theme, 
                      xlimits = c(0, 70),
                      xbreaks = c(0, 25, 50, 75),
                      ylimits = c(0, 50),
                      ybreaks = c(0, 20, 40),
                      linewidth = 1) {
    df |>
        ggplot(aes(.data$x, .data$value)) +
        scale_color_manual(
            name = NULL,
            breaks = c("both", "mom", "dad", "none"),
            labels = c("Lost both", "Lost mom only", "Lost dad only",
                       "Lost neither"),
            values = colours
        ) +
        coord_cartesian(xlim = xlimits, ylim = ylimits, clip = "off") +
        geom_line(aes(color = .data$curr_state)) +
        my_theme
    
}
get_legend <- function(plt) {
    tmp <- ggplot_gtable(ggplot_build(plt))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}
## Load data -------------------------------------------------------------------
us_mlt <- read_csv(here("data", "multistate-lt.csv"))
us_ex <- us_mlt |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    select(race, sex, x, ex_lost_none:ex_lost_both) |>
    pivot_longer(-c(race, sex, x)) |>
    mutate(in_state = str_remove(name, "ex_lost_"))
# us_ex_from_i <- us_mlt |>
#     mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
#     select(race, sex, x, dx_lost_mom_to_dead:dx_lost_none_to_dead) |>
#     pivot_longer(-c(race, sex, x)) |>
#     mutate(
#         curr_state = str_remove_all(str_extract(name, "lost_.+_in"), "_in"),
#         in_state = str_remove_all(str_extract(name, "in_lost_.+$"), "in_")
#     )

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
        colour = text_color, size = 10, family = font_family,
        margin = margin(0, 0, 0, .2, unit = "in")
    ),
    axis.line.x = element_line(colour = line_color),
    axis.line.y = element_blank(),
    axis.ticks = element_blank()
)
my_theme_no_legend <- my_theme +
    theme(legend.position = "none")
colours <- c(
    both = "#440154ff", mom = "#dce319ff", dad = "#29a387ff", none = "#b3b3b3"
)

df_ex <- us_ex

### Legend ---------------------------------------------------------------------
legend_parents <- plot_ex_in_state_area(df_ex, colours, my_theme) |> get_legend()

### Stacked areas --------------------------------------------------------------
plt_area_all_all <- df_ex |>
    filter(race == "all", sex == "all") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "Life expectancy", caption = "(a) Both sexes")

plt_area_all_female <- df_ex |>
    filter(race == "all", sex == "female") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "Life expectancy", caption = "(b) Female")

plt_area_all_male <- df_ex |>
    filter(race == "all", sex == "male") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "Life expectancy", caption = "(c) Male")

# postscript(here("plots", "ex_area_by_sex.eps"), onefile = FALSE,
#            family = "Times", width = 6, height = 3, horizontal = FALSE)
png(here("plots", "ex_area_by_sex.png"), 
    width = 6, heigh = 3, units = "in", res = 800)
grid.arrange(
    plt_area_all_all, plt_area_all_female, plt_area_all_male, legend_parents,
    heights = c(.06, 1),
    layout_matrix = rbind(
        c(NA, 4, NA), c(1, 2, 3)
    )
)
dev.off()

plt_area_nhwhite_all <- df_ex |>
    filter(race == "non-hispanic white", sex == "all") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "Life expectancy", caption = "(a) Non-hispanic white")
plt_area_nhblack_all <- df_ex |>
    filter(race == "non-hispanic black", sex == "all") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "Life expectancy", caption = "(b) Non-hispanic black")
plt_area_hispanic_all <- df_ex |>
    filter(race == "hispanic", sex == "all") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "Life expectancy", caption = "(c) Hispanic")
plt_area_nhasian_all <- df_ex |>
    filter(race == "non-hispanic asian", sex == "all") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "Life expectancy", caption = "(d) Non-hispanic Asian")
# postscript(here("plots", "ex_area_by_race.eps"), onefile = FALSE,
#            family = "Times", width = 6, height = 5, horizontal = FALSE)
png(here("plots", "ex_area_by_race.png"), 
    width = 6, heigh = 5, units = "in", res = 800)
grid.arrange(
    plt_area_nhwhite_all, plt_area_nhblack_all, 
    plt_area_hispanic_all, plt_area_nhasian_all, 
    legend_parents,
    heights = c(.12, 1, 1),
    layout_matrix = rbind(
        c(NA, 5, 5, NA), 
        c(1, 1, 2, 2),
        c(3, 3, 4, 4)
    )
)
dev.off()

### Lines-----------------------------------------------------------------------
# plt_line_all_all <- df |>
#     filter(race == "all", sex == "all") |>
#     plot_line(colours, my_theme_no_legend) +
#     labs(x = "Age", y = "Life expectancy", caption = "(a) Both sexes")
# plt_line_all_female <- df |>
#     filter(race == "all", sex == "female") |>
#     plot_line(colours, my_theme_no_legend) +
#     labs(x = "Age", y = "Life expectancy", caption = "(b) Female")
# plt_line_all_male <- df |>
#     filter(race == "all", sex == "male") |>
#     plot_line(colours, my_theme_no_legend) +
#     labs(x = "Age", y = "Life expectancy", caption = "(c) Male")
# 
# # postscript(here("plots", "ex_line_all_all.eps"), onefile = FALSE,
# #            family = "Times", width = 6, height = 3, horizontal = FALSE)
# png(here("plots", "ex_line_all_all.png"), 
#     width = 6, heigh = 3, units = "in", res = 800)
# grid.arrange(
#     plt_line_all_all, plt_line_all_female, plt_line_all_male, legend_parents,
#     heights = c(.06, 1),
#     layout_matrix = rbind(
#         c(NA, 4, NA), c(1, 2, 3)
#     )
# )
# dev.off()

