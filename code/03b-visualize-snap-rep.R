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

plot_line <- function(df, my_theme, 
                      linewidth = 1,
                      linetype = "dotted",
                      shape = 20,
                      size = 2,
                      color = TRUE,
                      se = FALSE,
                      level = .05) {
    if (color) {
        plt <- df |>
            ggplot(aes(.data$x, .data$value, color = .data$race)) +
            my_theme +
            geom_line(linetype = linetype, linewidth = linewidth,
                      position = position_dodge(width = 2)) +
            geom_point(shape = shape, size = size,
                       position = position_dodge(width = 2))    
    } else {
        plt <- df |>
            ggplot(aes(.data$x, .data$value)) +
            my_theme +
            geom_line(linetype = linetype, linewidth = linewidth) +
            geom_point(shape = shape, size = size)
    }
    
    if (se) {
        plt <- plt +
            geom_linerange(
                aes(
                    .data$x, 
                    ymin = .data$value + qnorm(level / 2, sd = .data$se),
                    ymax = .data$value - qnorm(level / 2, sd = .data$se)
                ),
                linetype = "solid",
                linewidth = linewidth,
                position = position_dodge(width = 2)
            )
    }
    plt
}
get_legend <- function(plt) {
    tmp <- ggplot_gtable(ggplot_build(plt))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}
## Load data -------------------------------------------------------------------
us_mlt <- read_csv(here("data", "multistate-lt_snap_rep.csv")) |>
    mutate(
        quantity = str_remove(name, "_.+"),
        state_to = str_extract(name, "(?<=to_).+"),
        state_from = str_extract(name, "(?<=^[a-zA-Z]{2}_)[a-z]+_[a-z]+"),
        state_from = if_else(is.na(state_from), "total", state_from),
        state_in = ifelse(is.na(state_to), state_from, NA),
        state_from = ifelse(is.na(state_to), NA, state_from),
        value = ifelse(value == 0, NA, value)
    ) |>
    select(race, x, quantity, state_in, state_from, state_to, value)
    # filter(race %in% c("all", "hispanic", "non-hispanic white", "non-hispanic black"))
se_us_mlt <- read_csv(here("data", "se-multistate-lt_snap_rep.csv")) |>
    mutate(
        name = str_remove(name, "^se_"),
        quantity = str_remove(name, "_.+"),
        state_to = str_extract(name, "(?<=to_).+"),
        state_from = str_extract(name, "(?<=^[a-zA-Z]{2}_)[a-z]+_[a-z]+"),
        state_from = if_else(is.na(state_from), "total", state_from),
        state_in = ifelse(is.na(state_to), state_from, NA),
        state_from = ifelse(is.na(state_to), NA, state_from),
        value = ifelse(value == 0, NA, value)
    ) |>
    select(race, x, quantity, state_in, state_from, state_to, value)
df_ex <- us_mlt |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    filter(quantity == "ex", state_in != "total") |>
    select(-c(state_from, state_to))
df_lx <- us_mlt |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    filter(quantity == "lx", state_in != "total") |>
    select(-c(state_from, state_to))
df_pr <- us_mlt |>
    filter(quantity == "pr") |>
    left_join(
        se_us_mlt, 
        by = c("race", "x", "quantity", "state_in", "state_from", "state_to"),
        suffix = c("", "_se")
    ) |>
    rename(se = "value_se") |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+"))) |>
    select(-c(state_from, state_to))
df_mx <- us_mlt |>
    filter(quantity == "mx")  |>
    left_join(
        se_us_mlt, 
        by = c("race", "x", "quantity", "state_in", "state_from", "state_to"),
        suffix = c("", "_se")
    ) |>
    rename(se = "value_se") |>
    mutate(x = as.numeric(str_extract(x, "(?<=\\[)\\d+")))

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
colours <- c(
    lost_both = "#440154ff", 
    lost_mom = "#dce319ff",
    lost_dad = "#29a387ff", 
    lost_none = "#b3b3b3"
)
colours_race <- RColorBrewer::brewer.pal(8, "Dark2")[c(1,2,3,7,8)] |>
    setNames(
        c("hispanic",
          "non-hispanic black",
          "non-hispanic white",
          "non-hispanic asian",
          "all")
    )

### Lines-----------------------------------------------------------------------
legend_races <- (
    df_ex |>
        filter(state_in == "lost_both") |>
        ggplot(aes(.data$x, .data$value, color = .data$race)) +
        my_theme +
        geom_point(shape = 19) +
        scale_color_manual(
            name = NULL,
            values = colours_race,
            breaks = c("hispanic",
                       "non-hispanic asian",
                       "non-hispanic black",
                       "non-hispanic white",
                       "all"),
            labels = c("Hispanic",
                       "Non-Hispanic Aisan",
                       "Non-Hispanic black",
                       "Non-Hispanic White",
                       "All including others"),
            guide = guide_legend(override.aes = list(size = 1))
        )
    ) |>
    get_legend()

#### SIPP parent status proportions --------------------------------------------
plt_pr_line_none <- df_pr |>
    filter(state_in == "lost_none") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .5, 1),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Age", y = NULL,
         # y = expression(paste({}[5], italic(p)[italic(x)], "(1)")),
         caption = "(a) Lost neither")

plt_pr_line_dad <- df_pr |>
    filter(state_in == "lost_dad") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish, 
                       breaks = c(0, .25, .5),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, .5)) +
    labs(x = "Age", y = NULL,
         # y = expression(paste({}[5], italic(p)[italic(x)], "(3)")),
         caption = "(d) Lost father")
plt_pr_line_mom <- df_pr |>
    filter(state_in == "lost_mom") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish, 
                       breaks = c(0, .25, .5),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, .5)) +
    labs(x = "Age", y = NULL,
         # y = expression(paste({}[5], italic(p)[italic(x)], "(2)")),
         caption = "(c) Lost mother")
plt_pr_line_both <- df_pr |>
    filter(state_in == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish, 
                       breaks = c(0, .5, 1),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Age", y = NULL,
         # y = expression(paste({}[5], italic(p)[italic(x)], "(4)")),
         caption = "(b) Lost both")

g_pr_line_by_race <- grid.arrange(
    plt_pr_line_none, 
    plt_pr_line_mom, 
    plt_pr_line_dad,
    plt_pr_line_both,
    legend_races,
    heights = c(1, 1, .12),
    layout_matrix = rbind(
        c(1, 1, 4, 4),
        c(2, 2, 3, 3),
        c(NA, NA, 5, 5) 
    )
)
postscript(here("plots", "repwgt", "pr_line_by_race_snap.eps"), onefile = FALSE,
           family = "Times", width = 6, height = 4, horizontal = FALSE)
grid.draw(g_pr_line_by_race)
dev.off()
png(here("plots", "repwgt", "pr_line_by_race_snap.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_pr_line_by_race)
dev.off()

#### Transition rates ----------------------------------------------------------
plt_mx_line_mom_first <- df_mx |>
    filter(state_from == "lost_none", state_to == "lost_mom") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish, 
                       breaks = c(0, .05, 0.1, .15),
                       labels = c(0, 50, 100, 150)) +
    coord_cartesian(ylim = c(0, 0.15)) +
    labs(
        x = "Age", caption = "(a) Losing mother first",
        y = "(per 1 000)"
    )
plt_mx_line_dad_first <- df_mx |>
    filter(state_from == "lost_none", state_to == "lost_dad") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .1, 0.2, 0.3),
                       labels = c(0, 100, 200, 300)) +
    coord_cartesian(ylim = c(0, 0.45)) +
    labs(
        x = "Age", caption = "(b) Losing father first",
        y = "(per 1 000)"
    )
plt_mx_line_both <- df_mx |>
    filter(state_from == "lost_none", state_to == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .5, 1),
                       labels = c(0, 500, 1000)) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
        x = "Age", caption = "(c) Losing both at once",
        y = "(per 1 000)"
    )

plt_mx_line_dad_last <- df_mx |>
    filter(state_from == "lost_mom", state_to == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .5, 1),
                       labels = c(0, 500, 1000)) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
        x = "Age", caption = "(d) Losing father last",
        y = "(per 1 000)"
    )

plt_mx_line_mom_last <- df_mx |>
    filter(race != "all", 
           state_from == "lost_dad", state_to == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .5, 1),
                       labels = c(0, 500, 1000)) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
        x = "Age", caption = "(e) Losing mother last",
        y = "(per 1 000)"
    )

g_mx_line_by_race <- grid.arrange(
    plt_mx_line_mom_first, 
    plt_mx_line_dad_first,
    plt_mx_line_both,
    plt_mx_line_dad_last, 
    plt_mx_line_mom_last,
    legend_races,
    heights = c(1, 1, 1, 1, .12),
    layout_matrix = rbind(
        c(1, 1, 2, 2, NA, NA),
        c(1, 1, 2, 2, 3, 3),
        c(4, 4, 5, 5, 3, 3),
        c(4, 4, 5, 5, NA, NA),
        c(NA, NA, NA, 6, 6, 6) 
    )
)
postscript(here("plots", "repwgt", "mx_line_by_race.eps"), onefile = FALSE,
           family = "Times", width = 6, height = 4, horizontal = FALSE)
grid.draw(g_mx_line_by_race)
dev.off()
png(here("plots", "repwgt", "mx_line_by_race.png"),
    width = 8, height = 6, units = "in", res = 800)
grid.draw(g_mx_line_by_race)
dev.off()

