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
    plt <- df |>
        ggplot(aes(.data$x, .data$value, color = .data$race)) +
        my_theme
    if (color) {
        if (se) {
            plt <- plt +
                geom_line(linetype = linetype, linewidth = linewidth,
                          position = position_dodge(width = 2)) +
                geom_point(shape = shape, size = size,
                           position = position_dodge(width = 2))    
        } else {
            plt <- plt +
                geom_line(linetype = linetype, linewidth = linewidth) +
                geom_point(shape = shape, size = size)    
        }
        
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
us_mlt <- read_csv(here("data", "multistate-lt-life.csv")) |>
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
se_us_mlt <- read_csv(here("data", "se-multistate-lt-life.csv")) |>
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

### Stacked areas --------------------------------------------------------------
legend_parents <- plot_ex_in_state_area(df_ex, colours, my_theme) |> 
    get_legend()
plt_ex_area_hispanic_all <- df_ex |>
    filter(race == "hispanic") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "(years)",
         # y = expression(paste(italic(e)[italic(x)], "(", italic(i), "), (years)")),
         caption = "(a) Hispanic")
plt_ex_area_nhasian_all <- df_ex |>
    filter(race == "non-hispanic asian") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "(years)",
         # y = expression(paste(italic(e)[italic(x)], "(", italic(i), "), (years)")),
         caption = "(b) Non-Hispanic Asian")
plt_ex_area_nhblack_all <- df_ex |>
    filter(race == "non-hispanic black") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "(years)",
         # y = expression(paste(italic(e)[italic(x)], "(", italic(i), "), (years)")),
         caption = "(c) Non-Hispanic black")
plt_ex_area_nhwhite_all <- df_ex |>
    filter(race == "non-hispanic white") |>
    plot_ex_in_state_area(colours, my_theme_no_legend) +
    labs(x = "Age", y = "(years)",
         # y = expression(paste(italic(e)[italic(x)], "(", italic(i), "), (years)")),
         caption = "(d) Non-Hispanic white")

g_ex_area_by_race <- grid.arrange(
    plt_ex_area_hispanic_all, plt_ex_area_nhasian_all, 
    plt_ex_area_nhblack_all, plt_ex_area_nhwhite_all, 
    legend_parents,
    heights = c(1, 1, .12),
    layout_matrix = rbind(
        c(1, 1, 2, 2),
        c(3, 3, 4, 4),
        c(NA, NA, 5, 5) 
    )
)
postscript(here("plots", "lifetime", "ex_area_by_race.eps"), onefile = FALSE,
           family = "Times", width = 6, height = 4, horizontal = FALSE)
grid.draw(g_ex_area_by_race)
dev.off()
png(here("plots", "lifetime", "ex_area_by_race.png"),
    width = 6, height = 5, units = "in", res = 800)
grid.draw(g_ex_area_by_race)
dev.off()

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
                       "non-hispanic white"),
            labels = c("Hispanic",
                       "Non-Hispanic Aisan",
                       "Non-Hispanic black",
                       "Non-Hispanic white"),
            guide = guide_legend(override.aes = list(size = 1))
        )
) |>
    get_legend()
legend_races_and_all <- (
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
                       "Non-Hispanic white",
                       "All including others"),
            guide = guide_legend(override.aes = list(size = 1))
        )
) |>
    get_legend()

#### ex parent status proportions --------------------------------------------
df_ex_pr <- df_ex |>
    filter(race != "all") |>
    reframe(value = value / sum(value),
            state_in = state_in,
            race = race,
            .by = c(race, x))
plt_ex_line_none <- df_ex_pr |>
    filter(state_in == "lost_none") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(limits = c(0, 1), 
                       breaks = c(0, .5, 1),
                       labels = scales::percent) +
    labs(x = "Age", y = NULL,
         # y = expression(paste(
         #     italic(e)[italic(x)], "(1)/", italic(e)[italic(x)]
         #     )),
         caption = "(a) Lost neither")

plt_ex_line_both <- df_ex_pr |>
    filter(state_in == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(limits = c(0, 1), 
                       breaks = c(0, .5, 1),
                       labels = scales::percent) +
    labs(x = "Age", y = NULL,
         # y = expression(paste(
         #     italic(e)[italic(x)], "(4)/", italic(e)[italic(x)]
         # )),
         caption = "(b) Lost both")

plt_ex_line_mom <- df_ex_pr |>
    filter(state_in == "lost_mom") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(limits = c(0, .25),
                       breaks = c(0, .1, .2),
                       labels = scales::percent) +
    labs(x = "Age", y = NULL,
         # y = expression(paste(
         #     italic(e)[italic(x)], "(2)/", italic(e)[italic(x)]
         # )),
         caption = "(c) Lost mother")

plt_ex_line_dad <- df_ex_pr |>
    filter(state_in == "lost_dad") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(limits = c(0, .25),
                       breaks = c(0, .1, .2),
                       labels = scales::percent) +
    labs(x = "Age", y = NULL,
         # y = expression(paste(
         #     italic(e)[italic(x)], "(3)/", italic(e)[italic(x)]
         # )),
         caption = "(d) Lost father")

g_ex_line_by_race <- grid.arrange(
    plt_ex_line_none, 
    plt_ex_line_mom, 
    plt_ex_line_dad,
    plt_ex_line_both,
    legend_races,
    heights = c(1, 1, .12),
    layout_matrix = rbind(
        c(1, 1, 4, 4),
        c(2, 2, 3, 3),
        c(NA, NA, 5, 5) 
    )
)
postscript(here("plots", "lifetime", "ex_line_by_race.eps"), onefile = FALSE,
           family = "Times", width = 6, height = 4, horizontal = FALSE)
grid.draw(g_ex_line_by_race)
dev.off()
png(here("plots", "lifetime", "ex_line_by_race.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_ex_line_by_race)
dev.off()

#### Survivor function ---------------------------------------------------------
plt_lx_line_none <- df_lx |>
    filter(race != "all", state_in == "lost_none") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(limits = c(0, 1e5), breaks = c(0, 50000, 1e5), 
                       labels = c(0, 50, 100)) +
    labs(
        x = "Age", caption = "(a) Lost neither",
        y = "(1 000s)"
        # y = expression(paste(italic(l)[italic(x)], "(1) (1 000s)"))
    )
plt_lx_line_dad <- df_lx |>
    filter(race != "all", state_in == "lost_dad") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(limits = c(0, 4e4), breaks = c(0, 2e4, 4e4), 
                       labels = c(0, 20, 40)) +
    labs(
        x = "Age", caption = "(d) Lost father",
        y = "(1 000s)"
        # y = expression(paste(italic(l)[italic(x)], "(3) (1 000s)"))
    )
plt_lx_line_mom <- df_lx |>
    filter(race != "all", state_in == "lost_mom") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(limits = c(0, 4e4), breaks = c(0, 2e4, 4e4), 
                       labels = c(0, 20, 40)) +
    labs(
        x = "Age", caption = "(b) Lost mother",
        y = "(1 000s)"
        # y = expression(paste(italic(l)[italic(x)], "(2) (1 000s)"))
    )
plt_lx_line_both <- df_lx |>
    filter(race != "all", state_in == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(limits = c(0, 1e5), breaks = c(0, 50000, 1e5), 
                       labels = c(0, 50, 100)) +
    labs(
        x = "Age", caption = "(b) Lost both",
        y = "(1 000s)"
        # y = expression(paste(italic(l)[italic(x)], "(4) (1 000s)"))
    )

g_lx_line_by_race <- grid.arrange(
    plt_lx_line_none, 
    plt_lx_line_mom, 
    plt_lx_line_dad,
    plt_lx_line_both,
    legend_races,
    heights = c(1, 1, .12),
    layout_matrix = rbind(
        c(1, 1, 4, 4),
        c(2, 2, 3, 3),
        c(NA, NA, 5, 5) 
    )
)

postscript(here("plots", "lifetime", "lx_line_by_race.eps"), onefile = FALSE,
           family = "Times", width = 6, height = 4, horizontal = FALSE)
grid.draw(g_lx_line_by_race)
dev.off()
png(here("plots", "lifetime", "lx_line_by_race.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_lx_line_by_race)
dev.off()

#### SIPP parent status proportions --------------------------------------------
plt_pr_line_none <- df_pr |>
    filter(state_in == "lost_none") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = FALSE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .5, 1),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Age", y = "Proportion", caption = "(a) Lost neither")

plt_pr_line_dad <- df_pr |>
    filter(state_in == "lost_dad") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = FALSE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish, 
                       breaks = c(0, .25, .5),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, .5)) +
    labs(x = "Age", y = "Proportion", caption = "(d) Lost father")
plt_pr_line_mom <- df_pr |>
    filter(state_in == "lost_mom") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = FALSE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish, 
                       breaks = c(0, .25, .5),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, .5)) +
    labs(x = "Age", y = "Proportion", caption = "(c) Lost mother")
plt_pr_line_both <- df_pr |>
    filter(state_in == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = FALSE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish, 
                       breaks = c(0, .5, 1),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Age", y = "Proportion", caption = "(b) Lost both")

g_pr_line_by_race <- grid.arrange(
    plt_pr_line_none, 
    plt_pr_line_mom, 
    plt_pr_line_dad,
    plt_pr_line_both,
    legend_races_and_all,
    heights = c(1, 1, .12),
    layout_matrix = rbind(
        c(1, 1, 4, 4),
        c(2, 2, 3, 3),
        c(NA, NA, 5, 5) 
    )
)
postscript(here("plots", "lifetime", "pr_line_by_race.eps"), onefile = FALSE,
           family = "Times", width = 6, height = 4, horizontal = FALSE)
grid.draw(g_pr_line_by_race)
dev.off()
png(here("plots", "lifetime", "pr_line_by_race.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_pr_line_by_race)
dev.off()

#### Transition rates ----------------------------------------------------------
plt_mx_line_mom_first <- df_mx |>
    filter(state_from == "lost_none", state_to == "lost_mom") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = FALSE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .05, 0.1),
                       labels = c(0, 50, 100)) +
    coord_cartesian(ylim = c(0, 0.12)) +
    labs(
        x = "Age", caption = "(a) Losing mother first",
        y = "(per 1 000)"
    )
plt_mx_line_dad_first <- df_mx |>
    filter(state_from == "lost_none", state_to == "lost_dad") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = FALSE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .05, 0.1),
                       labels = c(0, 50, 100)) +
    coord_cartesian(ylim = c(0, 0.12)) +
    labs(
        x = "Age", caption = "(b) Losing father first",
        y = "(per 1 000)"
    )
plt_mx_line_both <- df_mx |>
    filter(state_from == "lost_none", state_to == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = FALSE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .025, .05),
                       labels = c(0, 25, 50)) +
    coord_cartesian(ylim = c(0, .05)) +
    labs(
        x = "Age", caption = "(c) Losing both at once",
        y = "(per 1 000)"
    )

plt_mx_line_dad_last <- df_mx |>
    filter(state_from == "lost_mom", state_to == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = FALSE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .1, .2),
                       labels = c(0, 100, 200)) +
    coord_cartesian(ylim = c(0, .25)) +
    labs(
        x = "Age", caption = "(d) Losing father last",
        y = "(per 1 000)"
    )

plt_mx_line_mom_last <- df_mx |>
    filter(state_from == "lost_dad", state_to == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = FALSE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .1, .2),
                       labels = c(0, 100, 200)) +
    coord_cartesian(ylim = c(0, .25)) +
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
    legend_races_and_all,
    heights = c(1, 1, 1, 1, .12),
    layout_matrix = rbind(
        c(1, 1, 2, 2, NA, NA),
        c(1, 1, 2, 2, 3, 3),
        c(4, 4, 5, 5, 3, 3),
        c(4, 4, 5, 5, NA, NA),
        c(NA, NA, NA, 6, 6, 6) 
    )
)
postscript(here("plots", "lifetime", "mx_line_by_race.eps"), onefile = FALSE,
           family = "Times", width = 6, height = 4, horizontal = FALSE)
grid.draw(g_mx_line_by_race)
dev.off()
png(here("plots", "lifetime", "mx_line_by_race.png"),
    width = 8, height = 6, units = "in", res = 800)
grid.draw(g_mx_line_by_race)
dev.off()

### Lines with 95% CIs (nhb vs nhw) --------------------------------------------
df_pr_bw <- df_pr |>
    filter(race %in% c("non-hispanic black", "non-hispanic white"))
df_mx_bw <- df_mx |>
    filter(race %in% c("non-hispanic black", "non-hispanic white"))
legend_races_bw <- (
    df_ex |>
        filter(state_in == "lost_both") |>
        ggplot(aes(.data$x, .data$value, color = .data$race)) +
        my_theme +
        geom_point(shape = 19) +
        scale_color_manual(
            name = NULL,
            values = colours_race,
            breaks = c("non-hispanic black",
                       "non-hispanic white"),
            labels = c("Non-Hispanic black",
                       "Non-Hispanic white"),
            guide = guide_legend(override.aes = list(size = 1))
        )
) |>
    get_legend()
#### SIPP parent status proportions --------------------------------------------
plt_pr_line_none_bw <- df_pr_bw |>
    filter(state_in == "lost_none") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .5, 1),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Age", y = "Proportion", caption = "(a) Lost neither")

plt_pr_line_dad_bw <- df_pr_bw |>
    filter(state_in == "lost_dad") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish, 
                       breaks = c(0, .25, .5),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, .5)) +
    labs(x = "Age", y = "Proportion", caption = "(d) Lost father")

plt_pr_line_mom_bw <- df_pr_bw |>
    filter(state_in == "lost_mom") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish, 
                       breaks = c(0, .25, .5),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, .5)) +
    labs(x = "Age", y = "Proportion", caption = "(c) Lost mother")

plt_pr_line_both_bw <- df_pr_bw |>
    filter(state_in == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish, 
                       breaks = c(0, .5, 1),
                       labels = scales::percent) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Age", y = "Proportion", caption = "(b) Lost both")

g_pr_line_by_race_bw <- grid.arrange(
    plt_pr_line_none_bw, 
    plt_pr_line_mom_bw, 
    plt_pr_line_dad_bw,
    plt_pr_line_both_bw,
    legend_races_bw,
    heights = c(1, 1, .12),
    layout_matrix = rbind(
        c(1, 1, 4, 4),
        c(2, 2, 3, 3),
        c(NA, NA, 5, 5) 
    )
)
postscript(here("plots", "lifetime", "pr_line_by_race_with_ci.eps"), onefile = FALSE,
           family = "Times", width = 6, height = 4, horizontal = FALSE)
grid.draw(g_pr_line_by_race_bw)
dev.off()
png(here("plots", "lifetime", "pr_line_by_race_with_ci.png"),
    width = 6, height = 6, units = "in", res = 800)
grid.draw(g_pr_line_by_race_bw)
dev.off()

#### Transition rates ----------------------------------------------------------
plt_mx_line_mom_first_bw <- df_mx_bw |>
    filter(state_from == "lost_none", state_to == "lost_mom") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .05, 0.1),
                       labels = c(0, 50, 100)) +
    coord_cartesian(ylim = c(0, 0.12)) +
    labs(
        x = "Age", caption = "(a) Losing mother first",
        y = "(per 1 000)"
    )

plt_mx_line_dad_first_bw <- df_mx_bw |>
    filter(state_from == "lost_none", state_to == "lost_dad") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .05, 0.1),
                       labels = c(0, 50, 100)) +
    coord_cartesian(ylim = c(0, 0.12)) +
    labs(
        x = "Age", caption = "(b) Losing father first",
        y = "(per 1 000)"
    )

plt_mx_line_both_bw <- df_mx_bw |>
    filter(state_from == "lost_none", state_to == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .025, .05),
                       labels = c(0, 25, 50)) +
    coord_cartesian(ylim = c(0, .05)) +
    labs(
        x = "Age", caption = "(c) Losing both at once",
        y = "(per 1 000)"
    )

plt_mx_line_dad_last_bw <- df_mx_bw |>
    filter(state_from == "lost_mom", state_to == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .1, .2),
                       labels = c(0, 100, 200)) +
    coord_cartesian(ylim = c(0, .25)) +
    labs(
        x = "Age", caption = "(d) Losing father last",
        y = "(per 1 000)"
    )

plt_mx_line_mom_last_bw <- df_mx_bw |>
    filter(state_from == "lost_dad", state_to == "lost_both") |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2, se = TRUE) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(oob = scales::squish,
                       breaks = c(0, .1, .2),
                       labels = c(0, 100, 200)) +
    coord_cartesian(ylim = c(0, .25)) +
    labs(
        x = "Age", caption = "(e) Losing mother last",
        y = "(per 1 000)"
    )

g_mx_line_by_race_bw <- grid.arrange(
    plt_mx_line_mom_first_bw, 
    plt_mx_line_dad_first_bw,
    plt_mx_line_both_bw,
    plt_mx_line_dad_last_bw, 
    plt_mx_line_mom_last_bw,
    legend_races_bw,
    heights = c(1, 1, 1, 1, .12),
    layout_matrix = rbind(
        c(1, 1, 2, 2, NA, NA),
        c(1, 1, 2, 2, 3, 3),
        c(4, 4, 5, 5, 3, 3),
        c(4, 4, 5, 5, NA, NA),
        c(NA, NA, NA, 6, 6, 6) 
    )
)
postscript(here("plots", "lifetime", "mx_line_by_race_with_ci.eps"), onefile = FALSE,
           family = "Times", width = 6, height = 4, horizontal = FALSE)
grid.draw(g_mx_line_by_race_bw)
dev.off()
png(here("plots", "lifetime", "mx_line_by_race_with_ci.png"),
    width = 8, height = 6, units = "in", res = 800)
grid.draw(g_mx_line_by_race_bw)
dev.off()


mx_trans <- "log10"
plt_mx_line_mom_first <- df_mx |>
    filter(state_from == "lost_none", state_to == "lost_mom") |>
    mutate(value = ifelse(value == 0, 1e-10, value)) |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(trans = mx_trans, limits = c(0.00001, 0.12), 
                       breaks = c(0.0001, 0.001, 0.01, 0.1),
                       labels = c(0.1, 1, 10, 100)) +
    labs(
        x = "Age", caption = "(a) Losing mother first",
        y = "(per 1 000)"
        # y = expression(
        #     paste({}["5"], italic(m)[italic(x)], "(1,2) (per 1 000)")
        # )
    )
plt_mx_line_dad_first <- df_mx |>
    filter(state_from == "lost_none", state_to == "lost_dad") |>
    mutate(value = ifelse(value == 0, 1e-10, value)) |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(trans = mx_trans, limits = c(0.00001, 0.12), 
                       breaks = c(0.0001, 0.001, 0.01, 0.1),
                       labels = c(0.1, 1, 10, 100)) +
    labs(
        x = "Age", caption = "(b) Losing father first",
        y = "(per 1 000)"
        # y = expression(
        #     paste({}["5"], italic(m)[italic(x)], "(1,3) (per 1 000)")
        # )
    )
plt_mx_line_both <- df_mx |>
    filter(state_from == "lost_none", state_to == "lost_both") |>
    mutate(value = ifelse(value == 0, 1e-10, value)) |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(trans = mx_trans, limits = c(0.00001, 0.12), 
                       breaks = c(0.0001, 0.001, 0.01, 0.1),
                       labels = c(0.1, 1, 10, 100)) +
    labs(
        x = "Age", caption = "(c) Losing both at once",
        y = "(per 1 000)"
        # y = expression(
        #     paste({}["5"], italic(m)[italic(x)], "(1,4) (per 1 000)")
        # )
    )

plt_mx_line_dad_last <- df_mx |>
    filter(state_from == "lost_mom", state_to == "lost_both") |>
    mutate(value = ifelse(value == 0, 1e-10, value)) |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(trans = mx_trans, limits = c(0.00001, 0.12), 
                       breaks = c(0.0001, 0.001, 0.01, 0.1),
                       labels = c(0.1, 1, 10, 100)) +
    labs(
        x = "Age", caption = "(d) Losing father last",
        y = "(per 1 000)"
        # y = expression(
        #     paste({}["5"], italic(m)[italic(x)], "(2,4) (per 1 000)")
        # )
    )

plt_mx_line_mom_last <- df_mx |>
    filter(state_from == "lost_dad", state_to == "lost_both") |>
    mutate(value = ifelse(value == 0, 1e-10, value)) |>
    plot_line(my_theme_no_legend, size = .8, linewidth = .2) +
    scale_color_manual(values = colours_race) +
    scale_y_continuous(trans = mx_trans, limits = c(0.00001, 0.12), 
                       breaks = c(0.0001, 0.001, 0.01, 0.1),
                       labels = c(0.1, 1, 10, 100)) +
    labs(
        x = "Age", caption = "(e) Losing mother last",
        y = "(per 1 000)"
        # y = expression(
        #     paste({}["5"], italic(m)[italic(x)], "(3,4) (per 1 000)")
        # )
    )

g_mx_line_by_race <- grid.arrange(
    plt_mx_line_mom_first, 
    plt_mx_line_dad_first,
    plt_mx_line_both,
    plt_mx_line_dad_last, 
    plt_mx_line_mom_last,
    legend_races_and_all,
    heights = c(1, 1, 1, 1, .12),
    layout_matrix = rbind(
        c(1, 1, 2, 2, NA, NA),
        c(1, 1, 2, 2, 3, 3),
        c(4, 4, 5, 5, 3, 3),
        c(4, 4, 5, 5, NA, NA),
        c(NA, NA, NA, 6, 6, 6) 
    )
)
postscript(here("plots", "lifetime", "mx_line_by_race_log.eps"), onefile = FALSE,
           family = "Times", width = 6, height = 4, horizontal = FALSE)
grid.draw(g_mx_line_by_race)
dev.off()
png(here("plots", "lifetime", "mx_line_by_race_log.png"),
    width = 8, height = 6, units = "in", res = 800)
grid.draw(g_mx_line_by_race)
dev.off()
