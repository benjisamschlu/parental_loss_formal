##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon
##  Date: July 2024
##------------------------------------------------------------------------------

##  Checks ---------------------------------------------------------------------

rm(list = ls())

## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("here", "utils", "openxlsx", "pdftools", "tidyr", "dplyr", "testthat")
for (p in packages) {
    if (!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}
source("code/utils.R")

## Load data -------------------------------------------------------------------
us_mort_pop <- readRDS(here("data_private", "us_mort_pop.RDS"))

## Clean data ------------------------------------------------------------------
# rename mortality data
us_mort_pop <- us_mort_pop |>
    rename(
        race = "race_eth",
        age = "age_years"
    ) |>
    mutate(
        race = case_match(
            race,
            "black" ~ "non-hispanic black",
            "white" ~ "non-hispanic white",
            .default = race
        )
    )
# parse and organize fertility data
col_names <- c("year", seq(10, 45, 5))
fx_mom_2019 <- read.xlsx(
    here("data_private", "us_fert_mom.xlsx"),
    sheet = "A",
    rows = c(
        21:50, # all races
        109:142, # non-hispanic white
        190:223, # non-hispanic black
        # 262:285, # aian
        # 320:339, # api
        352:381 # hispanic
    ),
    cols = c(1, 4, 5, 8:13),
    colNames = FALSE
    ) |>
    rename_with(~ col_names) |>
    filter(!grepl("(single race)", year)) |>
    mutate(
        race = c(
            rep("all", 30),
            rep("non-hispanic white", 30),
            rep("non-hispanic black", 30),
            rep("hispanic", 30)
        )
    ) |>
    pivot_longer(
        matches("\\d+"),
        names_to = "age",
        values_to = "fx",
    ) |>
    mutate(
        fx = as.numeric(fx) / 1000 # recorded as per 1 000
    )

fx_mom_pdf <- pdf_text(here("data_private", "us_fert_mom2020.pdf"))
pages <- c(
    `all` = 13, 
    `non-hispanic white` = 13, 
    `non-hispanic black` = 13, 
    `hispanic` = 14
)
rows2020 <- c(
    `all` = 16,
    `non-hispanic white` = 30,
    `non-hispanic black` = 37, 
    `hispanic` = 16
)

fx_mom_2020 <- list()
for (r in names(pages)) {
    # extract the appropriate page
    tmp <- fx_mom_pdf[pages[r]] |>
        strsplit("\\n")
    # extract for year 2020
    tmp <- tmp[[1]][rows2020[r]]
    # clean
    tmp <- gsub("(\\.\\s)+", " ", tmp)
    tmp <- strsplit(tmp, "\\s+")[[1]]
    tmp <- tmp[tmp != ""]
    fx_mom_2020[[r]] <- data.frame(
        year = tmp[1],
        race = r,
        age = unique(fx_mom_2019$age),
        fx = as.double(tmp[c(3, 4, 7:12)]) / 1000
    )
}

fx_mom <- bind_rows(fx_mom_2020) |>
    bind_rows(fx_mom_2019) |>
    arrange(race, year, age) |>
    mutate(sex = "female")

fx_dad <- read.csv(here("data_private", "us_fert_dad.txt")) |>
    rename_with(tolower) |>
    mutate(
        year = year1,
        sex = "male",
        race = "all",
        fx = asfr
    ) |>
    filter(year >= 1990) |>
    select(race, sex, year, age, fx)

# pop_dad <- read.csv(
#     unz(
#         here("data_raw", "UNdata_Export_20240723_195548935.zip"),
#         "UNdata_Export_20240723_195548935.csv"
#     )
# ) |>
#     rename_with(tolower) |>
#     select(year, age, value) |>
#     rename(pop = value) |>
#     filter(grepl("^\\d+$", age)) |>
#     mutate(
#         age = as.numeric(age),
#         year = as.integer(year)
#     )
# 
# fx_dad <- fx_dad |>
#     left_join(
#         pop_dad,
#         by = c("year", "age")
#     )

## Save data -------------------------------------------------------------------
write.csv(
    us_mort_pop, here("data_private", "us_mort_pop.csv"), row.names = FALSE
)
write.csv(
    fx_mom, here("data_private", "us_fx_mom.csv"), row.names = FALSE
)
write.csv(
    fx_dad, here("data_private", "us_fx_dad.csv"), row.names = FALSE
)
