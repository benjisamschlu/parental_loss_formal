##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon, Benjamin Schlüter
##  Date: August 2023
##------------------------------------------------------------------------------
##
##  Notes on data
## --------------
##
## 2020 US life tables by race and sex accessed on August 28, 2023 as reported 
## in Arias, E and Xu, J. United States life tables 2020. 
## National vital statistics reports, 71(1). Hyattsville, 
## MD: National Center for Health Statistics. 2022. 
## https://www.cdc.gov/nchs/data/nvsr/nvsr71/nvsr71-01.pdf
##
## age: age (years) - 0-1, 1-2, ..., 100 and over
## qx:  probability of dying between age x and x + 1
## lx:  number of surviving to age x
## dx:  number of dying between ages x and x + 1
## Lx:  person-years lived between ages x and x + 1
## Tx:  total number of personz-years lived above age x
## ex:  expectation of live at age x

##  Checks ---------------------------------------------------------------------

rm(list = ls())

## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("tidyverse", "readxl", "here", "utils", "testthat")
for (p in packages) {
    if (!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Functions -------------------------------------------------------------------
clean_uslt <- function(df) {
    df |>
        mutate(x = str_extract(df[[1]], "^\\d+")) |>
        filter(!is.na(.data$x)) |>
        select("x", "qx", "lx", "dx", "Lx", "Tx", "ex")
}


## Load data -------------------------------------------------------------------
files_uslt <- list.files(here("data_private", "uslt"), full.names = TRUE)
columns_uslt <- c("age", "qx", "lx", "dx", "Lx", "Tx", "ex")
df_uslt <- (
    lapply(files_uslt, read_excel, skip = 2)
)

yr_race_sex <- do.call(rbind, str_split(basename(files_uslt), "_"))[ , 2:4]
colnames(yr_race_sex) <- c("year", "race", "sex")
yr_race_sex <- as_tibble(yr_race_sex) |>
    mutate(
        sex = str_remove(sex, ".xlsx"),
        race = str_replace(race, "non-hispanic-", "non-hispanic ")
    )
years <- unique(yr_race_sex$year)

## Clean data ------------------------------------------------------------------
df_uslt <- lapply(df_uslt, clean_uslt)
df_uslt <- lapply(seq(length(files_uslt)), function(i) {
    df_uslt[[i]] |>
        mutate(race = yr_race_sex$race[i],
               sex = yr_race_sex$sex[i])
})
df_uslt_by_year <- lapply(split(df_uslt, yr_race_sex$year), bind_rows)

### Data check
test_that(
    "US life table data check.", 
    {
        expect_equal(length(years), length(df_uslt_by_year))
        for (y in years) {
            expect_equal(nrow(df_uslt_by_year[[y]]), 1818)
        }
    }
)

## Save data -------------------------------------------------------------------
res <- sapply(seq(length(years)), function(i) {
    f <- paste0("uslt_", years[i], ".csv")
    df <- df_uslt_by_year[[i]]
    write.csv(df, here("data", f), row.names = FALSE)
})
