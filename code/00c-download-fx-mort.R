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
packages <- c("here", "utils", "testthat")
for (p in packages) {
    if (!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Download data ---------------------------------------------------------------

# From https://github.com/benjisamschlu/parental_deaths/
# `national_year_age-sex-race_drug-opioid-mortality.RDS` containing 
# multiple causes of death data and population estimates 
# from January 1, 1990 to December 31, 2020
mortality_url <- paste0(
    "https://github.com/benjisamschlu/parental_deaths/",
    "raw/main/inputs/data_public/",
    "national_year_age-sex-race_drug-opioid-mortality.RDS"
)
mortality_dest <- here("data_private", "us_mort_pop.RDS")
mortality_res <- 0
if (!file.exists(mortality_dest)) {
    mortality_res <- download.file(mortality_url, mortality_dest)
}

# From https://www.cdc.gov/nchs/hus/data-finder.htm?year=2020-2021&table=Table%20Brth
# Fertility rates by age, race, and Hispanic origin of mother, 1950-2019
fertility_url <- paste0(
    "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/", 
    "Publications/Health_US/hus20-21tables/brth.xlsx"
)
fertility_dest <- here("data_private", "us_fert_mom.xlsx")
fertility_res <- 0
if (!file.exists(fertility_dest)) {
    fertility_res <- download.file(fertility_url, fertility_dest)
}

# From https://www.cdc.gov/nchs/data/nvsr/nvsr72/nvsr72-01.pdf
# Fertility rates for 2020
fertility2020_url <- paste0(
    "https://www.cdc.gov/nchs/data/nvsr/nvsr72/nvsr72-01.pdf"
)
fertility2020_dest <- here("data_private", "us_fert_mom2020.pdf")
fertility2020_res <- 0
if (!file.exists(fertility2020_dest)) {
    fertility2020_res <- download.file(fertility2020_url, fertility2020_dest)
}

# From https://www.fertilitydata.org/File/GetFile/Country/males/USA/m_USA_ASFRstand_TOT.txt
# Fertility rates up to 2015, male
fertility_male_url <- paste0(
    "https://www.fertilitydata.org/File/GetFile/",
    "Country/males/USA/m_USA_ASFRstand_TOT.txt"
)
fertility_male_dest <- here("data_raw", "us_fert_dad.txt")
fertility_male_res <- 0
if (!file.exists(fertility_male_dest)) {
    fertility_male_res <- download.file(fertility_male_url, fertility_male_dest)
}


test_that(
    "Downloaded data files successfully.",
    {
        expect_true(mortality_res == 0)
        expect_true(fertility_res == 0)
        expect_true(fertility2020_res == 0)
        expect_true(fertility_male_res == 0)
    }
)
