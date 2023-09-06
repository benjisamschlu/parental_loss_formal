##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon, Benjamin Schlüter
##  Date: August 2023
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

## Functions -------------------------------------------------------------------



## Load data -------------------------------------------------------------------

## Loop and download data by year
years <- c(2021, 2022) # years
base_url <- paste0(
    "https://www2.census.gov/programs-surveys/sipp/", 
    "data/datasets/%s/pu%s_csv.zip"
)

# download
download_urls <- sprintf(base_url, years, years)
download_res <- sapply(
    download_urls, 
    function(u) download.file(u, here("data_private", basename(u)))
)

test_that(
    "Downloaded SIPP files successfully.", 
    {
        expect_true(all(download_res == 0))
    }
)
# unzip
unzip_res <- sapply(
    download_urls,
    function(u) unzip(here("data_private", basename(u)),
                      exdir = here("data_private", "sipp"))
)
test_that(
    "Extracted SIPP files successfully.", 
    {
        expect_true(all(grepl("\\.csv$", unzip_res)))
    }
)
