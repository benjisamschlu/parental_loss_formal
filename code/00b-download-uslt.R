##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Michael Jongho Moon
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
# years <- c(2020, 2019)
years <- c(2020)
# pub_num <- c("71-01", "70-19")
pub_num <- c("71-01")
table_nums <- 1:18
race <- rep(c("all",
              "hispanic", 
              "non-hispanic-american-indian-alaskan-native",
              "non-hispanic-asian",
              "non-hispanic-black",
              "non-hispanic-white"),
            each = 3)
sex <- rep(c("all", "male", "female"), 6)
base_url <- paste0(
    "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/NVSR/",
    "%s/Table%02d.xlsx"
)
download_urls <- sprintf(base_url,
                         rep(pub_num, each = length(table_nums)), 
                             table_nums)
dest_files <- sprintf("uslt_%s_%s_%s.xlsx", 
                      rep(years, each = length(table_nums)),
                      rep(race, length(years)),
                      rep(sex, length(years)))
# download
if (!dir.exists(here("data_private", "uslt"))) {
    dir.create(here("data_private", "uslt"))
}
download_res <- sapply(
    seq(length(download_urls)), 
    function(i) 
        download.file(download_urls[i], 
                      here("data_private", "uslt", dest_files[i]))
)

test_that(
    "Downloaded US life table files successfully.", 
    {
        expect_true(all(download_res == 0))
    }
)
