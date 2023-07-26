
##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  
## 
## 
##  Author: Benjamin Schlüter
##  Date: July 2023
##------------------------------------------------------------------------------
##
##  Notes on data
## --------------
##
## https://www.census.gov/data-tools/demo/uccb/sippdict?s_keyword=&sortby=name

##  tage: age as of last bday
##  ebdad: mortality status of father (2 = deceased)
##  ebmom
##  TBDADDODRAGE: age in years when father died
##  TBMOMDODRAGE: age in years when mother died
##  TBDADDOB_Y: In what year was father born?
##  TBDADDOD_Y: In what year did father die 
##  TBMOMDOB_Y
##  TBMOMDOD_Y
##  EORIGIN : Is of Hispanic, Latino, or Spanish origin
##  ERACE
##  ECITIZEN: Is ... a citizen of the United States?

## weighting: monthly, yearly, multiple years
## https://www2.census.gov/programs-surveys/sipp/tech-documentation/methodology/2022_SIPP_Users_Guide_JUN23.pdf
##
##  Checks
## -------
##  
## 
##------------------------------------------------------------------------------

rm(list = ls())



## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("tidyverse", "ggplot2", "here", "utils", "data.table")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}



## Functions -------------------------------------------------------------------



## Load data -------------------------------------------------------------------

## Loop and download + clean data by year
download_url <- "https://www2.census.gov/programs-surveys/sipp/data/datasets/2022/pu2022_csv.zip"

download.file(download_url,
              here("data_private", basename(download_url)))

unzip(here("data_private", basename(download_url)),
      exdir = here("data_private", "sipp_data"))

## get cols name
# data <- read.table(
#     here("data_private", "sipp_data", "pu2022.csv"),
#     header = TRUE,
#     sep = "|",
#     nrows = 3
# )
# names(data)[grep("FIN", names(data))]

## Need to select cols as the file is too big to be imported as is
data <- fread(here("data_private", "sipp_data", "pu2022.csv"), 
              header = T,
              sep = "|",
              select = c("TAGE", "EBDAD", "EBMOM", "TBDADDODRAGE", 
                         "TBMOMDODRAGE", "TBDADDOB_Y", "TBDADDOD_Y",
                         "TBMOMDOB_Y", "TBMOMDOD_Y", "EORIGIN",
                         "ERACE", "ECITIZEN",
                         "WPFINWGT")
              )

file.remove(here("data_private", basename(download_url)))



## Clean data ------------------------------------------------------------------

## Tidy
names(data) <- tolower(names(data)) 

df <- data |> 
    ## focus on individuals losing a mother (try reproducing plot)
    filter(ebmom == 2 | ebdad == 2,
           tbmomdodrage != 999,
           tbdaddodrage != 999,
           !is.na(tbmomdodrage),
           !is.na(tbdaddodrage)) |> 
    mutate(
        ## tidy age in 5y age groups
        age_loss_mom = cut(tbmomdodrage, 
                  breaks = c(seq(0, 70, 5), Inf),
                  right = FALSE),
        age_loss_dad = cut(tbdaddodrage, 
                           breaks = c(seq(0, 70, 5), Inf),
                           right = FALSE),
        ## rescale weights
        w = wpfinwgt/10000
        ) |> 
    pivot_longer(age_loss_mom:age_loss_dad,
                 names_to = "parent",
                 values_to = "age_loss") |> 
    mutate(parent = substr(parent, 10,12)) |> 
    group_by(
        ## Does not account for CoD 
        ## neither age and sex for the moment
        age_loss, parent
    ) |> 
    summarize(
        N = sum(w)
    ) |> 
    group_by(parent) |> 
    mutate(
        ## age at mother death
        perc = (N/sum(N))*100
    )
## Don't get the same percentages as here
# https://www.census.gov/library/visualizations/interactive/losing-our-parents.html



## Visu ------------------------------------------------------------------------

df |> 
    ggplot(aes(x = age_loss, y = perc, group = parent, col = parent)) +
    geom_line() +
    geom_point() +
    theme_bw()

