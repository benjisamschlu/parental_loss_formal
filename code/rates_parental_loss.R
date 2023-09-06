
##----------------------- PARENTAL LOSS FORMAL ---------------------------------
##
##  Rates of losing neither parent, a mother, a father, and both
##  
## 
## 
##  Author: Benjamin Schlüter
##  Date: Sept. 2023
##------------------------------------------------------------------------------
##
##  Notes
## ------
##
##  Individuals can be uniquely identified with SSUID & PNUM
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

source(here("code", "functions", "convert_to_fplh.R"))

## Load data -------------------------------------------------------------------

## Need to select cols as the file is too big to be imported as is
data <- fread(here("data_private", "sipp_data", "pu2021.csv"), 
              header = T,
              sep = "|",
              select = c("TAGE", # age at itw
                         "EBDAD", "EBMOM", # survival status of parent
                         "TBDADDODRAGE", "TBMOMDODRAGE", # age when parent died
                         "ERACE", "EORIGIN", # ethnicity
                         "WPFINWGT", # weight
                         "SPANEL", "SWAVE", "SSUID", "PNUM", "MONTHCODE") # individual identification
)



## Clean data ------------------------------------------------------------------

## Tidy col name
names(data) <- tolower(names(data)) 

## Combination of ssuid (HH) and individual within HH (pnum)
## allows to identify individuals
df <- data |> 
    mutate(
        id = paste0(ssuid, pnum)
    )

ind.ids <- df |> 
    pull(id) |> 
    unique() 

## There should be +-56,500 persons interviewed
length(ind.ids)

## and data refers to collection in 2021, so there should be 
## 12 months/lines for each individuals
(length(ind.ids) * 12) - dim(data)[1]
## 1826 inds have not 12 months of obs
## but no ind has more than 12 months of rows
df |> 
    group_by(id) |> 
    summarise(n.rows = n()) |> 
    filter(n.rows != 12)

## If info on mom or dad is missing, 
## or age at itw is missing -> rm
## crucial as fct converting data into
## full parent loss history depends on 
## these variables
rm.inds <- df |> 
    filter(
        is.na(ebdad) | 
            is.na(ebmom) |
            is.na(tage)
    ) 
## 1,684 inds
length(unique(rm.inds$id))

df <- df |> 
    filter(
        !is.na(ebdad),
        !is.na(ebmom),
        !is.na(tage)
    )

## Age at itw should be > age at parent death
df |> 
    filter((ebdad == 2 & tbdaddodrage != 999) & (tage < tbdaddodrage) | (ebmom == 2 & tbmomdodrage != 999) & (tage < tbmomdodrage))

## NA means not defined: ie mom not dead, age at mom death is NA
## 999 means, missing data -> filter



## Compute rates of parental loss ----------------------------------------------

## Convert CPS data into full parental loss
## history by age group and race/ethnicity
df.fplh <- df |> 
    mutate(
        ## create race/ethnicity categories
        race_eth = case_when(
            eorigin == 2 & erace == 1 ~ "NH-White",
            eorigin == 2 & erace == 2 ~ "NH-Black",
            eorigin == 2 & erace %in% 3:4 ~ "NH-Other",
            eorigin == 1 ~ "Hispanic"
            )
    ) |> 
    group_by(id, race_eth) |> 
    ## Use own defined fct to convert
    ## CPS data into FPLH data
    group_modify(convert_to_fplh)

## Compute rates accounting for weights
df.rates <- df.fplh |> 
    ## not needed to consider ages > 65 
    ## (interested in death of parents)
    ## and noisy above a certain age
    mutate(
        age = ifelse(age >= 65, 65, age)
        ) |> 
    group_by(
        ## Does not account for race
        ## neither sex for the moment
        age, race_eth, status
    ) |> 
    summarize(
        N = sum(w)
    ) |> 
    pivot_wider(names_from = "status", values_from = "N") |> 
    mutate(
        ## compute rates
        tot = sum(c_across(both:neither)),
        across(.cols = both:neither, .fns= ~.x/tot)
    ) |> 
    dplyr::select(!tot) |> 
    pivot_longer(both:neither, names_to = "status", values_to = "rate")


## Check: sum == 1
df.fplh |> 
    ## not needed to consider ages > 65 
    ## (interested in death of parents)
    mutate(age = ifelse(age >= 65, 65, age)) |> 
    group_by(
        ## Does not account for race
        ## neither sex for the moment
        age, race_eth, status
    ) |> 
    summarize(
        N = sum(w)
    ) |> 
    pivot_wider(names_from = "status", values_from = "N") |> 
    mutate(
        ## compute rates
        tot = sum(c_across(both:neither)),
        across(.cols = both:neither, .fns= ~.x/tot)
    ) |> 
    dplyr::select(!tot) |> 
    mutate(sum = sum(c_across(both:neither)))



## Visu ------------------------------------------------------------------------

df.rates |> 
    ggplot(aes(x = age, y = rate, 
               group = interaction(status, race_eth), 
               col = status, 
               linetype = race_eth, shape = race_eth)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 1, 0.2),
                       limits = c(0, 1)) +
    labs(x = "Age at loss",
         col = "Parent death",
         linetype = "Race/ethnicity",
         shape = "Race/ethnicity")

ggsave(here("plots", 
            "rates_parental_loss_by_race.jpeg"))
