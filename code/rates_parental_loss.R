
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
## crucial as fct converting data depends on 
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

## Required improvements:
# 1. Exposure not precise: parent dying at 23, individual
#    supposed to be exposed from [0, 25)
# 2. Right now, use December weight for all years. This might
#    be incorrect.

## Example to develop code
ind <- df |> 
    filter(
        id == 11481646521101
    )

## Created data to test function with all possible code
ind <- tibble(tage = 39,
              ebdad = 2,
              ebmom = 2,
              tbdaddodrage = 30,
              tbmomdodrage = 29,
              monthcode = 3)

convert_data <- function(ind, ...) {
    
    ## select latest month (despite
    ## it shouldn't change over the months)
    max.month <- max(ind$monthcode)
    
    ## select one row
    row <- ind[max.month == ind$monthcode, ]
    
    ## neither parent are dead
    if (row$ebdad == 1 & row$ebmom == 1) {
        
        ## outputed dataframe of exposure
        df.exp <- tibble(
            age = seq(0, row$tage, 5),
            status = "neither",
            w = row$wpfinwgt/1000
            )
    ## only father dead    
    } else if (row$ebdad == 2 & row$ebmom == 1) {
        
        ## missing info on age when dad dies -> assume alive
        if (row$tbdaddodrage == 999) {
            
            ## outputed dataframe of exposure
            df.exp <- tibble(
                age = seq(0, row$tage, 5),
                status = "neither",
                w = row$wpfinwgt/1000
            )
        } else {
            
            ## outputed dataframe of exposure
            df.exp <- tibble(
                age = seq(0, row$tage, 5),
                status = "father",
                w = row$wpfinwgt/1000
            )
            df.exp$status[df.exp$age %in% seq(0, (row$tbdaddodrage - 5), 5)] <- "neither"
        }
    ## only mother dead    
    } else if (row$ebdad == 1 & row$ebmom == 2) {
        
        ## missing info on age when mother dies -> assume alive
        if (row$tbmomdodrage == 999) {
            
            ## outputed dataframe of exposure
            df.exp <- tibble(
                age = seq(0, row$tage, 5),
                status = "neither",
                w = row$wpfinwgt/1000
            )
        } else {
            
            ## outputed dataframe of exposure
            df.exp <- tibble(
                age = seq(0, row$tage, 5),
                status = "mother",
                w = row$wpfinwgt/1000
            )
            df.exp$status[df.exp$age %in% seq(0, (row$tbmomdodrage - 5), 5)] <- "neither"
        }
    ## both parents are dead    
    } else {
        
        ## missing info on age when both died
        if (row$tbdaddodrage == 999 & row$tbmomdodrage == 999) {
            
            ## outputed dataframe of exposure
            df.exp <- tibble(
                age = seq(0, row$tage, 5),
                status = "neither",
                w = row$wpfinwgt/1000
            )
        ## missing info on age when father died    
        } else if (row$tbdaddodrage == 999) {
            
            ## outputed dataframe of exposure
            df.exp <- tibble(
                age = seq(0, row$tage, 5),
                status = "mother",
                w = row$wpfinwgt/1000
            )
            df.exp$status[df.exp$age %in% seq(0, (row$tbmomdodrage - 5), 5)] <- "neither"
            
        ## missing info on age when mother died    
        } else if (row$tbmomdodrage == 999) {
            
            ## outputed dataframe of exposure
            df.exp <- tibble(
                age = seq(0, row$tage, 5),
                status = "father",
                w = row$wpfinwgt/1000
            )
            df.exp$status[df.exp$age %in% seq(0, (row$tbdaddodrage - 5), 5)] <- "neither"
            
        ## both ages when parents died are known    
        } else {
            
            ## are death of mother and father in the same age interval?
            agegps_at_parent_death <- cut(c(row$tbdaddodrage, row$tbmomdodrage),
                                         seq(0,100, 5),
                                         right = FALSE)
            
            ## if parent died at the same age
            if (length(unique(agegps_at_parent_death)) == 1) {
                
                ## outputed dataframe of exposure
                df.exp <- tibble(
                    age = seq(0, row$tage, 5),
                    status = "both",
                    w = row$wpfinwgt/1000
                )
                df.exp$status[df.exp$age %in% seq(0, (row$tbmomdodrage - 5), 5)] <- "neither"
                
            ## parent died at different ages    
            } else {
                
                ## mother died first
                if (row$tbdaddodrage > row$tbmomdodrage) {
                    
                    ## outputed dataframe of exposure
                    df.exp <- tibble(
                        age = seq(0, row$tage, 5),
                        status = "both",
                        w = row$wpfinwgt/1000
                    )
                    df.exp$status[df.exp$age %in% seq(0, (row$tbmomdodrage - 5), 5)] <- "neither"
                    
                    df.exp$status[df.exp$age %in% seq((row$tbmomdodrage - (row$tbmomdodrage%%5)), (row$tbdaddodrage - 5), 5)] <- "mother"
                 
                ## father died first       
                } else {
                    
                    ## outputed dataframe of exposure
                    df.exp <- tibble(
                        age = seq(0, row$tage, 5),
                        status = "both",
                        w = row$wpfinwgt/1000
                    )
                    df.exp$status[df.exp$age %in% seq(0, (row$tbdaddodrage - 5), 5)] <- "neither"
                    
                    df.exp$status[df.exp$age %in% seq((row$tbdaddodrage - (row$tbdaddodrage%%5)), (row$tbmomdodrage - 5), 5)] <- "father"
                    
                }
            }
        }
    } 
    return(df.exp)
}

test <- df[1:10000, ]

## Still issue with seq, likely due to similar age at mom and dad death

test <- test |> 
    group_by(id) |> 
    group_modify(convert_data)

convert_data(ind)

## Need to remove missing age 
## but filter (tbdaddodrage != 999) remove the NA
## while NA means no age because no parent death
## -> Maybe see "neither" for best practice

data |> 
    filter(
        !is.na(ebmom),
        !is.na(ebdad),
        tbdaddodrage < tage )
