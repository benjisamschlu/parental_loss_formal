
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
        id == 41811411621101 
    )

## Created data to test function with all possible code
ind <- tibble(tage = 39,
              ebdad = 2,
              ebmom = 2,
              tbdaddodrage = 30,
              tbmomdodrage = 29,
              monthcode = 3)
## Test function
convert_data(ind)

## Debugging

ids <- unique(df$id)

for (i in ids) {
    
    cat(i, "\n")
    
    temp = df |> 
        filter(id == i)
    
    convert_data(temp)
}


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
            ## Father not dead in 1st age interval
            if (row$tbdaddodrage >= 5) {
                
                df.exp$status[df.exp$age %in% seq(0, (row$tbdaddodrage - 5), 5)] <- "neither"
            }
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
            
            ## Mother not dead in 1st age interval
            if (row$tbmomdodrage >= 5) {
                
                df.exp$status[df.exp$age %in% seq(0, (row$tbmomdodrage - 5), 5)] <- "neither"
            }
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
            ## Mother not dead in 1st age interval
            if (row$tbmomdodrage >= 5) {
                
                df.exp$status[df.exp$age %in% seq(0, (row$tbmomdodrage - 5), 5)] <- "neither"
            }
            
            
        ## missing info on age when mother died    
        } else if (row$tbmomdodrage == 999) {
            
            ## outputed dataframe of exposure
            df.exp <- tibble(
                age = seq(0, row$tage, 5),
                status = "father",
                w = row$wpfinwgt/1000
            )
            
            ## Father not dead in 1st age interval
            if (row$tbdaddodrage >= 5) {
                
                df.exp$status[df.exp$age %in% seq(0, (row$tbdaddodrage - 5), 5)] <- "neither"
            }
            
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
                ## Parents did not die in 1st age interval
                if (agegps_at_parent_death[1] != "[0,5)") {
                    
                    df.exp$status[df.exp$age %in% seq(0, (row$tbmomdodrage - 5), 5)] <- "neither"
                    
                }
                
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
                    
                    ## if father was not lost in 1st age interval
                    if (row$tbmomdodrage >= 5) {
                        
                        df.exp$status[df.exp$age %in% seq(0, (row$tbmomdodrage - 5), 5)] <- "neither"
                        
                        df.exp$status[df.exp$age %in% seq((row$tbmomdodrage - (row$tbmomdodrage%%5)), (row$tbdaddodrage - 5), 5)] <- "mother"
                        
                    } else {
                        
                        df.exp$status[df.exp$age %in% seq(0, (row$tbdaddodrage - 5), 5)] <- "mother"
                        
                    }
                ## father died first       
                } else {
                    
                    ## outputed dataframe of exposure
                    df.exp <- tibble(
                        age = seq(0, row$tage, 5),
                        status = "both",
                        w = row$wpfinwgt/1000
                    )
                    
                    ## if father was not lost in 1st age interval
                    if (row$tbdaddodrage >= 5) {
                        
                        df.exp$status[df.exp$age %in% seq(0, (row$tbdaddodrage - 5), 5)] <- "neither"
                        
                        df.exp$status[df.exp$age %in% seq((row$tbdaddodrage - (row$tbdaddodrage%%5)), (row$tbmomdodrage - 5), 5)] <- "father"
                        
                    } else {
                        
                        df.exp$status[df.exp$age %in% seq(0, (row$tbmomdodrage - 5), 5)] <- "father"
                        
                    }
                    
                    
                    
                }
            }
        }
    } 
    return(df.exp)
}

df.rates <- df |> 
    group_by(id) |> 
    group_modify(convert_data)

## Compute rates accounting for weights
test <- df.rates |> 
    ## not needed to consider ages > 70 
    ## (interested in death of parents)
    mutate(age = ifelse(age >= 70, 70, age)) |> 
    group_by(
        ## Does not account for race
        ## neither sex for the moment
        age, status
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
df.rates |> 
    ## not needed to consider ages > 70 
    ## (interested in death of parents)
    mutate(age = ifelse(age >= 70, 70, age)) |> 
    group_by(
        ## Does not account for race
        ## neither sex for the moment
        age, status
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

test |> 
    ggplot(aes(x = age, y = rate, group = status, col = status)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 1, 0.2),
                       limits = c(0, 1)) +
    labs(x = "Age at loss",
         col = "Parent death")

