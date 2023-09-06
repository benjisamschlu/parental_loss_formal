
## Function converting CPS data into full parental loss history (FPLH)
## by 5 years age group.

## Required improvements:
# 1. Exposure not precise: parent dying at 23, individual
#    supposed to be exposed from [0, 25)
# 2. Right now, use December weight for all years. This 
#    ~~might be incorrect~~ 
#    is correct. (confirmed in the SIPP documentation)

convert_to_fplh <- function(ind, ...) {
    
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


# ## Examples to develop code

# ind <- df |> 
#     filter(
#         id == 41811411621101 
#     )
# 
# ## Created data to test function with all possible code
# ind <- tibble(tage = 39,
#               ebdad = 2,
#               ebmom = 2,
#               tbdaddodrage = 30,
#               tbmomdodrage = 29,
#               monthcode = 3)
# ## Test function
# convert_data(ind)
# 
# ## Debugging
# 
# ids <- unique(df$id)
# 
# for (i in ids) {
#     
#     cat(i, "\n")
#     
#     temp = df |> 
#         filter(id == i)
#     
#     convert_data(temp)
# }


