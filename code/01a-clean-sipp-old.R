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
## https://www.census.gov/data-tools/demo/uccb/sippdict?s_keyword=&sortby=name

##  tage: age as of last bday
##  ebdad: mortality status of father (2 = deceased; from 2018)
##  ebmom: mortality status of mother (2 = deceased; from 2018)
##  TBDADDODRAGE: age in years when father died (only in 2021 and 2022)
##  TBMOMDODRAGE: age in years when mother died (only in 2021 and 2022)
##  TBDADDOB_Y: In what year was father born? (only in 2014 Wave 1)
##  TBDADDOD_Y: In what year did father die? (only in 2014 Wave 1)
##  TBMOMDOB_Y: In what year was mother born? (only in 2014 Wave 1)
##  TBMOMDOD_Y: In what year did mother die? (only in 2014 Wave 1)
##  EORIGIN : Is of Hispanic, Latino, or Spanish origin
##  ERACE: What race(s) does ... consider herself/himself to be?
##  ECITIZEN: Is ... a citizen of the United States?
##  ESEX: sex
##  TDOB_BYEAR: year of birth
##  EDOB_BMONTH: month of birth
##  SPANEL: panel year
##  MONTHCODE: reference month
##  SSUID: sample unit ID (household)
##  PNUM:  person number within sample unit
##  WPFINWGT: final person weight

## weighting: monthly, yearly, multiple years
## https://www2.census.gov/programs-surveys/sipp/tech-documentation/methodology/2022_SIPP_Users_Guide_JUN23.pdf
## CHOOSING APPROPRIATE WEIGHTS IN 2022 SIPP
## Time Duration | Person Level          | Example
## --------------|-----------------------|---------
## Monthly       | WPFINWGT              | Income in July, 2021
## Calendar Year | WPFINWGT in December  | Total TANF receipt in 2021
##               | of the reference year |
## Multi-Year    | FINYR# where # = 2, 3 | Median duration spell of
##               | represents the number | unemployment from June 2019 
##               | of years in the       | to June 2021
##               | reference period      | (# = 3)
## Most SIPP questions asked during the interview refered to the preceding 
## calendar year (e.g., January through December of 2021 for 2022 SIPP).
## --------------
##
## 2020 US life tables by race and sex accessed on August 28, 2023 as reported 
## in Arias, E and Xu, J. United States life tables 2020. 
## National vital statistics reports, 71(1). Hyattsville, 
## MD: National Center for Health Statistics. 2022. 
## https://www.cdc.gov/nchs/data/nvsr/nvsr71/nvsr71-01.pdf
##

##  Checks ---------------------------------------------------------------------

rm(list = ls())

## Load packages ---------------------------------------------------------------

## Install/load packages
packages <- c("tidyverse", "here", "utils", "data.table", "testthat")
for (p in packages) {
    if (!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Functions -------------------------------------------------------------------
tidy_race_and_sex <- function(df) {
    df |>
        rename_with(tolower) |>
        # for yearly estimates, use only December
        # individuals (ssuid-pnum) appear 12 times a year
        filter(.data$monthcode == "12") |>
        mutate(
            race = case_when(
                .data$eorigin == 1 ~ "hispanic",
                .data$erace == 1 ~ "non-hispanic white",
                .data$erace == 2 ~ "non-hispanic black",
                .data$erace == 3 ~ "non-hispanic asian",
                .default = "non-hispanic other"
            ),
            sex = if_else(esex == 1, "male", "female")
            # ,
            # # rescale weights 
            # # (unnecessary for rate calculation)
            # wpfinwgt = wpfinwgt / 10000
        ) 
}

get_parent_survival_weights <- function(df) {
     df_age <- df |>
         mutate(
             mom_loss_age = if_else(
                 .data$ebmom == 1, .data$tage, .data$tbmomdodrage
                ),
             dad_loss_age = if_else(
                 .data$ebdad == 1, .data$tage, .data$tbdaddodrage
                ),
             both_loss_age = if_else(
                 .data$mom_loss_age < .data$dad_loss_age,
                 .data$dad_loss_age, .data$mom_loss_age
                ),
             ebboth = case_when(
                 .data$ebmom == 1 & .data$ebdad == 1 ~ "both_alive",
                 .data$ebmom == 2 & .data$ebdad == 2 ~ "both_dead",
                 .data$ebmom * .data$ebdad == 2 ~ "one_alive"
             ),
             ebmom = if_else(.data$ebmom == 1, "alive", "dead"),
             ebdad = if_else(.data$ebdad == 1, "alive", "dead")
         )
     df_mom_loss <- df_age |>
         filter(!is.na(.data$ebmom)) |>  # requires mother's survivorship info
         group_by(.data$ebmom, .data$mom_loss_age, .data$sex, .data$race) |>
         summarise(w = sum(.data$wpfinwgt),
                   n = n(),
                   .groups = "keep") |>
         mutate(parent = "mom") |>
         rename(parent_status = "ebmom", loss_age = "mom_loss_age")
     df_dad_loss <- df_age |>
         filter(!is.na(.data$ebdad)) |>  # requires father's survivorship info
         group_by(.data$ebdad, .data$dad_loss_age, .data$sex, .data$race) |>
         summarise(w = sum(.data$wpfinwgt), 
                   n = n(),
                   .groups = "keep") |>
         mutate(parent = "dad") |>
         rename(parent_status = "ebdad", loss_age = "dad_loss_age")
     df_both_loss <- df_age |>
         filter(!is.na(.data$ebmom), !is.na(.data$ebdad)) |>
         group_by(.data$ebboth, .data$both_loss_age, .data$sex, .data$race) |>
         summarise(w = sum(.data$wpfinwgt), 
                   n = n(),
                   .groups = "keep") |>
         mutate(parent = "both") |>
         rename(parent_status = "ebboth", loss_age = "both_loss_age")
     
     bind_rows(df_mom_loss, df_dad_loss, df_both_loss) |>
         select("sex", "race", "parent", "loss_age", "parent_status", "w", "n") 
}

## Load data -------------------------------------------------------------------

years <- c(2021, 2022) # years
columns_sipp <- c(
    "TAGE",             # age of the respondent
    # "TDOB_BYEAR",       # year of birth of the respondent
    # "EDOB_BMONTH",      # month of birth of the respondent
    "EBDAD", "EBMOM",   # survival status of parent: 1 - alive; 2 - deceased
    "TBDADDODRAGE", "TBMOMDODRAGE",    # age when parent died
    #                                    # (not available in 2018)
    # "TBDADDOB_Y", "TBMOMDOB_Y",        # year of parent birth
    # "TBDADDOD_Y", "TBMOMDOD_Y",        # year when parent died
    # "ECITIZEN",         # is US citizen? 1 - Y; 2 - N
    "ESEX",             # sex: 1 - male; 2 - female
    "EORIGIN",          # is hispanic/latino;spanish? 1 - Y; 2 - N
    "ERACE",            # race: 1 - white; 2 - black; 3 - asian; 4 - others
    # "TPTOTINC"          # monthly earnings/income 
    # "SWAVE",            # wave number of interview
    # "SPANEL",           # panel year
    "MONTHCODE",        # reference month
    "SSUID",            # sample unit ID
    "PNUM",             # person number
    "WPFINWGT"          # final person weight
)

df_sipp <- lapply(
    list.files(here("data_private", "sipp"), full.names = TRUE),
    fread, header = TRUE, sep = "|", 
    select = columns_sipp
)
names(df_sipp) <- years

### Data check
test_that(
    "SIPP data check.", 
    {
        for (y in years) {
    
            months_found <- names(
                table(df_sipp[[as.character(y)]]$MONTHCODE, useNA = "ifany")
            )
            expect_true(all(as.character(1:12) %in% months_found))
            expect_false(any(is.na(months_found)))
        }
    }
)

## Clean data ------------------------------------------------------------------
df_sipp <- lapply(df_sipp, tidy_race_and_sex) |>
    lapply(get_parent_survival_weights)

## Save data -------------------------------------------------------------------
res <- sapply(seq(length(years)), function(i) {
    f <- paste0("sipp_", years[i], ".csv")
    df <- df_sipp[[i]]
    write.csv(df, here("data", f), row.names = FALSE)
})
