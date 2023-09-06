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
            sex = if_else(esex == 1, "male", "female"),
            # ,
            # # rescale weights 
            # # (unnecessary for rate calculation)
            # w = .data$wpfinwgt / 10000
            w = .data$wpfinwgt
        ) 
}

get_parent_survival_weights <- function(df) {
    max_age <- df |>
        select("tbmomdodrage", "tbdaddodrage") |>
        mutate(
            across(everything(), function(x) {
                x <- replace_na(x, 0)
                if_else(x == 999, 0, x)
            }),
            max_age = pmax(.data$tbmomdodrage, .data$tbdaddodrage)
        ) |>
        pull(max_age) |> 
        max()
        
    df_complete <- df |>
        filter(
            # consider complete cases only
            !is.na(.data$ebmom), !is.na(.data$ebdad),
            # TODO: missing info on age when dad dies -> assume alive?
            !.data$tbmomdodrage %in% 999, !.data$tbdaddodrage %in% 999
        ) |>
        mutate(
            tbmomdodrage = if_else(.data$ebmom == 1, 9999, .data$tbmomdodrage),
            tbdaddodrage = if_else(.data$ebdad == 1, 9999, .data$tbdaddodrage),
            # age when mom died while dad is alive
            age_lost_mom_first = if_else(
                .data$tbmomdodrage < .data$tbdaddodrage, 
                .data$tbmomdodrage, 9999
            ),
            # age when mom died while dad is dead
            age_lost_mom_last = if_else(
                .data$tbmomdodrage > .data$tbdaddodrage, 
                .data$tbmomdodrage, 9999
            ),
            # age when dad died while mom is alive
            age_lost_dad_first = if_else(
                .data$tbdaddodrage < .data$tbmomdodrage, 
                .data$tbdaddodrage, 9999
            ),
            # age when dad died while mom is dead
            age_lost_dad_last = if_else(
                .data$tbdaddodrage > .data$tbmomdodrage,
                .data$tbdaddodrage, 9999
            ),
            # age when both died if in the same year
            age_lost_both = if_else(
                .data$tbmomdodrage == .data$tbdaddodrage, 
                .data$tbmomdodrage, 9999
            )
        )
    
    lapply(seq(from = 0, to = max_age), function(age) {
        df_complete |>
            # lived at least `age` years
            filter(.data$tage >= age) |>
            mutate(
                # incidence
                i_lost_mom_first = .data$age_lost_mom_first == age,
                i_lost_mom_last = .data$age_lost_mom_last == age,
                i_lost_dad_first = .data$age_lost_dad_first == age,
                i_lost_dad_last = .data$age_lost_dad_last == age,
                i_lost_both = .data$age_lost_both == age,
                # state (at the start of age)
                s_lost_none = (
                    .data$tbmomdodrage >= age & .data$tbdaddodrage >= age 
                ),
                s_lost_mom = (
                    .data$tbmomdodrage < age & .data$tbdaddodrage >= age
                ),
                s_lost_dad = (
                    .data$tbmomdodrage >= age & .data$tbdaddodrage < age
                ),
                s_lost_both = (
                    .data$tbmomdodrage < age & .data$tbdaddodrage < age
                )
            ) |>
            summarise(
                .by = c("sex", "race"),
                # weights
                w_i_lost_mom_first = sum(.data$i_lost_mom_first * .data$w),
                w_i_lost_mom_last = sum(.data$i_lost_mom_last * .data$w),
                w_i_lost_dad_first = sum(.data$i_lost_dad_first * .data$w),
                w_i_lost_dad_last = sum(.data$i_lost_dad_last * .data$w),
                w_i_lost_both = sum(.data$i_lost_both * .data$w),
                
                w_s_lost_none = sum(.data$s_lost_none * .data$w),
                w_s_lost_mom = sum(.data$s_lost_mom * .data$w),
                w_s_lost_dad = sum(.data$s_lost_dad * .data$w),
                w_s_lost_both = sum(.data$s_lost_both * .data$w),
                w_s_total = sum(.data$w),
                
                # counts
                n_i_lost_mom_first = sum(.data$i_lost_mom_first),
                n_i_lost_mom_last = sum(.data$i_lost_mom_last),
                n_i_lost_dad_first = sum(.data$i_lost_dad_first),
                n_i_lost_dad_last = sum(.data$i_lost_dad_last),
                n_i_lost_both = sum(.data$i_lost_both),
                
                n_s_lost_none = sum(.data$s_lost_none),
                n_s_lost_mom = sum(.data$s_lost_mom),
                n_s_lost_dad = sum(.data$s_lost_dad),
                n_s_lost_both = sum(.data$s_lost_both)
            ) |>
            mutate(age = age)
    }) |>
        bind_rows()
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
df_sipp_by_ps <- lapply(df_sipp, tidy_race_and_sex) |>
    lapply(get_parent_survival_weights)
names(df_sipp_by_ps) <- years
### Data check
test_that(
    "SIPP cleaned data check.", 
    {
        
        for (y in years) {
            max_age <- max(df_sipp_by_ps[[as.character(y)]]$age)
            for (x in 0:max_age) {
                total_counts_before <- df_sipp[[as.character(y)]] |>
                    filter(
                        MONTHCODE == "12",
                        !is.na(EBMOM), !is.na(EBDAD),
                        !TBMOMDODRAGE %in% 999, !TBDADDODRAGE %in% 999,
                        TAGE >= x
                    ) |>
                    nrow()
                total_couns_after <- df_sipp_by_ps[[as.character(y)]] |>
                    filter(.data$age == x) |>
                    select(starts_with("n_s_")) |>
                    sum()
                expect_equal(total_counts_before, total_couns_after)
            }
        }
    }
)

## Save data -------------------------------------------------------------------
res <- sapply(seq(length(years)), function(i) {
    f <- paste0("sipp_", years[i], ".csv")
    df <- df_sipp_by_ps[[i]]
    write.csv(df, here("data", f), row.names = FALSE)
})
