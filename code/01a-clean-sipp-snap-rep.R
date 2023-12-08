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
## Respondents who do not have their biological mother/father
## in the household are asked the parent mortality questions, including:
##  - Whether a respondent’s biological mother/father is 
##    still alive (EBMOM, EBDAD)
##  - If a parent is deceased, the respondent’s age in single 
##    years when that parent died (EBMOMDODRAGE, EBDADDODRAGE)
##  - If a parent is deceased and if the respondent did not provide 
##    an age at the time of that parent’s death, they are asked whether 
##    they were less than 19 years old (a child) or older (an adult) 
##    at the time of death (EBMOMDODLT19, EBDADDODLT19)
## These items are asked during the Parent section at the back end of
## the survey of each respondent whose biological parent(s) is/are not 
## present in the household. For respondents whose biological parent(s) 
## is/are present in the household, the parent(s) is/are marked as 
## still alive. Thus, the parent mortality variables on the released 
## file contain information on the biological parents of all respondents.

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
packages <- c("tidyverse", "here", "utils", 
              "bit64", "readxl", "data.table",
              "testthat")
for (p in packages) {
    if (!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Functions -------------------------------------------------------------------
join_validation <- function(data, validate_data) {
    data_summary <- data |>
        summarise(
            across(
                everything(), 
                .names = "{.fn}_{.col}",
                .fns = list(
                    Mean = ~ mean(.x, na.rm = TRUE),
                    `Std Dev` = ~ sd(.x, na.rm = TRUE),
                    Minimum = ~ min(.x, na.rm = TRUE),
                    Maximum = ~ max(.x, na.rm = TRUE),
                    `N Miss` = ~ sum(is.na(.x))
                )
            ) 
        ) |>
        mutate(
            across(everything(), as.double)
        ) |>
        pivot_longer(
            everything(),
            names_to = c("Summary", "Variable"),
            names_sep = "_"
        )
    valid_summary <- validate_data |>
        select(-N) |>
        pivot_longer(-Variable, names_to = "Summary")
    
    data_summary |>
        inner_join(valid_summary, by = c("Summary", "Variable")) 
}

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
            sex = if_else(.data$esex == 1, "male", "female")
        ) 
}
get_parent_survival <- function(df) {
    df |>
        filter(
            # consider complete cases only
            !is.na(.data$ebmom), !is.na(.data$ebdad),
            # TODO: missing info on age when dad dies 
            # -> assume alive?
            # -> any other imputation methods?
            # -> use EBMOMDODLT19 and EBDADDODLT19
            # -> sensitivity analysis?
            !.data$tbmomdodrage %in% 999, !.data$tbdaddodrage %in% 999
        ) |>
        mutate(
            i_lost_none_to_lost_mom = (.data$ebdad == 1 & 
                                    .data$tbmomdodrage %in% .data$tage) * 1,
            i_lost_none_to_lost_dad = (.data$ebmom == 1 &
                                    .data$tbdaddodrage %in% .data$tage) * 1,
            i_lost_dad_to_lost_both = (.data$ebdad == 2 & 
                                   .data$tbmomdodrage %in% .data$tage) * 1,
            i_lost_mom_to_lost_both = (.data$ebmom == 2 &
                                   .data$tbdaddodrage %in% .data$tage) * 1,
            i_lost_none_to_lost_both = (.data$tbmomdodrage %in%.data$tage &
                               .data$tbdaddodrage %in% .data$tage) * 1,
            s_lost_none = (.data$ebmom == 1 & .data$ebdad == 1) * 1,
            s_lost_mom = (.data$ebmom == 2 & .data$ebdad == 1) * 1,
            s_lost_dad = (.data$ebmom == 1 & .data$ebdad == 2) * 1,
            s_lost_both = (.data$ebmom == 2 & .data$ebdad == 2) * 1,
            s_lost_none0 = .data$s_lost_none + .data$i_lost_none_to_lost_mom + 
                .data$i_lost_none_to_lost_dad + .data$i_lost_none_to_lost_both,
                s_lost_mom0 = .data$s_lost_mom + .data$i_lost_mom_to_lost_both,
            s_lost_dad0 = .data$s_lost_dad + .data$i_lost_dad_to_lost_both
            
        ) |>
        rename(age = "tage") |>
        select("spanel", "swave", "ssuid", "pnum", "monthcode",
               "race", "sex", "age",
               starts_with("s_"), starts_with("i_"))
}
## Load data -------------------------------------------------------------------

years <- c(2021, 2022) # years
columns_sipp <- c(
    "TAGE",             # age of the respondent
    "EBDAD", "EBMOM",   # survival status of parent: 1 - alive; 2 - deceased
    "TBDADDODRAGE", "TBMOMDODRAGE",    # age when parent died
    "EBMOMDODLT19", "EBDADDODLT19",
    "ESEX",             # sex: 1 - male; 2 - female
    "EORIGIN",          # is hispanic/latino;spanish? 1 - Y; 2 - N
    "ERACE",            # race: 1 - white; 2 - black; 3 - asian; 4 - others
    "MONTHCODE",        # reference month
    "PNUM",             # person number
    "SPANEL",           # panel year
    "SWAVE",            # wave number of interview
    "SSUID",            # sample unit ID
    "WPFINWGT"          # final person weight
)

pu_data <- list.files(here("data_private"), "^pu.+zip", full.names = TRUE)
pu_valid <- list.files(here("data_private"), "^pu.+validate", full.names = TRUE)
rw_data <- list.files(here("data_private"), "^rw.+zip", full.names = TRUE)
rw_valid <- list.files(here("data_private"), "^rw.+validate", full.names = TRUE)

pu <- lapply(
    pu_data,
    function(x) fread(
        cmd = paste("unzip -p", x),
        header = TRUE, 
        sep = "|", 
        select = columns_sipp   
    )
)
rw <- lapply(
    rw_data,
    function(x) fread(
        cmd = paste("unzip -p", x),
        header = TRUE,
        sep = "|"
    )
)

valid_pu <- lapply(pu_valid, read_xlsx)
valid_rw <- lapply(rw_valid, read_xlsx)

names(pu) <- years
names(rw) <- years
names(valid_pu) <- years
names(valid_rw) <- years

### Data check
test_that(
    "SIPP data read check.", 
    {
        for (y in years) {
            y <- as.character(y)
            pu_check <- join_validation(
                pu[[y]] |> 
                    select(-SSUID), 
                valid_pu[[y]] |> 
                    select(-Label) 
            )
            expect_equal(pu_check$value.x, pu_check$value.y,
                         tolerance = 1e-5)
            rw_check <- join_validation(
                rw[[y]] |> select(-ssuid), 
                valid_rw[[y]]
            )
            expect_equal(rw_check$value.x, rw_check$value.y,
                         tolerance = 1e-5)
        }
    }
)

## Clean data ------------------------------------------------------------------
pu_tidy <- lapply(pu, tidy_race_and_sex) |>
    lapply(get_parent_survival)
purw <- mapply(
    function(x ,y) inner_join(
        x, y, by = c("spanel", "swave", "ssuid", "pnum", "monthcode")
    ) |> 
        select("race", "sex", "age", 
               starts_with("s_"), 
               starts_with("i_"),
               starts_with("repwgt")),
    pu_tidy, rw, SIMPLIFY = FALSE
)

## Save data -------------------------------------------------------------------
res <- sapply(seq(length(years)), function(i) {
    f <- paste0("sipp_snap_", years[i], ".csv")
    df <- purw[[i]]
    write.csv(df, here("data", f), row.names = FALSE)
})
