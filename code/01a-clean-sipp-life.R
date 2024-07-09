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
packages <- c("tidyverse", "here", "utils", "testthat", 
              "bit64", "readxl", "data.table")
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
            sex = if_else(esex == 1, "male", "female")
        ) 
}

get_parent_survival_by_age <- function(df) {
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
            # TODO: missing info on age when dad dies 
            # -> assume alive?
            # -> any other imputation methods?
            #  -> use EBMOMDODLT19 and EBDADDODLT19
            # -> sensitivity analysis?
            !.data$tbmomdodrage %in% 999, !.data$tbdaddodrage %in% 999
        ) |>
        mutate(
            tbmomdodrage = if_else(.data$ebmom == 1, 9999, .data$tbmomdodrage),
            tbdaddodrage = if_else(.data$ebdad == 1, 9999, .data$tbdaddodrage)
        )
    
    s_lost_both <- sapply(0:max_age, function(age) {
        ifelse(
            df_complete$tage >= age,
            (df_complete$tbmomdodrage < age & 
                 df_complete$tbdaddodrage < age) * 1,
            NA
        )
    }) |>
        as.data.table() |>
        rename_with(function(x) paste0("s_lost_both_", 0:max_age))
    
    s_lost_dad <- sapply(0:max_age, function(age) {
        ifelse(
            df_complete$tage >= age,
            (df_complete$tbmomdodrage >= age &
                 df_complete$tbdaddodrage < age) * 1,
            NA
        )
    }) |>
        as.data.table() |>
        rename_with(function(x) paste0("s_lost_dad_", 0:max_age))
    
    s_lost_mom <- sapply(0:max_age, function(age) {
        ifelse(
            df_complete$tage >= age,
            (df_complete$tbmomdodrage < age &
                 df_complete$tbdaddodrage >= age) * 1,
            NA
        )
    }) |>
        as.data.table() |>
        rename_with(function(x) paste0("s_lost_mom_", 0:max_age))
    
    s_lost_none <- sapply(0:max_age, function(age) {
        ifelse(
            df_complete$tage >= age,
            (df_complete$tbmomdodrage >= age &
                 df_complete$tbdaddodrage >= age) * 1,
            NA
        )
    }) |>
        as.data.table() |>
        rename_with(function(x) paste0("s_lost_none_", 0:max_age))
    
    
    i_lost_both <- sapply(0:max_age, function(age) {
        (df_complete$tbmomdodrage == age & df_complete$tbdaddodrage == age) * 1  
    }) |>
        as.data.table() |>
        rename_with(function(x) paste0("i_lost_none_to_lost_both_", 0:max_age))
    
    i_lost_dad_first <- sapply(0:max_age, function(age) {
        (df_complete$tbmomdodrage > age & df_complete$tbdaddodrage == age) * 1  
    }) |>
        as.data.table() |>
        rename_with(function(x) paste0("i_lost_none_to_lost_dad_", 0:max_age))
    
    i_lost_mom_first <- sapply(0:max_age, function(age) {
        (df_complete$tbmomdodrage == age & df_complete$tbdaddodrage > age) * 1  
    }) |>
        as.data.table() |>
        rename_with(function(x) paste0("i_lost_none_to_lost_mom_", 0:max_age))
    
    i_lost_dad_last <- sapply(0:max_age, function(age) {
        (df_complete$tbmomdodrage < age & df_complete$tbdaddodrage == age) * 1  
    }) |>
        as.data.table() |>
        rename_with(function(x) paste0("i_lost_mom_to_lost_both_", 0:max_age))
    
    i_lost_mom_last <- sapply(0:max_age, function(age) {
        (df_complete$tbmomdodrage == age & df_complete$tbdaddodrage < age) * 1  
    }) |>
        as.data.table() |>
        rename_with(function(x) paste0("i_lost_dad_to_lost_both_", 0:max_age))
    
    bind_cols(
        df_complete |>
            select("monthcode", "pnum", "spanel", "swave", "ssuid", 
                   "race", "sex"),
        s_lost_none, s_lost_mom, s_lost_dad, s_lost_both,
        i_lost_both, 
        i_lost_mom_first, i_lost_dad_first,
        i_lost_mom_last, i_lost_dad_last
    ) |>
        pivot_longer(cols = c(starts_with("s_"), starts_with("i_"))) |>
        mutate(
            age = as.numeric(str_extract(name, "\\d+")),
            name = str_remove(name, "_\\d+")
        ) |>
        pivot_wider() |>
        filter(!is.na(s_lost_none))
}

## Load data -------------------------------------------------------------------
yr <- 2021 # survey year
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

pu_data <- list.files(
    here("data_private"), 
    paste0("^pu", yr, ".+zip"),
    full.names = TRUE
)
pu_valid <- list.files(
    here("data_private"), 
    paste0("^pu", yr, ".+validate"),
    full.names = TRUE
)
rw_data <- list.files(
    here("data_private"), 
    paste0("^rw", yr, ".+zip"), 
    full.names = TRUE
)
rw_valid <- list.files(
    here("data_private"), 
    paste0("^rw", yr, ".+validate"), 
    full.names = TRUE
)

pu <- fread(
    cmd = paste("unzip -p", pu_data),
    header = TRUE, 
    sep = "|", 
    select = columns_sipp   
)
rw <- fread(
    cmd = paste("unzip -p", rw_data),
    header = TRUE,
    sep = "|"
)

valid_pu <- read_xlsx(pu_valid)
valid_rw <- read_xlsx(rw_valid)

### Data check
test_that(
    "SIPP data read check.", 
    {
        pu_check <- join_validation(
            select(pu, -"SSUID"), 
            select(valid_pu, -"Label")
        )
        expect_equal(pu_check$value.x, pu_check$value.y, tolerance = 1e-5)
        rw_check <- join_validation(rw |> select(-ssuid), valid_rw)
        expect_equal(rw_check$value.x, rw_check$value.y, tolerance = 1e-5)
    }
)

## Clean data ------------------------------------------------------------------
pu_tidy <- tidy_race_and_sex(pu) |>
    get_parent_survival_by_age()

purw <- pu_tidy |>
    inner_join(rw, by = c("spanel", "swave", "ssuid", "pnum", "monthcode")) |>
    select(
        "race", "sex", "age",
        starts_with("s_"),
        starts_with("i_"),
        starts_with("repwgt")
    )

### Computation check
test_that(
    "Check correct age expansion.",
    {
        tmp <- pu_tidy |>
            summarise(
                .by = c("spanel", "swave", "ssuid", "pnum", "monthcode"),
                n = n(), age = max(age) + 1 # age 0 added
            )
        expect_equal(tmp$n, tmp$age)
    }
)

## Save data -------------------------------------------------------------------
res <- write.csv(
    purw, here("data_private", "sipp_2021-life.csv"), row.names = FALSE
)
