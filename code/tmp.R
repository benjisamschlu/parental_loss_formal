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
packages <- c("tidyverse", "readxl", "here", "utils", "data.table", "testthat")
for(p in packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    library(p,character.only = TRUE)
}

## Functions -------------------------------------------------------------------



## Load data -------------------------------------------------------------------

### SIPP
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

#### Data check
for (y in years) {
    test_that(
        "SIPP data check.", 
        {
            months_found <- names(
                table(df_sipp[[as.character(y)]]$MONTHCODE, useNA = "ifany")
            )
            expect_true(all(as.character(1:12) %in% months_found))
            expect_false(any(is.na(months_found)))
        }
    )
}

### US life tables
files_uslt <- list.files(here("data_private", "uslt"), full.names = TRUE)
columns_uslt <- c("age", "qx", "lx", "dx", "Lx", "Tx", "ex")
df_uslt <- (
    lapply(files_uslt,read_excel, skip = 2)
)
df_ltus[1]


## Clean data ------------------------------------------------------------------

## Tidy
names(data) <- tolower(names(data)) 


## Loss of mother
df.loss.mom <- data |> 
  filter(ebmom ==2, ## filter ind losing a mother
         !is.na(tbmomdodrage),
         tbmomdodrage != 999) |> ## having info on age when losing mother 
  mutate(age_loss = cut(tbmomdodrage, 
                        breaks = c(seq(0, 70, 5), Inf),
                        right = FALSE),
         ## rescale weights
         w = wpfinwgt/10000
         ) |> 
  group_by(
    ## Does not account for race
    ## neither age and sex for the moment
    age_loss
  ) |> 
  summarize(
    N = sum(w)
  ) |> 
  ungroup() |> 
  mutate(
    ## age at mother death
    perc = (N/sum(N))*100
  )


## Loss of father
df.loss.dad <- data |> 
  filter(ebdad ==2,
         !is.na(tbdaddodrage),
         tbdaddodrage != 999) |> 
  mutate(age_loss = cut(tbdaddodrage, 
                        breaks = c(seq(0, 70, 5), Inf),
                        right = FALSE),
         ## rescale weights
         w = wpfinwgt/10000
  ) |> 
  group_by(
    ## Does not account for race
    ## neither age and sex for the moment
    age_loss
  ) |> 
  summarize(
    N = sum(w)
  ) |> 
  ungroup() |> 
  mutate(
    ## age at mother death
    perc = (N/sum(N))*100
  )

## Check that it corresponds to the percentages shown here
# https://www.census.gov/library/visualizations/interactive/losing-our-parents.html

## Bind the df
df <- bind_rows(df.loss.mom |> 
                  mutate(parent = "mom"),
                df.loss.dad |> 
                  mutate(parent = "dad"))



## Visu ------------------------------------------------------------------------

df |> 
    ggplot(aes(x = age_loss, y = perc, group = parent, col = parent)) +
    geom_line() +
    geom_point() +
    theme_bw()
## Similar figure as on census website



## Compute rates for Michael Moon ----------------------------------------------

## Need to remove missing age 
## but filter (tbdaddodrage != 999) remove the NA
## while NA means no age because no parent death
## -> Maybe see "neither" for best practice

## Losing mother
df.rates.loss.mom <- data |> 
  filter(!is.na(ebmom)) |> 
  mutate(
    ## For ind not having lost their mother,
    ## set tage = tbmomdodrage
    tbmomdodrage = ifelse(ebmom == 1, tage, tbmomdodrage)
    ) |> 
  filter(
    ## rm unknown age age death
    tbmomdodrage != 999) |>
  mutate(
    ## Categorise age in 5-year bands
    age_loss = cut(tbmomdodrage, 
                        breaks = c(seq(0, 65, 5), Inf),
                        right = FALSE),
         ## rescale weights
         w = wpfinwgt/10000,
    ebmom = ifelse(ebmom == 1, "m_alive", "m_dead")
    ) |> 
  group_by(
    ## Does not account for race
    ## and sex for the moment
    ebmom, age_loss
  ) |> 
  summarize(
    N = sum(w)
  ) |> 
  pivot_wider(names_from = "ebmom", values_from = "N") |> 
  mutate(
    ## compute rates
    rate = m_dead/(m_alive+m_dead)
  )

## Losing father
df.rates.loss.dad <- data |> 
  filter(!is.na(ebdad)) |> 
  mutate(
    ## For ind not having lost their mother,
    ## set tage = tbmomdodrage
    tbdaddodrage = ifelse(ebdad == 1, tage, tbdaddodrage)
  ) |> 
  filter(tbdaddodrage != 999) |>
  mutate(
    ## Categorise age in 5-year bands
    age_loss = cut(tbdaddodrage, 
                   breaks = c(seq(0, 65, 5), Inf),
                   right = FALSE),
    ## rescale weights
    w = wpfinwgt/10000,
    ebdad = ifelse(ebdad == 1, "d_alive", "d_dead")
  ) |> 
  group_by(
    ## Does not account for race
    ## neither age and sex for the moment
    ebdad, age_loss
  ) |> 
  summarize(
    N = sum(w)
  ) |> 
  pivot_wider(names_from = "ebdad", values_from = "N") |> 
  mutate(
    ## compute rates
    rate = d_dead/(d_alive+d_dead)
  )

## Losing both
df.rates.loss.both <- data |> 
  filter(!is.na(ebdad),
         !is.na(ebmom)) |> 
  mutate(
    eb = case_when(
      (ebdad == 2) & (ebmom == 2) ~ "both_dead",
      (ebdad == 1) | (ebmom == 1) ~ "at_least_one_alive"
    ),
    tbdodrage = case_when(
      ## If both parent are death, age when second died
      eb == "both_dead" ~ pmax(tbdaddodrage, tbmomdodrage),
      ## if only one parent died
      TRUE ~ tage
      )
    ) |> 
  filter(tbdodrage != 999) |>
  mutate(
    ## Categorise age in 5-year bands
    age_loss = cut(tbdodrage, 
                   breaks = c(seq(0, 65, 5), Inf),
                   right = FALSE),
    ## rescale weights
    w = wpfinwgt/10000
  ) |> 
  group_by(
    ## Does not account for race
    ## neither sex for the moment
    eb, age_loss
  ) |> 
  summarize(
    N = sum(w)
  ) |> 
  pivot_wider(names_from = "eb", values_from = "N") |> 
  mutate(
    ## compute rates
    rate = both_dead/(both_dead+at_least_one_alive)
  )

## Losing neither
df.rates.loss.neither <- data |> 
  filter(!is.na(ebdad),
         !is.na(ebmom)) |> 
  ## Need to have this info
  mutate(
    eb = case_when(
      (ebdad == 1) & (ebmom == 1) ~ "neither_dead",
      (ebdad == 2) | (ebmom == 2) ~ "at_least_one_loss"
    ),
    tbdodrage = case_when(
      ## If both parent are death, age when second died
      eb == "neither_dead" ~ tage,
      ## if both parent death, earlier age -> pmin
      ## if only one, NA will be ignored and min is the only age considered
      eb == "at_least_one_loss" ~ pmin(tbmomdodrage, tbdaddodrage, na.rm = TRUE)
      )
    ) |> 
  filter(tbdodrage != 999) |>
  mutate(
    ## Categorise age in 5-year bands
    age_loss = cut(tbdodrage, 
                   breaks = c(seq(0, 65, 5), Inf),
                   right = FALSE),
    ## rescale weights
    w = wpfinwgt/10000
  ) |> 
  group_by(
    ## Does not account for race
    ## neither sex for the moment
    eb, age_loss
  ) |> 
  summarize(
    N = sum(w)
  ) |> 
  pivot_wider(names_from = "eb", values_from = "N") |> 
  mutate(
    ## compute rates
    rate = neither_dead/(neither_dead+at_least_one_loss)
  )



## Combine
df.rates <- bind_rows(
  df.rates.loss.mom |> 
    dplyr::select(age_loss, rate) |> 
    mutate(parent = "mom") ,
  df.rates.loss.dad |> 
      dplyr::select(age_loss, rate) |> 
    mutate(parent = "dad"),
  df.rates.loss.both |> 
      dplyr::select(age_loss, rate) |> 
    mutate(parent = "both"),
  df.rates.loss.neither |> 
    dplyr::select(age_loss, rate) |> 
    mutate(parent = "neither")
)

## Store df
saveRDS(df.rates,
        here("data", "df_rates.rda"))

# df.rates <- readRDS(here("data", "df_rates.rda"))

## Visu ------------------------------------------------------------------------

df.rates |> 
  ggplot(aes(x = age_loss, y = rate, group = parent, col = parent)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "Age at loss",
       col = "Parent death")
