
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
##  SPANEL: panel year

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
# download_url <- "https://www2.census.gov/programs-surveys/sipp/data/datasets/2021/pu2022_csv.zip"
# download_url <- "https://www2.census.gov/programs-surveys/sipp/data/datasets/2021/pu2021_csv.zip"
# 
# download.file(download_url,
#               here("data_private", basename(download_url)))
# 
# unzip(here("data_private", basename(download_url)),
#       exdir = here("data_private", "sipp_data"))

## get cols name
# data <- read.table(
#     here("data_private", "sipp_data", "pu2022.csv"),
#     header = TRUE,
#     sep = "|",
#     nrows = 3
# )
# names(data)[grep("FIN", names(data))]

## Need to select cols as the file is too big to be imported as is
data <- fread(here("data_private", "sipp_data", "pu2021.csv"), 
              header = T,
              sep = "|",
              select = c("TAGE", "EBDAD", "EBMOM", "TBDADDODRAGE", 
                         "TBMOMDODRAGE", "TBDADDOB_Y", "TBDADDOD_Y",
                         "TBMOMDOB_Y", "TBMOMDOD_Y", "EORIGIN",
                         "ERACE", "ECITIZEN",
                         "WPFINWGT",
                         "SPANEL", "SWAVE", "PNUM", "MONTHCODE",
                         "TPTOTINC") # data check
              )

# file.remove(here("data_private", basename(download_url)))

## Data check
# mean(data$TPTOTINC, na.rm = TRUE)



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

## Get the same percentages as here
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



## Compute rates for Monica ----------------------------------------------------

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
  mutate(
    ## Categorise age in 5-year bands
    age_loss = cut(tbmomdodrage, 
                        breaks = c(seq(0, 70, 5), Inf),
                        right = FALSE),
         ## rescale weights
         w = wpfinwgt/10000,
    ebmom = ifelse(ebmom == 1, "m_alive", "m_dead")
    ) |> 
  group_by(
    ## Does not account for race
    ## neither age and sex for the moment
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
  mutate(
    ## Categorise age in 5-year bands
    age_loss = cut(tbdaddodrage, 
                   breaks = c(seq(0, 70, 5), Inf),
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
      ebdad == 2 & ebmom == 2 ~ "both_dead",
      TRUE ~ "one_alive"
    ),
    tbdodrage = case_when(
      ## If both parent are death, age when second died
      ebdad == 2 & ebmom == 2 ~ pmap_int(list(tbdaddodrage, tbmomdodrage), max),
      ## if only one parent died
      TRUE ~ tage
      )
    ) |> 
  mutate(
    ## Categorise age in 5-year bands
    age_loss = cut(tbdodrage, 
                   breaks = c(seq(0, 70, 5), Inf),
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
    rate = both_dead/(both_dead+one_alive)
  )

## Losing neither
df.rates.loss.neither <- data |> 
  filter(!is.na(ebdad),
         !is.na(ebmom)) |> 
  ## Need to have this info
  mutate(
    eb = case_when(
      (ebdad == 1) & (ebmom == 1) ~ "none_dead",
      (ebdad == 2) | (ebmom == 2) ~ "other"
    ),
    tbdodrage = case_when(
      ## If both parent are death, age when second died
      (ebdad == 1) & (ebmom == 1) ~ tage,
      ## as soon as one parent dies, at this age not "neither" anymore
      (ebdad == 2) & (ebmom == 2) ~ min(c(tbdaddodrage, tbmomdodrage)),
      (ebdad == 1) & (ebmom == 2) ~ tbmomdodrage,
      (ebdad == 2) & (ebmom == 1) ~ tbdaddodrage
      )
    ) |> 
  ## remove observations where age at 
  ## parent death is required but unknown
  filter(tbdodrage != 999) |> 
  mutate(
    ## Categorise age in 5-year bands
    age_loss = cut(tbdodrage, 
                   breaks = c(seq(0, 70, 5), Inf),
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
    rate = none_dead/(none_dead+other)
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



## Visu ------------------------------------------------------------------------

df.rates |> 
  ggplot(aes(x = age_loss, y = rate, group = parent, col = parent)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Age at loss")
