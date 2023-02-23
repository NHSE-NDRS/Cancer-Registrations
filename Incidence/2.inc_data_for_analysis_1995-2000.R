# setup ----
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(readstata13)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load standard pop ---------------
ESP_pop <- fread("")
ESP_pop$agegroup[ESP_pop$agegroup == "0"] <- "00-00"
ESP_pop$sex <-
  factor(ESP_pop$sex ,
         levels = c('Male', 'Female'),
         labels = c('1', '2'))
names(ESP_pop)[2:3] <- c('standard', 'age_gp')
ESP_pop <- ESP_pop %>%
  mutate(
    age_gp_2 = case_when(
      age_gp == "00-00" | age_gp == "01-04" ~ "00-04",
      age_gp == "85-89" |
        age_gp == "90plus" ~ "85+",
      T ~ age_gp
    )
  ) %>%
  group_by(sex, age_gp = age_gp_2) %>%
  summarise(standard = sum(standard))

icd4digit_sites <-
  read.dta13(
    ""
  )

inc_raw <-
  readRDS(file = "")


inc <- inc_raw %>%
  mutate(
    age_flag = is.na(age),
    sex_flag = is.na(sex),
    date_flag = diagnosisdatebest < birthdatebest,
    site_flag = !(icd10code_3char %in% c(
      sprintf('C%02d', 0:97), sprintf('D%02d', 0:48)
    )) &
      !(
        str_detect(coding_system_desc, "ICD-10")  &
          substr(site_coded, 1, 3) %in% c(sprintf('C%02d', 0:97), sprintf('D%02d', 0:48))
      )
  ) %>%
  filter(!(age_flag == 1 |
             sex_flag == 1 | date_flag == 1 | site_flag)) %>%
  mutate(
    icd10code = ifelse(icd10code == 'C22' &
                         site_coded == 'C220', 'C229', icd10code),
    icd10code = ifelse(icd10code == 'C22' &
                         morph_coded == '8000', 'C229', icd10code),
    icd10code = ifelse(
      icd10code %in% c('C71', 'C72') &
        str_detect(behaviour_coded_desc, 'UNCERTAIN'),
      site_coded,
      icd10code
    ),
    icd10code = ifelse(icd10code == 'C71' &
                         morph_coded == '9421', 'D430', icd10code),
    icd10code = ifelse(icd10code == 'C83' &
                         morph_coded == '9680', 'C833', icd10code),
    icd10code = ifelse(icd10code == 'C92' &
                         morph_coded == '9863', 'C921', icd10code),
    icd10code_3char = substr(icd10code, 1, 3)
  )

## create stage and age_gp ----
inc <-
  inc %>% mutate(
    age_gp = case_when(
      age >= 0 & age <= 4 ~ '00-04',
      age >= 5 & age <= 9 ~ '05-09',
      age >= 10 & age <= 14 ~ '10-14',
      age >= 15 & age <= 19 ~ '15-19',
      age >= 20 & age <= 24 ~ '20-24',
      age >= 25 & age <= 29 ~ '25-29',
      age >= 30 & age <= 34 ~ '30-34',
      age >= 35 & age <= 39 ~ '35-39',
      age >= 40 & age <= 44 ~ '40-44',
      age >= 45 & age <= 49 ~ '45-49',
      age >= 50 & age <= 54 ~ '50-54',
      age >= 55 & age <= 59 ~ '55-59',
      age >= 60 & age <= 64 ~ '60-64',
      age >= 65 & age <= 69 ~ '65-69',
      age >= 70 & age <= 74 ~ '70-74',
      age >= 75 & age <= 79 ~ '75-79',
      age >= 80 & age <= 84 ~ '80-84',
      age >= 85 ~ '85+'
    )
  )


## create release and publish flag and filter on publish flag ----
inc <- inc %>%
  mutate(release = case_when(
    icd10code_3char %in% c(sprintf('C%02d', 0:97), sprintf('D%02d', 0:48)) ~
      1,
    str_detect(coding_system_desc, "ICD-10")  &
      substr(site_coded, 1, 3) %in% c(sprintf('C%02d', 0:97), sprintf('D%02d', 0:48)) ~ 1,
    T ~ 0
  )) %>%
  mutate(
    release = case_when(
      icd10code_3char == 'D04' ~ 0,
      icd10code_3char %in% sprintf('D%02d', 10:31) ~ 0,
      icd10code %in% c('D350', 'D351', 'D355', 'D356', 'D357', 'D358', 'D359', 'D36') ~ 0,
      T ~ release
    )
  ) %>%
  mutate(icd = case_when(
    release == 1 &
      icd10code_3char %in% c(sprintf('C%02d', 0:97), sprintf('D%02d', 0:48)) ~ str_trim(icd10code, side = 'both'),
    release == 1  ~ str_trim(site_coded, side = 'both'),
    T ~ ''
  )) %>%
  filter(release != 0)

## merge site codes ----

inc <-
  inc %>% merge(icd4digit_sites,
                by.x = 'icd',
                by.y = 'icd10',
                all.y = T)



## collapse data down --------------------
inc <- inc %>%
  group_by(year, sex, age_gp, icd) %>%
  summarise(tumourcount = sum(release), .groups = 'drop')
## complete the data ----
inc <- inc %>%
  complete(year, icd, age_gp, sex, fill = list(tumourcount = 0)) %>%
  mutate(icd_3digit = ifelse(
    icd %in% c("D352", "D353", "D354"),
    as.character(icd),
    substr(icd, 1, 3)
  ))


inc <- inc %>%
  mutate(
    combinations_to_drop = case_when(
      icd_3digit %in% sprintf('C5%s', 1:8) & sex == '1' ~ 1,
      icd_3digit %in% sprintf('C6%s', 0:3) & sex == '2' ~ 1,
      icd_3digit == 'D06' & sex == '1' ~ 1,
      icd %in% sprintf('D07%s', 0:3) & sex == '1' ~ 1,
      icd_3digit == 'D39' & sex == '1' ~ 1,
      icd %in% sprintf('D07%s', 4:6) & sex == '2' ~ 1,
      icd_3digit == 'D40' & sex == '2' ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(combinations_to_drop != 1) %>%
  select(-combinations_to_drop)


## set up groups ------------
inc_C0097 <- inc %>%
  filter(substr(icd_3digit, 1, 1) == 'C') %>%
  group_by(year, sex, age_gp) %>%
  summarise(
    icd = 'C00-C97',
    icd_3digit = 'C00-C97',
    tumourcount = sum(tumourcount),
    .groups = 'drop'
  )

inc_C0097_C44 <- inc %>%
  filter(substr(icd_3digit, 1, 1) == 'C' & icd_3digit != 'C44') %>%
  group_by(year, sex, age_gp) %>%
  summarise(
    icd = 'C00-C97 excl. C44',
    icd_3digit = 'C00-C97 excl. C44',
    tumourcount = sum(tumourcount),
    .groups = 'drop'
  )

inc_all <- inc %>%
  group_by(year, sex, age_gp) %>%
  summarise(
    icd = 'C00-C97, D00-D48',
    icd_3digit = 'C00-C97, D00-D48',
    tumourcount = sum(tumourcount),
    .groups = 'drop'
  )

inc_C0014 <- inc %>%
  filter(icd_3digit %in% sprintf('C%02d', 00:14)) %>%
  group_by(year, sex, age_gp) %>%
  summarise(
    icd = 'C00-C14',
    icd_3digit = 'C00-C14',
    tumourcount = sum(tumourcount),
    .groups = 'drop'
  )

inc_C1820 <- inc %>%
  filter(icd_3digit %in% sprintf('C%02d', 18:20)) %>%
  group_by(year, sex, age_gp) %>%
  summarise(
    icd = 'C18-C20',
    icd_3digit = 'C18-C20',
    tumourcount = sum(tumourcount),
    .groups = 'drop'
  )


inc_C3334 <- inc %>%
  filter(icd_3digit %in% sprintf('C%02d', 33:34)) %>%
  group_by(year, sex, age_gp) %>%
  summarise(
    icd = 'C33-C34',
    icd_3digit = 'C33-C34',
    tumourcount = sum(tumourcount),
    .groups = 'drop'
  )

inc_C5455 <- inc %>%
  filter(icd_3digit %in% sprintf('C%02d', 54:55)) %>%
  group_by(year, sex, age_gp) %>%
  summarise(
    icd = 'C54-C55',
    icd_3digit = 'C54-C55',
    tumourcount = sum(tumourcount),
    .groups = 'drop'
  )

inc_C5657 <- inc %>%
  filter(icd_3digit %in% sprintf('C%02d', 56:57)) %>%
  group_by(year, sex, age_gp) %>%
  summarise(
    icd = 'C56-C57',
    icd_3digit = 'C56-C57',
    tumourcount = sum(tumourcount),
    .groups = 'drop'
  )

inc_C8286 <- inc %>%
  filter(icd_3digit %in% sprintf('C%02d', 82:86)) %>%
  group_by(year, sex, age_gp) %>%
  summarise(
    icd = 'C82-C86',
    icd_3digit = 'C82-C86',
    tumourcount = sum(tumourcount),
    .groups = 'drop'
  )

inc_C9195 <- inc %>%
  filter(icd_3digit %in% sprintf('C%02d', 91:95)) %>%
  group_by(year, sex, age_gp) %>%
  summarise(
    icd = 'C91-C95',
    icd_3digit = 'C91-C95',
    tumourcount = sum(tumourcount),
    .groups = 'drop'
  )
## combine all groups ---------------
inc <- rbind(
  inc,
  inc_all,
  inc_C0097,
  inc_C0097_C44,
  inc_C0014,
  inc_C1820,
  inc_C3334,
  inc_C5455,
  inc_C5657,
  inc_C8286,
  inc_C9195
)
rm(
  inc_all,
  inc_C0097,
  inc_C0097_C44,
  inc_C0014,
  inc_C1820,
  inc_C3334,
  inc_C5455,
  inc_C5657,
  inc_C8286,
  inc_C9195,
  inc_raw
)

## load years population ----------
pop <-
  readRDS(
    paste0(
      ""
    )
  )

names(pop) <- str_to_lower(names(pop))
## merge with the cases --------
inc_std <- base::merge(inc, ESP_pop, by = c('sex', 'age_gp'))
inc_std_pop <-
  base::merge(inc_std, pop, by = c('year', 'sex', 'age_gp'))


## save the data for analysis -----------
saveRDS(
  inc_std_pop,
  file = paste0(
    ""
  )
)
