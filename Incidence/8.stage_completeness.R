# setup ----
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(readstata13)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

first_year <- 2013
last_year <- 2020


# loop through all years ----
years <- first_year:last_year
for (year in years) {
  inc_raw <-
    readRDS(
      file = paste0(
        ""
      )
    )
  
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
  inc <- inc_raw %>% mutate(
    age_gp = case_when(
      age == 0 ~ '00-00',
      age >= 1 &
        age <= 4 ~ '01-04',
      age >= 5 &
        age <= 9 ~ '05-09',
      age >= 10 &
        age <= 14 ~ '10-14',
      age >= 15 &
        age <= 19 ~ '15-19',
      age >= 20 &
        age <= 24 ~ '20-24',
      age >= 25 &
        age <= 29 ~ '25-29',
      age >= 30 &
        age <= 34 ~ '30-34',
      age >= 35 &
        age <= 39 ~ '35-39',
      age >= 40 &
        age <= 44 ~ '40-44',
      age >= 45 &
        age <= 49 ~ '45-49',
      age >= 50 &
        age <= 54 ~ '50-54',
      age >= 55 &
        age <= 59 ~ '55-59',
      age >= 60 &
        age <= 64 ~ '60-64',
      age >= 65 &
        age <= 69 ~ '65-69',
      age >= 70 &
        age <= 74 ~ '70-74',
      age >= 75 &
        age <= 79 ~ '75-79',
      age >= 80 &
        age <= 84 ~ '80-84',
      age >= 85 &
        age <= 89 ~ '85-89',
      age >= 90  ~ '90plus'
    ),
    age_gp_CCG = case_when(
      age >= 0 & age <= 24 ~ '00-24',
      age >= 25 &
        age <= 49 ~ '25-49',
      age >= 50 &
        age <= 59 ~ '50-59',
      age >= 60 &
        age <= 69 ~ '60-69',
      age >= 70 &
        age <= 79 ~ '70-79',
      age >= 80  ~ '80plus'
    ),
    stage_best = case_when(
      stage_best == 'A' ~ '1',
      stage_best == 'B' ~ '2',
      stage_best == 'C' ~ '3',
      substr(stage_best, 1, 1) == 0 &
        icd10code == "C911" ~ '1',
      stage_best != '' &
        !(stage_best %in% c('A', 'B', 'C')) ~ substr(stage_best, 1, 1)
    )
  ) %>%
    mutate(
      stage_catergory = case_when(
        stage_best_system == 'Binet' &
          substr(stage_best, 1, 1) %in% c('A', 'B') &
          stage_pi_detail == 'Y' ~ 'Stage 1 & 2',
        #Binet
        stage_best_system == 'Binet' &
          substr(stage_best, 1, 1) == ('C') &
          stage_pi_detail == 'Y' ~ 'Stage 3 & 4',
        #Binet
        stage_best_system == 'INRG' &
          substr(stage_best, 1, 1) == 'L' &
          stage_pi_detail == 'Y' ~ 'Stage 1 & 2',
        #INRG
        stage_best_system == 'INRG' &
          substr(stage_best, 1, 1) == 'M' &
          stage_pi_detail == 'Y' ~ 'Stage 3 & 4',
        #INRG
        stage_best_system == 'Wilms' &
          substr(stage_best, 1, 1) == '5' &
          stage_pi_detail == 'Y' ~ 'Stage 3 & 4',
        #Wilms
        stage_best_system == 'Chang' &
          substr(stage_best, 1, 2) %in% c('M0') &
          stage_pi_detail == 'Y' ~ 'Stage 1 & 2',
        #Chang
        stage_best_system == 'Chang' &
          substr(stage_best, 1, 2) %in% c('M1', 'M2', 'M3', 'M4') &
          stage_pi_detail == 'Y' ~ 'Stage 3 & 4',
        #Chang
        stage_best %in% c('1', '2') &
          stage_pi_detail == 'Y' ~ 'Stage 1 & 2',
        stage_best %in% c('3', '4') &
          stage_pi_detail == 'Y' ~ 'Stage 3 & 4',
        stage_best %in% c('1', '2', '3', '4') &
          stage_pi_detail == 'N' ~ 'Missing',!(stage_best %in% c('1', '2', '3', '4')) &
          stage_pi_detail %in% c('Y', 'N') ~ 'Missing',
        (stage_pi_detail %in% c('U', 'NA')) ~ 'Unstageable',
        stage_pi_detail == "" ~ 'Data quality issues',
        is.na(stage_pi_detail) ~ 'Data quality issues'
      )
    ) %>%
    mutate(
      stage_catergory = ifelse(
        stage_catergory == 'Data quality issues' &
          icd10code_3char == 'C83',
        'Unstageable',
        stage_catergory
      )
    )
  
  inc <- inc %>%
    mutate(
      stage_group = case_when(
        icd10code_3char == 'C15' |
          icd10code == 'C160' ~ 'Oesophagus including cardia and gastroesophageal junction',
        icd10code_3char == 'C16' ~ 'Stomach excluding cardia and gastroesophageal junction',
        icd10code_3char == 'C18' ~ 'Colon',
        icd10code_3char %in% sprintf('C%s', 19:20) ~ 'Rectum and rectosigmoid junction',
        icd10code_3char == 'C25' ~ 'Pancreas',
        icd10code_3char == 'C34' ~ 'Lung',
        icd10code_3char == 'C43' ~ 'Melanoma of skin',
        icd10code_3char == 'C50' ~ "Breast",
        icd10code_3char == 'C53' ~ 'Cervix',
        icd10code_3char %in% sprintf('C%s', 54:55) ~ 'Uterus',
        (icd10code_3char %in% c('C56', 'C57') &
           !(icd10code %in% c(
             'C577', 'C578', 'C579'
           )))  ~ 'Ovary and fallopian tube',
        icd10code_3char == 'C61' ~ 'Prostate',
        icd10code_3char == 'C62' ~ 'Testis',
        icd10code_3char == 'C64' ~ 'Kidney, except renal pelvis',
        icd10code_3char == 'C67' ~ 'Bladder',
        icd10code_3char == 'C73' ~ 'Thyroid',
        icd10code_3char == 'C81' ~ 'Hodgkin lymphoma',
        icd10code_3char == 'C32' |
          icd10code == 'C101' ~ 'Larynx including anterior surface of epiglottis',
        icd10code_3char %in% c('C01', 'C09') |
          icd10code %in% c(
            'C024',
            'C051',
            'C052',
            'C100',
            'C102',
            'C103',
            'C104',
            'C108',
            'C109'
          ) ~ 'Oropharynx, base of tongue, tonsil, soft palate and uvula',
        icd10code_3char %in% c('C03', 'C04', 'C06') |
          icd10code %in% c(
            'C003',
            'C004',
            'C005',
            'C020',
            'C021',
            'C022',
            'C023',
            'C028',
            'C029',
            'C050'
          ) ~ 'Oral cavity, hard palate and lip (inner aspect)',
        icd10code_3char %in% sprintf('C%s', 82:86) ~ 'Non-Hodgkin lymphoma',
        TRUE ~ ''
      )
    )
  
  
  data <- inc %>%
    group_by(stage_group, stage_catergory) %>%
    summarise(yydx = year, count = n()) %>%
    pivot_wider(
      id_cols = c(stage_group, yydx),
      names_from = stage_catergory,
      values_from = count
    ) %>%
    filter(stage_group != '')
  assign(paste0('stage_', year), data)
}

stage <- rbindlist(mget(paste0('stage_', years)))
names(stage) <- str_remove_all(names(stage), '&')
names(stage) <- str_replace_all(names(stage), ' ', '')

stage_completeness <- stage %>%
  mutate(
    total = ifelse(is.na(Missing), 0, Missing) + ifelse(is.na(Stage12), 0, Stage12) + ifelse(is.na(Stage34), 0, Stage34),
    staged = ifelse(is.na(Stage12), 0, Stage12) + ifelse(is.na(Stage34), 0, Stage34)
  ) %>%
  mutate(completeness = round(100 * staged / total, 1)) %>%
  select(stage_group, yydx, completeness) %>%
  pivot_wider(id_cols = stage_group,
              names_from = yydx,
              values_from = completeness)

stage_completeness_1820 <- stage %>%
  filter(yydx >= 2018) %>%
  group_by(stage_group) %>%
  summarise(
    Missing = sum(Missing, na.rm = T),
    Stage12 = sum(Stage12, na.rm = T),
    Stage34 = sum(Stage34, na.rm = T),
    Unstageable = sum(Unstageable, na.rm = T)
  ) %>%
  mutate(
    total = ifelse(is.na(Missing), 0, Missing) + ifelse(is.na(Stage12), 0, Stage12) + ifelse(is.na(Stage34), 0, Stage34) ,
    staged = ifelse(is.na(Stage12), 0, Stage12) + ifelse(is.na(Stage34), 0, Stage34)
  ) %>%
  mutate(`2018-2020` = round(100 * staged / total, 1)) %>%
  select(stage_group, `2018-2020`)

stage_completeness_all <-
  merge(stage_completeness, stage_completeness_1820)

fwrite(stage_completeness_all, file = "")
