# setup ----
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(readstata13)

#run R population extraction file before this code
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

first_year <- 2001
last_year <- 2020


## load standard pop ---------------
ESP_pop <- fread("")
ESP_pop$agegroup[ESP_pop$agegroup == "0"] <- "00-00"
ESP_pop$sex <-
  factor(ESP_pop$sex ,
         levels = c('Male', 'Female'),
         labels = c('1', '2'))
names(ESP_pop)[2:3] <- c('standard', 'age_gp')

icd4digit_sites <-
  read.dta13("")
# loop through all years ----
pb <- winProgressBar(
  label = "0% done",
  min = 0,
  max = 100,
  initial = 0
)
years <- first_year:last_year
for (year in years) {
  inc_raw <-
    readRDS(file = paste0(""))
  
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
  inc <- inc %>% mutate(
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
    
    # stage_category updated  August 2022, updated Chang where M0 is only early stage code
    mutate(
      stage_category = case_when(
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
          stage_pi_detail == 'N' ~ 'Missing',
        !(stage_best %in% c('1', '2', '3', '4')) &
          stage_pi_detail %in% c('Y', 'N') ~ 'Missing',
        (stage_pi_detail %in% c('U', 'NA')) ~ 'Unstageable',
        stage_pi_detail == "" ~ 'Data quality issues',
        is.na(stage_pi_detail) ~ 'Data quality issues'
      )
    ) %>%
    mutate(
      stage_category = ifelse(
        stage_category == 'Data quality issues' &
          icd10code_3char == 'C83',
        'Unstageable',
        stage_category
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
  
  
  ## add in stage categories ----
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
  
  ## collapse data down --------------------
  inc <- inc %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      icd,
      stage_category,
      stage_group,
      #hormone_status,gleason,
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(tumourcount = sum(release), .groups = 'drop')
  
  inc$stage_category <-
    factor(
      inc$stage_category,
      levels = c(
        'Stage 1 & 2',
        'Stage 3 & 4',
        'Missing',
        'Unstageable',
        'Data quality issues'
      )
    )
  
  inc_staged <- inc %>%
    filter(stage_group != '') %>%
    complete(nesting(icd, stage_group),
             stage_category,
             fill = list(tumourcount = 0))
  
  inc_unstaged <- inc %>%
    filter(stage_group == '') %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      icd,
      stage_group,
      #hormone_status,gleason,
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      stage_category = '',
      tumourcount = sum(tumourcount, na.rm = T),
      .groups = 'drop'
    )
  
  inc <- rbind(inc_staged, inc_unstaged)
  
  inc$stage_category <- as.character(inc$stage_category)
  
  rm(inc_staged, inc_unstaged)
  ## complete the data ----
  inc <- inc %>%
    complete(
      nesting(icd, stage_category, stage_group),
      nesting(gor_code, gor_name, ccg21cd, ccg21nm, loc22cd, loc22nm),
      nesting(age_gp, age_gp_CCG),
      sex,
      imd_quintile,
      fill = list(tumourcount = 0)
    ) %>%
    mutate(icd_3digit = ifelse(
      icd %in% c("D352", "D353", "D354"),
      as.character(icd),
      substr(icd, 1, 3)
    ))
  
  if (year == last_year) {
    inc <- inc %>%
      drop_na(
        gor_code,
        gor_name,
        ccg21cd,
        ccg21nm,
        loc22cd,
        loc22nm,
        age_gp,
        age_gp_CCG,
        sex,
        imd_quintile
      )
  } else {
    inc <- inc %>%
      drop_na(gor_code,
              gor_name,
              ccg21cd,
              ccg21nm,
              loc22cd,
              loc22nm,
              age_gp,
              age_gp_CCG,
              sex)
  }
  
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
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      stage_category,
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      icd = 'C00-C97',
      icd_3digit = 'C00-C97',
      stage_group = '',
      tumourcount = sum(tumourcount),
      .groups = 'drop'
    )
  
  inc_C0097_C44 <- inc %>%
    filter(substr(icd_3digit, 1, 1) == 'C' &
             icd_3digit != 'C44') %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      stage_category,
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      icd = 'C00-C97 excl. C44',
      icd_3digit = 'C00-C97 excl. C44',
      stage_group = '',
      tumourcount = sum(tumourcount),
      .groups = 'drop'
    )
  
  inc_all <- inc %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      stage_category,
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      icd = 'C00-C97, D00-D48',
      icd_3digit = 'C00-C97, D00-D48',
      stage_group = '',
      tumourcount = sum(tumourcount),
      .groups = 'drop'
    )
  
  inc_C0014 <- inc %>%
    filter(icd_3digit %in% sprintf('C%02d', 00:14)) %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      stage_category,
      
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      icd = 'C00-C14',
      icd_3digit = 'C00-C14',
      stage_group = '',
      tumourcount = sum(tumourcount),
      .groups = 'drop'
    )
  
  inc_C1820 <- inc %>%
    filter(icd_3digit %in% sprintf('C%02d', 18:20)) %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      stage_category,
      
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      icd = 'C18-C20',
      icd_3digit = 'C18-C20',
      stage_group = '',
      tumourcount = sum(tumourcount),
      .groups = 'drop'
    )
  
  
  inc_C3334 <- inc %>%
    filter(icd_3digit %in% sprintf('C%02d', 33:34)) %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      stage_category,
      
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      icd = 'C33-C34',
      icd_3digit = 'C33-C34',
      stage_group = '',
      tumourcount = sum(tumourcount),
      .groups = 'drop'
    )
  
  inc_C5455 <- inc %>%
    filter(icd_3digit %in% sprintf('C%02d', 54:55)) %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      stage_category,
      
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      icd = 'C54-C55',
      icd_3digit = 'C54-C55',
      stage_group = '',
      tumourcount = sum(tumourcount),
      .groups = 'drop'
    )
  
  inc_C5657 <- inc %>%
    filter(icd_3digit %in% sprintf('C%02d', 56:57)) %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      stage_category,
      
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      icd = 'C56-C57',
      icd_3digit = 'C56-C57',
      stage_group = '',
      tumourcount = sum(tumourcount),
      .groups = 'drop'
    )
  
  inc_C8286 <- inc %>%
    filter(icd_3digit %in% sprintf('C%02d', 82:86)) %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      stage_category,
      
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      icd = 'C82-C86',
      icd_3digit = 'C82-C86',
      stage_group = '',
      tumourcount = sum(tumourcount),
      .groups = 'drop'
    )
  
  inc_C9195 <- inc %>%
    filter(icd_3digit %in% sprintf('C%02d', 91:95)) %>%
    group_by(
      sex,
      age_gp,
      age_gp_CCG,
      imd_quintile,
      stage_category,
      
      gor_code,
      gor_name,
      ccg21cd,
      ccg21nm,
      loc22cd,
      loc22nm
    ) %>%
    summarise(
      icd = 'C91-C95',
      icd_3digit = 'C91-C95',
      stage_group = '',
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
    readRDS(paste0(""))
  
  pop <- pop %>%
    mutate(age = ifelse(age == '90+', '90plus', age)) %>%
    rename(
      country = geography,
      gor_code = rgn,
      ccg21cd = ccg,
      loc22cd = loc22cd,
      age_gp = age
    )
  
  ## merge with the cases --------
  inc_std <- base::merge(inc, ESP_pop, by = c('sex', 'age_gp'))
  inc_std_pop <-
    base::merge(
      inc_std,
      pop,
      by = c(
        'sex',
        'age_gp',
        'gor_code',
        'ccg21cd',
        'loc22cd',
        'imd_quintile'
      )
    )
  
  saveRDS(inc_std_pop,
          file = paste0(""))
  
  
  # create files for tables other than the subicb geographies
  inc_eng_gor <- inc_std_pop %>%
    group_by(
      year,
      sex,
      age_gp,
      imd_quintile,
      icd,
      icd_3digit,
      stage_group,
      stage_category,
      
      gor_code,
      gor_name
    ) %>%
    summarise(
      tumourcount = sum(tumourcount),
      population = sum(population),
      standard = max(standard),
      .groups = 'drop'
    )
  
  ## save the data for analysis -----------
  saveRDS(inc_eng_gor,
          file = paste0(""))
  rm(inc_eng_gor)
  
  
  ##create files to export for use in r shiny app
  
  inc_std_pop <-
    readRDS(paste0(""))
  
  inc_eng <- inc_std_pop %>%
    group_by(year,
             sex,
             age_gp,
             icd,
             icd_3digit,
             stage_group,
             stage_category,
             
             standard) %>%
    summarise(
      tumourcount = sum(tumourcount),
      population = sum(population),
      .groups = 'drop'
    ) %>%
    group_by(year, sex, age_gp, icd_3digit, standard, population) %>%
    summarise(tumourcount = sum(tumourcount),
              .groups = 'drop')
  
  #by ccg for 2019 data now micing to loc22 below, left in code for comparision for now
  inc_CCG <- inc_std_pop %>%
    group_by(
      year,
      sex,
      age_gp,
      age_gp_CCG,
      icd,
      icd_3digit,
      stage_group,
      stage_category,
      
      ccg21cd,
      ccg21nm,
      standard
    ) %>%
    summarise(
      tumourcount = sum(tumourcount),
      population = sum(population),
      .groups = 'drop'
    ) %>%
    group_by(year,
             sex,
             age_gp,
             age_gp_CCG,
             icd_3digit,
             ccg21cd,
             ccg21nm,
             standard,
             population) %>%
    summarise(tumourcount = sum(tumourcount),
              .groups = 'drop') %>%
    group_by(year, sex, age_gp_CCG, icd_3digit, ccg21cd, ccg21nm) %>%
    summarise(
      tumourcount = sum(tumourcount),
      population = sum(population),
      standard = sum(standard),
      .groups = 'drop'
    )
  
  #replacing CCG FOR 2020 registrations, this file will also be used in calculations code
  inc_LOC <- inc_std_pop %>%
    group_by(
      year,
      sex,
      age_gp,
      age_gp_CCG,
      icd,
      icd_3digit,
      stage_group,
      stage_category,
      
      loc22cd,
      loc22nm,
      standard
    ) %>%
    summarise(
      tumourcount = sum(tumourcount),
      population = sum(population),
      .groups = 'drop'
    ) %>%
    group_by(year,
             sex,
             age_gp,
             age_gp_CCG,
             icd_3digit,
             loc22cd,
             loc22nm,
             standard,
             population) %>%
    summarise(tumourcount = sum(tumourcount),
              .groups = 'drop') %>%
    group_by(year, sex, age_gp_CCG, icd_3digit, loc22cd, loc22nm) %>%
    summarise(
      tumourcount = sum(tumourcount),
      population = sum(population),
      standard = sum(standard),
      .groups = 'drop'
    )
  saveRDS(inc_eng,
          file = paste0(""))
  saveRDS(inc_CCG,
          file = paste0(""))
  saveRDS(inc_LOC,
          file = paste0(""))
  rm(inc_eng, inc_CCG, inc_LOC, inc, inc_std, inc_std_pop)
  
  info <- sprintf("%d%% done", round(((year - 2012) / 8) * 100))
  setWinProgressBar(pb, round(((year - 2012) / 8) * 100), label = info)
  
}
close(pb)
