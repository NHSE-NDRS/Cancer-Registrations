# setup ----
library(dplyr)
library(tidyr)
library(data.table)
library(openxlsx)
library(readstata13)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# lookup files ----
site_code_name <-
  read.dta13(
    ""
  )
site_descriptions <-
  read.csv(
    ""
  )

#read in site group lookup for Sub ICB geographies and icd_3digit field so can merge to sub_ICB data
loc_site_groups <-
  readRDS(
    ""
  ) %>%
  rename (icd10locgroup = icd_3digit)

# this loads the rate functions to be used.
source("rate_functions.R")

first_year <- 2001
last_year <- 2020

# run analysis ----
years <- first_year:last_year

for (year in years) {
  ## load data ----
  mort_eng <-
    readRDS(file = paste0(""))
  
  ## 3 digit ASRs and crude ----
  out_ICD3_digit <- mort_eng %>%
    ASR(
      groups = c('SITE'),
      sex_name = 'SEX',
      age_gp_name = 'FIVEYEARAGEBAND'
    ) %>%
    pivot_longer(
      cols = -c(SITE, SEX, Count, flag),
      names_to = c('type', '.value'),
      names_pattern = "(.*)_(.*)"
    ) %>%
    mutate(
      Geography_type = 'Country',
      Geography_code = 'E92000001',
      Geography_name = 'England',
      type = ifelse(type == 'ASR', 'Age-standardised', 'Non-standardised'),
      age_gp = 'All ages'
    ) %>%
    merge(site_code_name,
          by.x = 'SITE',
          by.y = 'icd10',
          all.x = TRUE) %>%
    merge(site_descriptions,
          by.x = 'SITE',
          by.y = 'icd10',
          all.x = TRUE) %>%
    select(
      ICD10_code = SITE ,
      Site_description = site_description,
      Geography_type,
      Geography_code,
      Geography_name,
      Sex = SEX,
      Age_Group = age_gp,
      Count,
      Type_of_rate = type,
      Rate,
      LCI = LCL,
      UCI = UCL,
      Flag = flag
    )
  
  if (year == last_year) {
    ## 3 digit age specific ----
    age_spec_ICD3_digit <- mort_eng %>%
      AgeSpecificRates(
        groups = c('SITE'),
        sex_name = 'SEX',
        age_gp_name = 'FIVEYEARAGEBAND'
      ) %>%
      mutate(
        Geography_type = 'Country',
        Geography_code = 'E92000001',
        Geography_name = 'England',
        flag = NA
      ) %>%
      merge(site_code_name,
            by.x = 'SITE',
            by.y = 'icd10',
            all.x = TRUE) %>%
      merge(
        site_descriptions,
        by.x = 'SITE',
        by.y = 'icd10',
        all.x = TRUE
      ) %>%
      select(
        ICD10_code = SITE ,
        Site_description = site_description,
        Geography_type,
        Geography_code,
        Geography_name,
        Sex = SEX,
        Age_Group = FIVEYEARAGEBAND,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
      )
    
    ## bind to make tables ----
    table1 <- rbind(out_ICD3_digit, age_spec_ICD3_digit)
    
    ## filter 3 digit to make trends latest year ----
    out <- out_ICD3_digit %>%
      filter(Type_of_rate == 'Age-standardised') %>%
      mutate(Year = year) %>%
      select(Year,
             ICD10_code,
             Site_description,
             Sex,
             Count,
             Rate,
             LCI,
             UCI,
             Flag)
    assign(paste0('table2_', year), out)
    
  } else{
    ## filter 3 digit to make trends not latest year ----
    out <- out_ICD3_digit %>%
      filter(Type_of_rate == 'Age-standardised') %>%
      mutate(Year = year) %>%
      select(Year,
             ICD10_code,
             Site_description,
             Sex,
             Count,
             Rate,
             LCI,
             UCI,
             Flag)
    assign(paste0('table2_', year), out)
  }
  
  if (year >= 2020)
  {
    # table 3 ----
    mort_eng_imd <-
      readRDS(file = paste0(""))
    
    ## 3 digit ASRs and crude ----
    out_ICD3_digit_imd <- mort_eng_imd %>%
      ASR(
        groups = c('SITE', 'IMD_QUINTILE'),
        sex_name = 'SEX',
        age_gp_name = 'FIVEYEARAGEBAND'
      ) %>%
      pivot_longer(
        cols = -c(SITE, IMD_QUINTILE, SEX, Count, flag),
        names_to = c('type', '.value'),
        names_pattern = "(.*)_(.*)"
      ) %>%
      mutate(
        Geography_type = 'Country',
        Geography_code = 'E92000001',
        Geography_name = 'England',
        type = ifelse(type == 'ASR', 'Age-standardised', 'Non-standardised'),
        age_gp = 'All ages'
      ) %>%
      merge(site_code_name,
            by.x = 'SITE',
            by.y = 'icd10',
            all.x = TRUE) %>%
      merge(
        site_descriptions,
        by.x = 'SITE',
        by.y = 'icd10',
        all.x = TRUE
      ) %>%
      select(
        ICD10_code = SITE ,
        Site_description = site_description,
        IMD_quintile = IMD_QUINTILE,
        Sex = SEX,
        Age_Group = age_gp,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag,
      )

    ## bind to make tables ----
    table3 <- out_ICD3_digit_imd
    
    # table 4 ----
    mort_loc <-
      readRDS(file = paste0(""))
    
    out_ICD3_digit_loc <- mort_loc %>%
      filter(SITE %in% loc_site_groups$icd10locgroup) %>%
      ASR(
        groups = c('SITE', 'LOC22_CODE', 'LOC22_NAME'),
        sex_name = 'SEX',
        age_gp_name = 'ALTAGEBAND'
      ) %>%
      pivot_longer(
        cols = -c(SITE, SEX, LOC22_CODE, LOC22_NAME, Count, flag),
        names_to = c('type', '.value'),
        names_pattern = "(.*)_(.*)"
      ) %>%
      mutate(
        type = ifelse(type == 'ASR', 'Age-standardised', 'Non-standardised'),
        age_gp = 'All ages'
      ) %>%
      merge(site_code_name,
            by.x = 'SITE',
            by.y = 'icd10',
            all.x = TRUE) %>%
      merge(
        site_descriptions,
        by.x = 'SITE',
        by.y = 'icd10',
        all.x = TRUE
      ) %>%
      select(
        ICD10_code = SITE ,
        Site_description = site_description,
        Sub_ICB_Code = LOC22_CODE,
        Sub_ICB_Name = LOC22_NAME,
        Sex = SEX,
        Age_Group = age_gp,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
      )
    
    age_spec_ICD3_digit_loc <- mort_loc %>%
      filter(SITE %in% loc_site_groups$icd10locgroup) %>%
      AgeSpecificRates(
        groups = c('SITE', 'LOC22_CODE', 'LOC22_NAME'),
        sex_name = 'SEX',
        age_gp_name = 'ALTAGEBAND'
      ) %>%
      mutate(flag = NA) %>%
      merge(site_code_name,
            by.x = 'SITE',
            by.y = 'icd10',
            all.x = TRUE) %>%
      merge(
        site_descriptions,
        by.x = 'SITE',
        by.y = 'icd10',
        all.x = TRUE
      ) %>%
      select(
        ICD10_code = SITE ,
        Site_description = site_description,
        Sub_ICB_Code = LOC22_CODE,
        Sub_ICB_Name = LOC22_NAME,
        Sex = SEX,
        Age_Group = ALTAGEBAND,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
      )
    
    ## bind to make tables ----
    table4 <- rbind(out_ICD3_digit_loc, age_spec_ICD3_digit_loc)
  }
}

## bind trends to make table 2 ----
table2 <- rbindlist(mget(paste0('table2_', years)))

# format tables and values ----
table1 <- table1 %>%
  mutate(
    Rate = ifelse(is.na(Rate), '[u1]', sprintf('%0.1f', Rate)),
    LCI = ifelse(is.na(LCI), '[u1]', sprintf('%0.1f', LCI)),
    UCI = ifelse(is.na(UCI), '[u1]', sprintf('%0.1f', UCI)),
    Sex = ifelse(Sex == '1', 'Males', 'Females')
  ) %>%
  mutate(
    ICD10_code = ifelse(
      str_length(ICD10_code) == 4,
      gsub('^(.{3})(.*)$', '\\1\\.\\2', ICD10_code),
      ICD10_code
    ),
    Age_Group = case_when(
      Age_Group == '00-00' ~ 'Under 1',
      Age_Group == '01-04' ~ '1 to 4',
      Age_Group == '05-09' ~ '5 to 9',
      Age_Group == '90plus' ~ '90 and over',
      T ~ str_replace(Age_Group, '-', ' to ')
    )
  ) %>%
  mutate(Flag = ifelse(Rate == '[u1]', '[note2]', Flag)) %>%
  mutate(Flag = ifelse (Type_of_rate == 'Non-standardised' &
                          Flag == '[note1]', NA, Flag)) %>%
  mutate(ICD10_code = str_replace_all(ICD10_code, "-", " to "))

table2 <- table2 %>%
  mutate(
    Rate = ifelse(is.na(Rate), '[u1]', sprintf('%0.1f', Rate)),
    LCI = ifelse(is.na(LCI), '[u1]', sprintf('%0.1f', LCI)),
    UCI = ifelse(is.na(UCI), '[u1]', sprintf('%0.1f', UCI)),
    Sex = ifelse(Sex == '1', 'Males', 'Females')
  ) %>%
  mutate(Flag = str_replace_all(Flag, "u", "u2")) %>%
  mutate(ICD10_code = ifelse(
    str_length(ICD10_code) == 4,
    gsub('^(.{3})(.*)$', '\\1\\.\\2', ICD10_code),
    ICD10_code
  )) %>%
  mutate(Flag = ifelse(Rate == '[u1]', '[note2]', Flag)) %>%
  mutate(ICD10_code = str_replace_all(ICD10_code, "-", " to "))

table3 <- table3 %>%
  mutate(
    Rate = ifelse(is.na(Rate), '[u1]', sprintf('%0.1f', Rate)),
    LCI = ifelse(is.na(LCI), '[u1]', sprintf('%0.1f', LCI)),
    UCI = ifelse(is.na(UCI), '[u1]', sprintf('%0.1f', UCI)),
    Sex = ifelse(Sex == '1', 'Males', 'Females')
  ) %>%
  mutate(
    ICD10_code = ifelse(
      str_length(ICD10_code) == 4,
      gsub('^(.{3})(.*)$', '\\1\\.\\2', ICD10_code),
      ICD10_code
    ),
    Age_Group = case_when(
      Age_Group == '00-00' ~ 'Under 1',
      Age_Group == '01-04' ~ '1 to 4',
      Age_Group == '05-09' ~ '5 to 9',
      Age_Group == '90plus' ~ '90 and over',
      T ~ str_replace(Age_Group, '-', ' to ')
    )
  ) %>%
  mutate(Flag = ifelse(Rate == '[u1]', '[note2]', Flag)) %>%
  mutate(Flag = ifelse (Type_of_rate == 'Non-standardised' &
                          Flag == '[note1]', NA, Flag)) %>%
  mutate(ICD10_code = str_replace_all(ICD10_code, "-", " to "))

table4 <- table4 %>%
  mutate(
    Rate = ifelse(is.na(Rate), '[u1]', sprintf('%0.1f', Rate)),
    LCI = ifelse(is.na(LCI), '[u1]', sprintf('%0.1f', LCI)),
    UCI = ifelse(is.na(UCI), '[u1]', sprintf('%0.1f', UCI)),
    Sex = ifelse(Sex == '1', 'Males', 'Females')
  ) %>%
  mutate(
    ICD10_code = ifelse(
      str_length(ICD10_code) == 4,
      gsub('^(.{3})(.*)$', '\\1\\.\\2', ICD10_code),
      ICD10_code
    ),
    Age_Group = case_when(
      Age_Group == '00-00' ~ 'Under 1',
      Age_Group == '01-04' ~ '1 to 4',
      Age_Group == '05-09' ~ '5 to 9',
      Age_Group == '90plus' ~ '90 and over',
      T ~ str_replace(Age_Group, '-', ' to ')
    )
  ) %>%
  mutate(Flag = ifelse(Rate == '[u1]', '[note2]', Flag)) %>%
  mutate(Flag = ifelse (Type_of_rate == 'Non-standardised' &
                          Flag == '[note1]', NA, Flag)) %>%
  mutate(ICD10_code = str_replace_all(ICD10_code, "-", " to "))


# save data to compare ----
save.dta13(table1, file = "")
save.dta13(table2, file = "")
save.dta13(table3, file = "")
save.dta13(table4, file = "")

# load excel workbook ----
wb <-
  loadWorkbook("")
sheets(wb)
## write data to template ----
writeData(
  wb,
  x = table1,
  sheet = "Table_1",
  startRow = 4,
  colNames = F
)
writeData(
  wb,
  x = table2,
  sheet = "Table_2",
  startRow = 4,
  colNames = F
)
writeData(
  wb,
  x = table3,
  sheet = "Table_3",
  startRow = 4,
  colNames = F
)
writeData(
  wb,
  x = table4,
  sheet = "Table_4",
  startRow = 4,
  colNames = F
)

## save workbook ----
saveWorkbook(wb, file = "", overwrite = TRUE)
