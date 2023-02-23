# setup ----
library(dplyr)
library(tidyr)
library(data.table)
library(openxlsx)
library(stringr)
library(readstata13)
library(tidyxl)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# lookup files ----
site_code_name <- read.csv("")

stage_codes <- read.csv("")
names(stage_codes)[1] <- "Site_name"

#read in site group lookup for Sub ICB geographies and icd_3digit field so can merge to sub_ICB data
loc_site_groups <- readRDS("") %>%
  rename (icd10locgroup = icd_3digit)

region_code_name <- fread("")


# this loads the rate functions to be used.
source("rate_functions.R")

first_year <- 2001
last_year <- 2020

# run analysis ----
years <- first_year:last_year


for (year in years) {
  ## load data ----
  inc_gor <- readRDS(paste0(""))
  
  #correction of a typo
  colnames(inc_gor)[8] <- "stage_category"
  
  # need to create a England data set so there isn't any problems with the functions
  inc_eng_imd <- inc_gor %>%
    group_by(sex,
             age_gp,
             icd,
             imd_quintile,
             stage_category,
             icd_3digit,
             stage_group,
             standard) %>%
    summarise(
      tumourcount = sum(tumourcount),
      population = sum(population),
      .groups = 'drop'
    )
  
  inc_eng <- inc_gor %>%
    group_by(sex,
             age_gp,
             icd,
             stage_category,
             icd_3digit,
             stage_group,
             standard) %>%
    summarise(
      tumourcount = sum(tumourcount),
      population = sum(population),
      .groups = 'drop'
    )
  
  inc_gor <- inc_gor %>%
    group_by(
      sex,
      age_gp,
      gor_code,
      gor_name,
      icd,
      stage_category,
      icd_3digit,
      stage_group,
      standard
    ) %>%
    summarise(
      tumourcount = sum(tumourcount),
      population = sum(population),
      .groups = 'drop'
    )
  
  
  #read in loc file (subICB geograohy in from categorisation)
  inc_loc <- readRDS(paste0(""))
  
  
  ## 3 digit ASRs and crude ----
  out_ICD3_digit <- inc_eng %>%
    ASR(
      groups = c('icd_3digit'),
      pop_name = 'population',
      cases_name = 'tumourcount',
      std_name = 'standard'
    ) %>%
    pivot_longer(
      cols = -c(icd_3digit, sex, Count, flag),
      names_to = c('type', '.value'),
      names_pattern = "(.*)_(.*)"
    ) %>%
    mutate(
      Geography_type = 'Country',
      Geography_code = 'E92000001',
      Geography_name = 'England',
      IMD_Quintile = 'All',
      type = ifelse(type == 'ASR', 'Age-standardised', 'Non-standardised'),
      age_gp = 'All ages'
    ) %>%
    merge(site_code_name, by.x = 'icd_3digit', by.y = 'icd10') %>%
    mutate(classification = ifelse(icd_3digit == 'C86', 'New code ICD10r4', '')) %>%
    select(
      ICD10_code = icd_3digit ,
      Site_description = site_description,
      Geography_type,
      Geography_code,
      Geography_name,
      IMD_Quintile,
      Sex = sex,
      Age_at_Diagnosis = age_gp,
      Count,
      Type_of_rate = type,
      Rate,
      LCI = LCL,
      UCI = UCL,
      Flag = flag,
      Classification_flag = classification
    )
  
  
  if (year == last_year) {
    ## 3 digit age specific ----
    age_spec_ICD3_digit <- inc_eng %>%
      AgeSpecificRates(
        groups = c('icd_3digit'),
        pop_name = 'population',
        cases_name = 'tumourcount'
      ) %>%
      mutate(
        Geography_type = 'Country',
        Geography_code = 'E92000001',
        Geography_name = 'England',
        IMD_Quintile = 'All',
        flag = NA
      ) %>%
      merge(site_code_name, by.x = 'icd_3digit', by.y = 'icd10') %>%
      select(
        ICD10_code = icd_3digit ,
        Site_description = site_description,
        Geography_type,
        Geography_code,
        Geography_name,
        IMD_Quintile,
        Sex = sex,
        Age_at_Diagnosis = age_gp,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
      )
    
    ## 3 digit ASRs and crude gor ----
    out_ICD3_digit_gor <- inc_gor %>%
      ASR(
        groups = c('icd_3digit', 'gor_code'),
        pop_name = 'population',
        cases_name = 'tumourcount',
        std_name = 'standard'
      ) %>%
      pivot_longer(
        cols = -c(icd_3digit, gor_code, sex, Count, flag),
        names_to = c('type', '.value'),
        names_pattern = "(.*)_(.*)"
      ) %>%
      mutate(
        Geography_type = 'Region',
        IMD_Quintile = 'All',
        type = ifelse(type == 'ASR', 'Age-standardised', 'Non-standardised'),
        age_gp = 'All ages'
      ) %>%
      filter(type != 'Non-standardised') %>%
      merge(region_code_name, ., by.y = 'gor_code', by.x = 'Geography_code') %>%
      merge(site_code_name, by.x = 'icd_3digit', by.y = 'icd10') %>%
      select(
        ICD10_code = icd_3digit ,
        Site_description = site_description,
        Geography_type,
        Geography_code,
        Geography_name,
        IMD_Quintile,
        Sex = sex,
        Age_at_Diagnosis = age_gp,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
      )
    
    ## 3 digit ASRs and crude deprivation ----
    out_ICD3_digit_dep <- inc_eng_imd %>%
      ASR(
        groups = c('icd_3digit', 'imd_quintile'),
        pop_name = 'population',
        cases_name = 'tumourcount',
        std_name = 'standard'
      ) %>%
      pivot_longer(
        cols = -c(icd_3digit, sex, imd_quintile, Count, flag),
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
      merge(site_code_name, by.x = 'icd_3digit', by.y = 'icd10') %>%
      select(
        ICD10_code = icd_3digit ,
        Site_description = site_description,
        Geography_type,
        Geography_code,
        Geography_name,
        IMD_Quintile = imd_quintile,
        Sex = sex,
        Age_at_Diagnosis = age_gp,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
      )
    
    ## 4 digit ASRs and crude ----
    asr_ICD4_digit <- inc_eng %>%
      ASR(
        groups = c('icd'),
        pop_name = 'population',
        cases_name = 'tumourcount',
        std_name = 'standard'
      ) %>%
      pivot_longer(
        cols = -c(icd, sex, Count, flag),
        names_to = c('type', '.value'),
        names_pattern = "(.*)_(.*)"
      ) %>%
      mutate(
        type = ifelse(type == 'ASR', 'Age-standardised', 'Non-standardised'),
        age_gp = 'All ages'
      ) %>%
      merge(site_code_name, by.x = 'icd', by.y = 'icd10') %>%
      select(
        ICD10_code = icd ,
        Site_description = site_description,
        Sex = sex,
        Age_at_Diagnosis = age_gp,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
      )
    
    ## 4 digit age specific ----
    age_spec_ICD4_digit <- inc_eng %>%
      AgeSpecificRates(
        groups = c('icd'),
        pop_name = 'population',
        cases_name = 'tumourcount'
      ) %>%
      mutate(flag = NA) %>%
      merge(site_code_name, by.x = 'icd', by.y = 'icd10') %>%
      select(
        ICD10_code = icd,
        Site_description = site_description,
        Sex = sex,
        Age_at_Diagnosis = age_gp,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
      )
    
    ## 3 digit ASRs and crude stage ----
    out_ICD3_digit_stage <- inc_eng %>%
      filter(stage_group != '') %>%
      ASR(
        groups = c('stage_group', 'stage_category'),
        pop_name = 'population',
        cases_name = 'tumourcount',
        std_name = 'standard'
      ) %>%
      pivot_longer(
        cols = -c(stage_group, stage_category, sex, Count, flag),
        names_to = c('type', '.value'),
        names_pattern = "(.*)_(.*)"
      ) %>%
      mutate(type = ifelse(type == 'ASR', 'Age-standardised', 'Non-standardised')) %>%
      merge(stage_codes, by.x = 'stage_group', by.y = 'Site_Group') %>%
      filter(!(stage_group == 'Ovary and fallopian tube' &
                 sex == '1')) %>%
      filter(stage_category != 'Data quality issues') %>%
      select(
        ICD10_code = Site_TNM_version_8,
        Site_Group = stage_group ,
        Sex = sex,
        Stage_at_diagnosis = stage_category,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
      )
    ## 3 digit ASRs breakdwon by LOC (subICB) (stage code used as template) ----
    out_ICD3_digit_loc <- inc_loc %>%
      rename(age_gp = age_gp_CCG) %>%
      filter(loc22cd != '') %>%
      ASR(
        groups = c('loc22cd', 'loc22nm', 'icd_3digit'),
        pop_name = 'population',
        cases_name = 'tumourcount',
        std_name = 'standard'
      ) %>%
      pivot_longer(
        cols = -c(loc22cd, loc22nm, icd_3digit, sex, Count, flag),
        names_to = c('type', '.value'),
        names_pattern = "(.*)_(.*)"
      ) %>%
      #create an age_gp field where all avlues are All ages
      mutate(age_gp = 'All ages') %>%
      mutate(type = ifelse(type == 'ASR', 'Age-standardised', 'Non-standardised')) %>%
      merge(loc_site_groups, by.x = 'icd_3digit', by.y = 'icd10locgroup') %>%
      filter(site_description != '') %>% #remove all sites which aren't in the lookup we joined to
      filter(!(icd_3digit == 'C56-C57' &
                 sex == '1')) %>% 
      #     merge to site grouping which match the R shiny app on cancerdata
      
      #      mutate(classification = ifelse(icd_3digit == 'C86','New code ICD10r4','')) %>% #do we need this field not in stage but is in all england, if so need to include in select as well
      select(
        ICD10_code = icd_3digit,
        Site_description = site_description,
        Sex = sex,
        Age_at_Diagnosis = age_gp,
        loc22cd,
        loc22nm,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
        #Classification = classification
      )
    
    ## 3 digit age specific for SUB ICB ----
    age_spec_ICD3_loc <- inc_loc %>%
      rename(age_gp = age_gp_CCG) %>%
      filter(loc22cd != '') %>%
      AgeSpecificRates(
        groups = c('icd_3digit', 'loc22cd', 'loc22nm'),
        pop_name = 'population',
        cases_name = 'tumourcount'
      ) %>%
      mutate(flag = NA) %>%
      merge(loc_site_groups, by.x = 'icd_3digit', by.y = 'icd10locgroup') %>%
      filter(site_description != '') %>% #remove all site which aren't in the lookup we joined to
      filter(!(icd_3digit == 'C56-C57' &
                 sex == '1')) %>% 
      select(
        ICD10_code = icd_3digit,
        Site_description = site_description,
        Sex = sex,
        Age_at_Diagnosis = age_gp,
        loc22cd,
        loc22nm,
        Count,
        Type_of_rate = type,
        Rate,
        LCI = LCL,
        UCI = UCL,
        Flag = flag
      )
    
    ## bind to make tables ----
    table1 <-
      rbind(
        out_ICD3_digit %>% select(-Classification_flag),
        out_ICD3_digit_gor,
        age_spec_ICD3_digit,
        out_ICD3_digit_dep
      )
    table2 <- rbind(asr_ICD4_digit, age_spec_ICD4_digit)
    table3 <- out_ICD3_digit_stage
    table4 <- rbind(out_ICD3_digit_loc, age_spec_ICD3_loc)
    
    ## filter 3 digit to make trends latest year ----
    out <- out_ICD3_digit %>%
      filter(Type_of_rate == 'Age-standardised') %>%
      mutate(Year = year) %>%
      select(
        Year,
        ICD10_code,
        Site_description,
        Sex,
        Count,
        Rate,
        LCI,
        UCI,
        Flag,
        Classification_flag
      )
    assign(paste0('table5_', year), out)
    
  } else{
    ## filter 3 digit to make trends for all years except latest ----
    out <- out_ICD3_digit %>%
      filter(Type_of_rate == 'Age-standardised') %>%
      mutate(Year = year) %>%
      select(
        Year,
        ICD10_code,
        Site_description,
        Sex,
        Count,
        Rate,
        LCI,
        UCI,
        Flag,
        Classification_flag
      )
    assign(paste0('table5_', year), out)
  }
}

## bind trends to make table 5 ----
table5 <- rbindlist(mget(paste0('table5_', years)))

#read in DQ table to add most recent year DQ to template (no need to overwrite previous years values)
table6 <- fread(file = "")


# format tables ----
table1 <- table1 %>%
  mutate(
    Rate = ifelse(is.na(Rate), '[u1]', sprintf('%.1f', Rate)),
    LCI = ifelse(is.na(LCI), '[u1]', sprintf('%.1f', LCI)),
    UCI = ifelse(is.na(UCI), '[u1]', sprintf('%.1f', UCI)),
    Sex = ifelse(Sex == '1', 'Males', 'Females')
  ) %>%
  mutate(
    ICD10_code = ifelse(
      str_length(ICD10_code) == 4,
      gsub('^(.{3})(.*)$', '\\1\\.\\2', ICD10_code),
      ICD10_code
    ),
    Age_at_Diagnosis = case_when(
      Age_at_Diagnosis == '00-00' ~ 'Under 1',
      Age_at_Diagnosis == '01-04' ~ '1 to 4',
      Age_at_Diagnosis == '05-09' ~ '5 to 9',
      Age_at_Diagnosis == '90plus' ~ '90 and over',
      T ~ str_replace(Age_at_Diagnosis, '-', ' to ')
    )
  ) %>%
  mutate(Flag = ifelse(Rate == '[u1]', '[note2]', Flag)) %>%
  mutate(Flag = ifelse (Type_of_rate == 'Non-standardised' &
                          Flag == '[note1]', NA, Flag)) %>%
  mutate(ICD10_code = str_replace_all(ICD10_code, "-", " to "))

table2 <- table2 %>%
  filter(ICD10_code != 'C80') %>%
  mutate(
    Rate = ifelse(is.na(Rate), '[u1]', sprintf('%.1f', Rate)),
    LCI = ifelse(is.na(LCI), '[u1]', sprintf('%.1f', LCI)),
    UCI = ifelse(is.na(UCI), '[u1]', sprintf('%.1f', UCI)),
    Sex = ifelse(Sex == '1', 'Males', 'Females')
  ) %>%
  mutate(
    ICD10_code = ifelse(
      str_length(ICD10_code) == 4,
      gsub('^(.{3})(.*)$', '\\1\\.\\2', ICD10_code),
      ICD10_code
    ),
    Age_at_Diagnosis = case_when(
      Age_at_Diagnosis == '00-00' ~ 'Under 1',
      Age_at_Diagnosis == '01-04' ~ '1 to 4',
      Age_at_Diagnosis == '05-09' ~ '5 to 9',
      Age_at_Diagnosis == '90plus' ~ '90 and over',
      T ~ str_replace(Age_at_Diagnosis, '-', ' to ')
    )
  ) %>%
  mutate(Flag = ifelse(Rate == '[u1]', '[note2]', Flag)) %>%
  mutate(Flag = ifelse (Type_of_rate == 'Non-standardised' &
                          Flag == '[note1]', NA, Flag)) %>%
  mutate(ICD10_code = str_replace_all(ICD10_code, "-", " to "))

table3 <- table3 %>%
  mutate(
    Rate = ifelse(is.na(Rate), '[u1]', sprintf('%.1f', Rate)),
    LCI = ifelse(is.na(LCI), '[u1]', sprintf('%.1f', LCI)),
    UCI = ifelse(is.na(UCI), '[u1]', sprintf('%.1f', UCI)),
    Sex = ifelse(Sex == '1', 'Males', 'Females')
  ) %>%
  mutate(Flag = ifelse(Rate == '[u1]', '[note2]', Flag)) %>%
  mutate(Flag = ifelse (Type_of_rate == 'Non-standardised' &
                          Flag == '[note1]', NA, Flag)) %>%
  filter(!(
    Site_Group %in% c(
      'Cervix',
      'Stomach excluding cardia and gastroesophageal junction',
      'Thyroid'
    )
  )) %>%
  mutate(ICD10_code = str_replace_all(ICD10_code, "-", " to "))


table4 <- table4 %>%
  mutate(
    Rate = ifelse(is.na(Rate), '[u1]', sprintf('%.1f', Rate)),
    LCI = ifelse(is.na(LCI), '[u1]', sprintf('%.1f', LCI)),
    UCI = ifelse(is.na(UCI), '[u1]', sprintf('%.1f', UCI)),
    Sex = ifelse(Sex == '1', 'Males', 'Females')
  ) %>%
  mutate(Flag = ifelse(Rate == '[u1]', '[note2]', Flag)) %>%
  mutate(Flag = ifelse (Type_of_rate == 'Non-standardised' &
                          Flag == '[note1]', NA, Flag)) %>%
  mutate(ICD10_code = str_replace_all(ICD10_code, "-", " to ")) %>%
  mutate(Age_at_Diagnosis = str_replace_all(Age_at_Diagnosis, "-", " to ")) %>%
  mutate(Age_at_Diagnosis = str_replace_all(Age_at_Diagnosis, "80plus", "80 and over")) %>%
  mutate(Age_at_Diagnosis = str_replace_all(Age_at_Diagnosis, "00 to 24", "24 and under"))


table5 <- table5 %>%
  mutate(
    Rate = ifelse(is.na(Rate), '[u1]', sprintf('%.1f', Rate)),
    LCI = ifelse(is.na(LCI), '[u1]', sprintf('%.1f', LCI)),
    UCI = ifelse(is.na(UCI), '[u1]', sprintf('%.1f', UCI)),
    Sex = ifelse(Sex == '1', 'Males', 'Females')
  ) %>%
  mutate(ICD10_code = ifelse(
    str_length(ICD10_code) == 4,
    gsub('^(.{3})(.*)$', '\\1\\.\\2', ICD10_code),
    ICD10_code
  )) %>%
  mutate(Flag = ifelse(Rate == '[u1]', '[note2]', Flag)) %>%
  # as table5 is age standardised only no need to write over non-standardised values like in tables 1-4
  mutate(ICD10_code = str_replace_all(ICD10_code, "-", " to "))

table5_95_00 <- read.dta13(file = "")

table5 <- bind_rows(table5_95_00, table5)
table5 <- table5 %>%
  arrange(Year, ICD10_code, Sex)

table6 <-  table6 %>%
  mutate(
    Total = (format(
      Total, big.mark = ",", scientific = FALSE
    )),
    Status_1 = (format(
      Status_1, big.mark = ",", scientific = FALSE
    )),
    Status_2 = (format(
      Status_2, big.mark = ",", scientific = FALSE
    )),
    Status_3 = (format(
      Status_3, big.mark = ",", scientific = FALSE
    )),
    #for 2020 percentage Percentage_of_Status_3= 0 add a decimel point for formatting in ODS
    Percentage_of_Status_3 = if (Percentage_of_Status_3 == 0)
      '0.0'
    else
      as.numeric(Percentage_of_Status_3)
  )

# save data to compare ----
save.dta13(table1, file = "")
save.dta13(table2, file = "")
save.dta13(table3, file = "")
save.dta13(table4, file = "")
save.dta13(table5, file = "")
save.dta13(table6, file = "")



# load excel workbook ----
wb <- loadWorkbook("")
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

writeData(
  wb,
  x = table5,
  sheet = "Table_5",
  startRow = 4,
  colNames = F
)

writeData(
  wb,
  x = table6,
  sheet = "Table_6",
  startRow = 53,
  colNames = F
)

## save workbook ----
saveWorkbook(wb, file = "", overwrite = TRUE)
