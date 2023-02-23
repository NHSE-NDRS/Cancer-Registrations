# libraries
library(dplyr)
library(tidyselect)
library(data.table)
library(tidyr)
library(readstata13)

# sets directory to where the script is located.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set years of interest ----
years <- 2001:2020

# load standard pop, change 0 age group and sex labels from Male, Female to 1,2
ESP_pop <- fread("")
ESP_pop$agegroup[ESP_pop$agegroup == "0"] <- "00-00"
ESP_pop$sex <-
  factor(ESP_pop$sex ,
         levels = c('Male', 'Female'),
         labels = c('1', '2'))
names(ESP_pop) <- c('SEX', 'STANDARD', 'FIVEYEARAGEBAND')


icd4digit_sites <-
  read.dta13("")

# for each year in range create the tables
for (year in years) {
  if (year >= 2020) {
    # read in mortality data
    mort_raw <-
      readRDS(file = paste0(""))
    
    mort_raw <-
      mort_raw %>% merge(icd4digit_sites,
                         by.x = 'ICD10',
                         by.y = 'icd10',
                         all.y = T)
    
    # cast sex as string
    mort_raw$SEX <-  as.character(mort_raw$SEX)
    
    # completing the rows
    mort <- mort_raw %>%
      complete(
        nesting(LOC22_CODE, LOC22_NAME, COUNTRY_CODE, COUNTRY_NAME),
        nesting(FIVEYEARAGEBAND, ALTAGEBAND),
        IMD_QUINTILE,
        ICD10,
        YEAR,
        SEX,
        fill = list(CASES = 0)
      ) %>%
      drop_na(
        LOC22_CODE,
        LOC22_NAME,
        IMD_QUINTILE,
        COUNTRY_CODE,
        COUNTRY_NAME,
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND
      ) %>%
      mutate(SITE = ifelse(
        ICD10 %in% c("D352", "D353", "D354"),
        ICD10,
        substr(ICD10, 1, 3)
      ))
    
    mort <- mort %>%
      filter(!(SITE %in% sprintf("C%02d", 51:58) & SEX == "1")) %>%
      filter(!(SITE %in% sprintf("C%02d", 60:63)  & SEX == "2")) %>%
      filter(!(SITE %in% c("D06", "D39") & SEX == "1")) %>%
      filter(!(SITE == "D40" & SEX == "2")) %>%
      filter(!(ICD10 %in% sprintf("D0%02d", 70:73) &
                 SEX == "1")) %>%
      filter(!(ICD10 %in% sprintf("D0%02d", 74:76) & SEX == "2"))
    
    # Group, count cases, remove NAs, then ungroup
    mort <- mort %>%
      group_by(
        YEAR,
        SEX,
        SITE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE
      ) %>%
      summarise(CASES = sum(CASES, na.rm = T), .groups = 'drop')
    
    # set up groups
    # malignant neoplasms
    mort_C0097 <- mort %>%
      filter(substr(SITE, 1, 1) == 'C') %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C00-C97',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasms excluding NMSC
    mort_C0097_C44 <- mort %>%
      filter(substr(SITE, 1, 1) == 'C' & SITE != 'C44') %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C00-C97 excl. C44',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # all neoplasms
    mort_c00d48 <- mort %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C00-C97, D00-D48',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasms of lip, oral cavity and pharynx
    mort_C0014 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 00:14)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C00-C14',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasms of the colon, rectosignoid junction and rectum
    mort_C1820 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 18:20)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C18-C20',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasms of the trachea, bronchus and lung
    mort_C3334 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 33:34)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C33-C34',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasms of corpus uteri and uterus unspecified
    mort_C5455 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 54:55)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C54-C55',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasm of the ovary and other/unspecified female genital organs
    mort_C5657 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 56:57)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C56-C57',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # non-hodgkin lymphoma
    mort_C8286 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 82:86)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C82-C86',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # leukaemias
    mort_C9195 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 91:95)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        IMD_QUINTILE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C91-C95',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # combine all groups
    mort <- rbind(
      mort,
      mort_c00d48,
      mort_C0097,
      mort_C0097_C44,
      mort_C0014,
      mort_C1820,
      mort_C3334,
      mort_C5455,
      mort_C5657,
      mort_C8286,
      mort_C9195
    )
    
    rm(
      mort_c00d48,
      mort_C0097,
      mort_C0097_C44,
      mort_C0014,
      mort_C1820,
      mort_C3334,
      mort_C5455,
      mort_C5657,
      mort_C8286,
      mort_C9195
    )
    # load years population
    pop <-
      readRDS(paste0(""))
    
    
    pop <- pop %>%
      group_by(year, sex, age, loc22cd, imd_quintile) %>%
      summarise(POPULATION = sum(population), .groups = 'drop') %>%
      mutate(age = ifelse(age == '90+', '90plus', age)) %>%
      rename(
        SEX = sex,
        YEAR = year,
        FIVEYEARAGEBAND = age,
        LOC22_CODE = loc22cd,
        IMD_QUINTILE = imd_quintile
      )
    
    # merge with the cases
    mort_std <-
      base::merge(mort, ESP_pop, by = c('SEX', 'FIVEYEARAGEBAND'))
    mort_std_pop <-
      base::merge(
        mort_std,
        pop,
        by = c(
          'YEAR',
          'SEX',
          'FIVEYEARAGEBAND',
          'LOC22_CODE',
          'IMD_QUINTILE'
        )
      )
    
    # england tables
    eng <- mort_std_pop %>%
      group_by(YEAR,
               SEX,
               FIVEYEARAGEBAND,
               COUNTRY_CODE,
               COUNTRY_NAME,
               SITE,
               STANDARD) %>%
      summarise(
        CASES = sum(CASES),
        POPULATION = sum(POPULATION),
        .groups = 'drop'
      )
    
    eng_imd <- mort_std_pop %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        COUNTRY_CODE,
        COUNTRY_NAME,
        SITE,
        STANDARD,
        IMD_QUINTILE
      ) %>%
      summarise(
        CASES = sum(CASES),
        POPULATION = sum(POPULATION),
        .groups = 'drop'
      )
    
    # loc table
    # note the cascading grouping in order to sum the totals for the new age band
    loc <- mort_std_pop %>%
      group_by(YEAR,
               SEX,
               FIVEYEARAGEBAND,
               ALTAGEBAND,
               LOC22_CODE,
               LOC22_NAME,
               SITE,
               STANDARD) %>%
      summarise(
        CASES = sum(CASES),
        POPULATION = sum(POPULATION),
        .groups = 'drop'
      ) %>%
      group_by(YEAR, SEX, ALTAGEBAND, LOC22_CODE, LOC22_NAME, SITE) %>%
      summarise(
        CASES = sum(CASES),
        POPULATION = sum(POPULATION),
        STANDARD = sum(STANDARD),
        .groups = 'drop'
      )
    
    # save as RDS
    saveRDS(eng,
            file = paste0(""))
    saveRDS(eng_imd,
            file = paste0(""))
    saveRDS(loc,
            file = paste0(""))
    
    # export to shiny app
    saveRDS(eng,
            file = paste0(""))
    saveRDS(eng_imd,
            file = paste0(""))
    saveRDS(loc,
            file = paste0(""))
    
  } else {
    # read in mortality data
    mort_raw <-
      readRDS(file = paste0(""))
    
    mort_raw <-
      mort_raw %>% merge(icd4digit_sites,
                         by.x = 'ICD10',
                         by.y = 'icd10',
                         all.y = T)
    
    # cast sex as string
    mort_raw$SEX <-  as.character(mort_raw$SEX)
    
    # completing the rows
    mort <- mort_raw %>%
      complete(
        nesting(LOC22_CODE, LOC22_NAME, COUNTRY_CODE, COUNTRY_NAME),
        nesting(FIVEYEARAGEBAND, ALTAGEBAND),
        ICD10,
        YEAR,
        SEX,
        fill = list(CASES = 0)
      ) %>%
      drop_na(
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME,
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND
      ) %>%
      mutate(SITE = ifelse(
        ICD10 %in% c("D352", "D353", "D354"),
        ICD10,
        substr(ICD10, 1, 3)
      ))
    
    mort <- mort %>%
      filter(!(SITE %in% sprintf("C%02d", 51:58) & SEX == "1")) %>%
      filter(!(SITE %in% sprintf("C%02d", 60:63)  & SEX == "2")) %>%
      filter(!(SITE %in% c("D06", "D39") & SEX == "1")) %>%
      filter(!(SITE == "D40" & SEX == "2")) %>%
      filter(!(ICD10 %in% sprintf("D0%02d", 70:73) &
                 SEX == "1")) %>%
      filter(!(ICD10 %in% sprintf("D0%02d", 74:76) & SEX == "2"))
    
    # Group, count cases, remove NAs, then ungroup
    mort <- mort %>%
      group_by(
        YEAR,
        SEX,
        SITE,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME,
        FIVEYEARAGEBAND,
        ALTAGEBAND
      ) %>%
      summarise(CASES = sum(CASES, na.rm = T), .groups = 'drop')
    
    # set up groups
    # malignant neoplasms
    mort_C0097 <- mort %>%
      filter(substr(SITE, 1, 1) == 'C') %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C00-C97',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasms excluding NMSC
    mort_C0097_C44 <- mort %>%
      filter(substr(SITE, 1, 1) == 'C' & SITE != 'C44') %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C00-C97 excl. C44',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # all neoplasms
    mort_c00d48 <- mort %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C00-C97, D00-D48',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasms of lip, oral cavity and pharynx
    mort_C0014 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 00:14)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C00-C14',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasms of the colon, rectosignoid junction and rectum
    mort_C1820 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 18:20)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C18-C20',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasms of the trachea, bronchus and lung
    mort_C3334 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 33:34)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C33-C34',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasms of corpus uteri and uterus unspecified
    mort_C5455 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 54:55)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C54-C55',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # malignant neoplasm of the ovary and other/unspecified female genital organs
    mort_C5657 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 56:57)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C56-C57',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # non-hodgkin lymphoma
    mort_C8286 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 82:86)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C82-C86',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # leukaemias
    mort_C9195 <- mort %>%
      filter(SITE %in% sprintf('C%02d', 91:95)) %>%
      group_by(
        YEAR,
        SEX,
        FIVEYEARAGEBAND,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        COUNTRY_CODE,
        COUNTRY_NAME
      ) %>%
      summarise(SITE = 'C91-C95',
                CASES = sum(CASES),
                .groups = 'drop')
    
    # combine all groups
    mort <- rbind(
      mort,
      mort_c00d48,
      mort_C0097,
      mort_C0097_C44,
      mort_C0014,
      mort_C1820,
      mort_C3334,
      mort_C5455,
      mort_C5657,
      mort_C8286,
      mort_C9195
    )
    
    rm(
      mort_c00d48,
      mort_C0097,
      mort_C0097_C44,
      mort_C0014,
      mort_C1820,
      mort_C3334,
      mort_C5455,
      mort_C5657,
      mort_C8286,
      mort_C9195
    )
    
    # load years population
    pop <-
      readRDS(paste0(""))
    
    
    pop <- pop %>%
      group_by(year, sex, age, loc22cd) %>%
      summarise(POPULATION = sum(population), .groups = 'drop') %>%
      mutate(age = ifelse(age == '90+', '90plus', age)) %>%
      rename(
        SEX = sex,
        FIVEYEARAGEBAND = age,
        LOC22_CODE = loc22cd,
        YEAR = year
      )
    
    # merge with the cases
    mort_std <-
      base::merge(mort, ESP_pop, by = c('SEX', 'FIVEYEARAGEBAND'))
    mort_std_pop <-
      base::merge(mort_std,
                  pop,
                  by = c('SEX', 'FIVEYEARAGEBAND', 'LOC22_CODE', 'YEAR'))
    
    # england tables
    eng <- mort_std_pop %>%
      group_by(YEAR,
               SEX,
               FIVEYEARAGEBAND,
               COUNTRY_CODE,
               COUNTRY_NAME,
               SITE,
               STANDARD) %>%
      summarise(
        CASES = sum(CASES),
        POPULATION = sum(POPULATION),
        .groups = 'drop'
      )
    
    # loc table
    # note the cascading grouping in order to sum the totals for the new age band
    loc <- mort_std_pop %>%
      group_by(
        YEAR,
        SEX,
        ALTAGEBAND,
        LOC22_CODE,
        LOC22_NAME,
        SITE) %>%
        summarise(
          CASES = sum(CASES),
          POPULATION = sum(POPULATION),
          STANDARD = sum(STANDARD),
          .groups = 'drop'
        )
        
        # save as RDS
        saveRDS(eng,
                file = paste0(""))
        saveRDS(loc,
                file = paste0(""))
        
        # export to shiny app
        saveRDS(eng,
                file = paste0(""))
        saveRDS(loc,
                file = paste0(""))
  }
}
