# setup ----
library(dplyr)
library(tidyr)
library(data.table)
library(openxlsx)
library(stringr)
library(readstata13)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# lookup files ----
site_code_name <-
  read.csv(
    ""
  )

site_code_name <-
  rbind(
    site_code_name,
    data.frame(icd10 = 'C82-C85', site_description = 'Non-Hodgkin lymphoma')
  )

region_code_name <- fread("")
# this loads the rate functions to be used.
source("rate_functions.R")

## load data ----
inc_95_00 <-
  readRDS(
    paste0(
      ""
    )
  )

#reordered to table 5 for 2020 regstrations publication
## 3 digit ASRs and crude ----
table5_95_00 <- inc_95_00 %>%
  ASR(
    groups = c('year', 'icd_3digit'),
    pop_name = 'population',
    cases_name = 'tumourcount',
    std_name = 'standard'
  ) %>%
  pivot_longer(
    cols = -c(year, icd_3digit, sex, Count, flag),
    names_to = c('type', '.value'),
    names_pattern = "(.*)_(.*)"
  ) %>%
  merge(site_code_name, by.x = 'icd_3digit', by.y = 'icd10') %>%
  filter(type == 'ASR') %>%
  select(
    Year = year,
    ICD10_code = icd_3digit ,
    Site_description = site_description,
    Sex = sex,
    Count,
    Rate,
    LCI = LCL,
    UCI = UCL,
    Flag = flag
  )




table5_95_00 <- table5_95_00 %>%
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
  ))  %>%
  mutate(Flag = ifelse(Rate == '[u1]', '[note2]', Flag)) %>%
  mutate(Flag = str_replace_all(Flag, "u", "[note1]")) %>%
  mutate(ICD10_code = str_replace_all(ICD10_code, "-", " to "))
#save data to use in next file
#added replace to change - to to for accessibility in icd names

save.dta13(table5_95_00, file = "")

table5_95_00_oldstyle <- table5_95_00 %>%
  mutate(Rate = ifelse(Rate == '[u1]', 'u', Rate))  %>%
  mutate(LCI = ifelse(LCI == '[u1]', 'u', LCI)) %>%
  mutate(UCI = ifelse(UCI == '[u1]', 'u', UCI)) %>%
  mutate(Flag = ifelse(Flag == '[note1', 'u', Flag)) %>%
  select (Year ,
          ICD10_code ,
          Site_description ,
          Sex ,
          Count,
          Rate,
          Flag)


# save data to compare ----
save.dta13(table5_95_00_oldstyle, file = "")

# load excel workbook for QA ----
#updated file name for 2022 to final excel saved for last run
wb <-
  loadWorkbook("")

previous <- read.xlsx(wb,
                      sheet = 5,
                      startRow = 3,
                      colNames = T) %>%
  filter(!is.na(Year)) %>% filter(between(Year, 1995 , 2000))


compare <-
  merge(
    table5_95_00_oldstyle,
    previous,
    by.x = c("Year", "ICD10_code", "Sex"),
    by.y = c("Year", "ICD10.code", "Sex"),
    all = T
  )

compare %>% filter(is.na(Rate) |
                     is.na(Rate)) %>% View() 

compare %>% filter(Rate != `Rate.(per.100,000.population)`) %>% View() 

compare %>%
  mutate(diff = abs(
    as.numeric(Rate) - as.numeric(`Rate.(per.100,000.population)`)
  )) %>%
  filter(diff < 5 &
           diff > 0) %>% View() 

#Now move to R_inc_ASr_calculation.R code
