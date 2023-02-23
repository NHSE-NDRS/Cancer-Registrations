library(dplyr)
library(tidyr)
library(data.table)

# Rate funtions ----
## Age specific ----
AgeSpecificRates <- function(data,groups,pop_name = NA,cases_name = NA,sex_name = 'sex',age_gp_name = 'age_gp'){
  
  if(!is.na(pop_name)){
    data <- data %>% rename(POPULATION = all_of(pop_name))
  }
  if(!is.na(cases_name)){
    data <- data %>% rename(CASES = all_of(cases_name))
  }
  
  data %>%
    group_by_at(c(groups,sex_name,age_gp_name)) %>%
    summarise(sum_cases = sum(CASES),
              sum_pop = max(POPULATION),
              .groups = 'drop') %>%
    mutate(Crude.Rate = sum_cases/sum_pop * 100000,
           LCL = case_when(sum_cases > 2 & sum_cases <= 9 ~ qchisq(0.025,2*sum_cases)/(2*sum_pop)*100000,
                           sum_cases > 9 ~ sum_cases*(1-1/(9*sum_cases)-qnorm(0.975)/(3*sqrt(sum_cases)))^3/sum_pop*100000),
           UCL = case_when(sum_cases > 2 & sum_cases <= 9 ~ qchisq(0.975,2*sum_cases+2)/(2*sum_pop)*100000,
                           sum_cases > 9 ~ (sum_cases+1)*(1-1/(9*(sum_cases+1))+qnorm(0.975)/(3*sqrt((sum_cases+1))))^3/sum_pop*100000)) %>%
    mutate(Rate = ifelse(sum_cases > 2,round(Crude.Rate,1),NA),
           LCL = round(LCL,1),
           UCL = round(UCL,1),
           flag = NA,
           type = 'Age-specific') %>%
    select(all_of(groups),
           all_of(sex_name), 
           all_of(age_gp_name),
           Count = sum_cases,
           type,
           Rate ,
           LCL,
           UCL)
  
}

## Age-standarised and non-standardised rates ----
ASR <- function(data,groups,pop_name = NA,cases_name = NA,std_name = NA,sex_name = 'sex',age_gp_name = 'age_gp'){
  # set the variable names so the code runs

  if(!is.na(pop_name)){
    data <- data %>% rename(POPULATION = all_of(pop_name))
  }
  if(!is.na(cases_name)){
    data <- data %>% rename(CASES = all_of(cases_name))
  }
  if(!is.na(std_name)){
    data <- data %>% rename(STANDARD = all_of(std_name))
  }
  
  data %>%
    group_by_at(c(groups,sex_name,age_gp_name)) %>%
    summarise(CASES = sum(CASES),
              POPULATION = max(POPULATION),
              STANDARD = max(STANDARD),.groups = 'drop') %>%
    group_by_at(c(groups,sex_name)) %>%
    summarise(sum_std2cases_overpop2 = sum((STANDARD^2 * CASES)/(POPULATION^2)),
              R = sum(STANDARD*CASES/POPULATION)/sum(STANDARD), #point estimate
              sum_cases = sum(CASES),
              sum_std2 = sum(STANDARD^2),
              sum_std2overpop2 = sum(STANDARD^2 / (POPULATION^2)),
              sum_std = sum(STANDARD),
              sum_stdoverpop = sum(STANDARD/POPULATION),
              sum_pop = sum(POPULATION),
              .groups = 'keep') %>%
    mutate(sum_std_all_squared = sum_std^2) %>%
    mutate(multiplier_with_square_root = sqrt(sum_std2cases_overpop2 / (sum_cases * sum_std_all_squared)),
           lcl_degrees_of_freedom = 2 * sum_cases,
           ucl_degrees_of_freedom = 2 * sum_cases + 2)%>%
    mutate(lcl_inverse_chi_squared = qchisq(0.05/2, lcl_degrees_of_freedom),
           ucl_inverse_chi_squared = qchisq(1 - (0.05/2), ucl_degrees_of_freedom)) %>%
    mutate(lcl_multiplicand = lcl_inverse_chi_squared / 2 - sum_cases,
           ucl_multiplicand = ucl_inverse_chi_squared / 2 - sum_cases) %>%
    mutate(lcl = R + (multiplier_with_square_root * lcl_multiplicand),
           ucl = R + (multiplier_with_square_root * ucl_multiplicand),
           Crude.Rate = sum_cases/sum_pop * 100000,
           LCL = case_when(sum_cases > 2 & sum_cases <= 9 ~ qchisq(0.025,2*sum_cases)/(2*sum_pop)*100000,
                           sum_cases > 9 ~ sum_cases*(1-1/(9*sum_cases)-qnorm(0.975)/(3*sqrt(sum_cases)))^3/sum_pop*100000),
           UCL = case_when(sum_cases > 2 & sum_cases <= 9 ~ qchisq(0.975,2*sum_cases+2)/(2*sum_pop)*100000,
                           sum_cases > 9 ~ (sum_cases+1)*(1-1/(9*(sum_cases+1))+qnorm(0.975)/(3*sqrt((sum_cases+1))))^3/sum_pop*100000),
           ASR = R * 100000,
           ASR_LCL = lcl * 100000,
           ASR_UCL = ucl * 100000) %>% 
    mutate(Crude_Rate = ifelse(sum_cases >2,round(Crude.Rate,1),NA),
           Crude_LCL = round(LCL,1),
           Crude_UCL = round(UCL,1), 
           ASR_Rate = ifelse(sum_cases > 9,round(ASR,1),NA), 
           ASR_LCL = ifelse(sum_cases > 9,round(ASR_LCL,1),NA), 
           ASR_UCL = ifelse(sum_cases > 9,round(ASR_UCL,1),NA),
           flag = ifelse(sum_cases < 20 & sum_cases > 9 ,'[note1]',NA)) %>%
    select(all_of(groups),
           all_of(sex_name),
           Count = sum_cases,
           Crude_Rate ,
           Crude_LCL,
           Crude_UCL, 
           ASR_Rate, 
           ASR_LCL , 
           ASR_UCL,
           flag) %>%
    ungroup() 
  
}