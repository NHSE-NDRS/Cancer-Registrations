# set-up
library(RJDBC)
library(svDialogs)
library(getPass)
library(dplyr)
library(tidyr)

# Set up CAS ----
# This section is where you make your changes.

# sets directory to where the script is located.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# make sure the oracle_connection.R is in the same folder as your script or put I full path into the source function
source("oracle_connection.R")


# CAS set up. Use your own user name as you would in sql developer.
casUserName = ''
schema <- ""
CAS <- createConnection(sid = schema, username = casUserName)


# Set years of interest ----
first_year <- 2001
last_year <- 2020


# Query ----
# This section should not need changing.
mortQuery <- "SELECT
    icd10                icd10,
    substr(
        icd10, 1, 3
    )                    site,
    regyear              year,
    sex                  sex,
    CASE
    WHEN age = 0               THEN
    '00-00'
    WHEN age BETWEEN 1 AND 4   THEN
    '01-04'
    WHEN age BETWEEN 5 AND 9   THEN
    '05-09'
    WHEN age BETWEEN 10 AND 14 THEN
    '10-14'
    WHEN age BETWEEN 15 AND 19 THEN
    '15-19'
    WHEN age BETWEEN 20 AND 24 THEN
    '20-24'
    WHEN age BETWEEN 25 AND 29 THEN
    '25-29'
    WHEN age BETWEEN 30 AND 34 THEN
    '30-34'
    WHEN age BETWEEN 35 AND 39 THEN
    '35-39'
    WHEN age BETWEEN 40 AND 44 THEN
    '40-44'
    WHEN age BETWEEN 45 AND 49 THEN
    '45-49'
    WHEN age BETWEEN 50 AND 54 THEN
    '50-54'
    WHEN age BETWEEN 55 AND 59 THEN
    '55-59'
    WHEN age BETWEEN 60 AND 64 THEN
    '60-64'
    WHEN age BETWEEN 65 AND 69 THEN
    '65-69'
    WHEN age BETWEEN 70 AND 74 THEN
    '70-74'
    WHEN age BETWEEN 75 AND 79 THEN
    '75-79'
    WHEN age BETWEEN 80 AND 84 THEN
    '80-84'
    WHEN age BETWEEN 85 AND 89 THEN
    '85-89'
    WHEN age >= 90             THEN
    '90plus'
    END                  fiveyearageband,
    CASE
    WHEN age <= 24             THEN
    '24 and under'
    WHEN age BETWEEN 25 AND 49 THEN
    '25 to 49'
    WHEN age BETWEEN 50 AND 59 THEN
    '50 to 59'
    WHEN age BETWEEN 60 AND 69 THEN
    '60 to 69'
    WHEN age BETWEEN 70 AND 79 THEN
    '70 to 79'
    WHEN age >= 80             THEN
    '80 and over'
    END                  altageband,
    imd19_quintile_lsoas imd_quintile,
    loc22cd              loc22_code,
    loc22nm              loc22_name,
    'E92000001'          country_code,
    'England'            country_name,
    COUNT(*)             cases
FROM
    ons
    LEFT JOIN icb ON icb.lsoa11cd = ons.lsoa11
    LEFT JOIN dep ON dep.lsoa11_code = ons.lsoa11
                                             AND regyear = 2020
WHERE
    regyear = ?
    AND substr(
        icd10, 1, 3
    ) NOT IN ( 'D04', 'D34', 'D36' )
    AND substr(
        icd10, 1, 3
    ) NOT BETWEEN 'D10' AND 'D31'
    AND icd10 NOT IN ( 'D350', 'D351', 'D355' )
GROUP BY
    icd10,
    substr(
        icd10, 1, 3
    ),
    regyear,
    sex,
    CASE
        WHEN age = 0               THEN
        '00-00'
        WHEN age BETWEEN 1 AND 4   THEN
        '01-04'
        WHEN age BETWEEN 5 AND 9   THEN
        '05-09'
        WHEN age BETWEEN 10 AND 14 THEN
        '10-14'
        WHEN age BETWEEN 15 AND 19 THEN
        '15-19'
        WHEN age BETWEEN 20 AND 24 THEN
        '20-24'
        WHEN age BETWEEN 25 AND 29 THEN
        '25-29'
        WHEN age BETWEEN 30 AND 34 THEN
        '30-34'
        WHEN age BETWEEN 35 AND 39 THEN
        '35-39'
        WHEN age BETWEEN 40 AND 44 THEN
        '40-44'
        WHEN age BETWEEN 45 AND 49 THEN
        '45-49'
        WHEN age BETWEEN 50 AND 54 THEN
        '50-54'
        WHEN age BETWEEN 55 AND 59 THEN
        '55-59'
        WHEN age BETWEEN 60 AND 64 THEN
        '60-64'
        WHEN age BETWEEN 65 AND 69 THEN
        '65-69'
        WHEN age BETWEEN 70 AND 74 THEN
        '70-74'
        WHEN age BETWEEN 75 AND 79 THEN
        '75-79'
        WHEN age BETWEEN 80 AND 84 THEN
        '80-84'
        WHEN age BETWEEN 85 AND 89 THEN
        '85-89'
        WHEN age >= 90             THEN
        '90plus'
    END,
    CASE
        WHEN age <= 24             THEN
        '24 and under'
        WHEN age BETWEEN 25 AND 49 THEN
        '25 to 49'
        WHEN age BETWEEN 50 AND 59 THEN
        '50 to 59'
        WHEN age BETWEEN 60 AND 69 THEN
        '60 to 69'
        WHEN age BETWEEN 70 AND 79 THEN
        '70 to 79'
        WHEN age >= 80             THEN
        '80 and over'
    END,
    imd19_quintile_lsoas,
    loc22cd,
    loc22nm,
    'E92000001',
    'England'"


# loop through years and extract data ----
years <- first_year:last_year
for (year in years) {
  # Extract data ----
  mort <-
    dbGetQueryOracle(CAS, mortQuery, year, timeit = F, rowlimit = NA)
  
  saveRDS(
    mort,
    file = paste0(
      ""
    )
  )
}

