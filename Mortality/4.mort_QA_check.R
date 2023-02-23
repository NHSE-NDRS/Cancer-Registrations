library(dplyr)

#Script to check mortality extracts are the same



mortality_stata <-
  read.csv(
    ""
  )

mortality_r_2020 <-
  readRDS(
    ""
  )
mortality_stata_2001 = mortality_stata %>% filter(YEAR == 2020)
d1f_2020 = mortality_r_2020 %>% anti_join(mortality_stata_2020, by = NULL)
nrow(d1f_2020)

years <- 2001:2019
for (year in years) {
  mortality_r <-
    readRDS(
      file = paste0(
        ""
      )
    )
  mortality_stata_year = mortality_stata %>%
    filter(YEAR == year) %>%
    group_by(
      ICD10,
      YEAR,
      SEX,
      FIVEYEARAGEBAND,
      LOC22CD,
      LOC22NM,
      COUNTRY_CODE,
      COUNTRY_NAME,
      SITE
    ) %>%
    summarise(CASES = sum(CASES), .groups = 'drop')
  d1f = mortality_r %>% anti_join(mortality_stata_year, by = NULL)
  print(year)
  print(nrow(d1f))
}
