# Cancer-Registrations
This is the RAP code used to publish the Cancer Registration Statistics, England 2020 (https://digital.nhs.uk/data-and-information/publications/statistical/cancer-registration-statistics/england-2020)

There are three main sections (alphabetically):
1) Incidence
2) Mortality
3) Population_data

All three sections use the supporting Oracle drivers in R.

The population data code extracts the populations covering the period 1995 to 2020; it is split into two sets of code because of the different locations for populations data between 1995 to 2000 and 2001 to 2020. These population data are used by the code in the incidence and mortality sections to estimate rates.

The incidence and mortality code extracts and formats the data before producing the estimates presented in the publication. Both incidence and mortality use the same rate functions which have been placed into both sections for ease of reference.

The incidence section extraction covers the period 1995 to 2020 so requires two sets code so each period (1995 to 2000 and 2001 to 2020) can be merged with the relevant formatted population extracts. The code for 2001 to 2020 includes the sections that estimates incidence by stage at diagnosis and by deprivation quintile. There are analysis files to produce a report on the data quality used in the incidence estimates and to estimate the stage completeness.

The same functions are used for the mortality section; this requires less code than the incidence section for 2 reasons:
1) A shorter time series is required for the mortality estimates, so a single data extract can be created to merge with the population data extract covering 2001 to 2020. 
2) The mortality data set cannot be linked to other patient-level data sets which limits the scope of the estimates that can be produced.
