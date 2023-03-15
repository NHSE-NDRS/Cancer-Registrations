# Setup ----
library(data.table)
library(dplyr)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# make sure the oracle_connection.R is in the same folder as your script or put I full path into the source function
source("oracle_connection.R")

# connect to CAS ----
CASREF <- createConnection(sid = "", username = "",port =)


# Set years of interest ----
first_year <- 1995
last_year <- 2020

popQuery <- "with pop as(
select year
       ,sex
       ,'00-00' as age
       ,sum(a0) as population
       ,lsoa11
       ,'England' as geography
from 
where year = ? AND substr(lsoa11, 1,1) = 'E'
group by year
         ,sex
         ,'00-00'
         ,lsoa11
         ,'England'
UNION
select year 
       ,sex
       ,'01-04' as age 
       ,sum(a1 + a2 + a3 + a4) as population
       ,lsoa11
       ,'England' as geography
from 
where year = ? and substr(lsoa11, 1, 1) = 'E'
group by year
         ,sex
         ,'01-04'
         ,lsoa11
         ,'England'
UNION
select year
       ,sex
       ,case when quinaryagegroup = '5-9' then '05-09'
        else quinaryagegroup
        end 
       ,sum(popcount) as population
       ,lsoa11
       ,'England' as geography
from 
where year = ? and substr(lsoa11, 1, 1) = 'E' and quinaryagegroupint > 1
group by year
         ,sex
         ,case when quinaryagegroup = '5-9' then '05-09'
		  else quinaryagegroup
          end
          , lsoa11
         ,'England'
)
select pop.year
    , pop.sex
    , pop.age
    , pop.geography
    , a.rgn
    , a.CCG
    , icb.loc22cd
    , icb.icb22cd
    , imd.IMD19_QUINTILE_LSOAS  IMD_QUINTILE
    , sum(pop.population) population
from pop
inner join (select distinct lsoa11 , RGN, CCG  from  where substr(lsoa11, 1,1) = 'E' ) a on pop.lsoa11 = a.lsoa11 /*Update to most recent nspl*/
inner join (select distinct lsoa11cd , loc22cd , icb22cd  from  where substr(lsoa11cd, 1,1) = 'E' ) icb on pop.lsoa11 = icb.lsoa11cd 
left join imd on pop.lsoa11 = imd.lsoa11_code and pop.year = ?
group by pop.year
    , pop.sex
    , pop.age
    , pop.geography
    , a.rgn
    , a.CCG
    , icb.loc22cd
    , icb.icb22cd
    , imd.IMD19_QUINTILE_LSOAS"

# loop through years and extract data ----
years <- first_year:last_year
for (year in years) {
  # extract data for particular years.
  pop <- dbGetQueryOracle(CASREF,popQuery,year,year,year,last_year, timeit = F, rowlimit = NA)

  # set names to lower case
  names(pop) <- str_to_lower(names(pop))
  # save data
  saveRDS(pop, file = paste0("",year,".RDS"))
}
