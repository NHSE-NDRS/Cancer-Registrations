#install.packages("DataLakeR")

#Using R in the Data Lake
#need to git installed to access the dataklake via R
#The DataLakeR package can be installed using the following code in R:

if (!require(devtools)) install.packages("devtools")
devtools::install_git('https://gitlab.phe.gov.uk/packages/DataLakeR')

library(dplyr)
library(tidyr)
library(PHEindicatormethods)
library(zoo)
library(data.table)
library(DataLakeR)

#for 2022 run copied over population file from last year's run to use as nothing shoudl have chnaged 
#EF couldn't run this extraction from the datalake as didnlt have the right packages installed

#Clear the workspace
rm(list= ls())

#set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pop <- DataLakeR::get_datalake(paste0("SELECT        
		Top (100) percent
		  [Period] as 'year'
      ,[Sex]
      ,case when [QuinaryAgeBand] = '85+' then '85+'
      	else concat(SUBSTRING([QuinaryAgeBand],1,2),'-',SUBSTRING([QuinaryAgeBand],3,2))
      end as 'age_gp'
      ,round(sum([Population]),0) as 'population'
    FROM [Populations].[dbo].[vRes_zLSOA11_FiveYear_Pre2001_85plus]
    where Period between 1995 and 2000
      and [Sex] != 4
      and [OfficialCode] like 'E%'
    group by [Period]
      ,[Sex]
      ,[QuinaryAgeBand]
    order by [Period] 
      ,[QuinaryAgeBand]
"))

saveRDS(pop,"population_extraction_95-00.RDS")
