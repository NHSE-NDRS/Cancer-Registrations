# Setup ----
library(data.table)
library(dplyr)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# make sure the oracle_connection.R is in the same folder as your script or put I full path into the source function
source("oracle_connection.R")

# connect to CAS ----

CAS <-
  createConnection(sid = "",
                   username = "",
                   port =)

# Set years of interest ----
first_year <- 2001
last_year <- 2020

# create queries for inc and pop ----
incQuery <- "WITH ons_patch_pats AS (
    SELECT DISTINCT
        o.nhsno AS nhsnumber
    FROM
        o /*it is o because this is the last incidence table we have from ONS*/
        LEFT OUTER JOIN nspl ON replace(
            o.postcode, ' '
        ) = replace(
            nspl.pcd, ' '
        ) /* DO NOT use the nspl alias here as using the alias will call the most recent dataset which drops most of the 4 CCGs*/
    WHERE
        substr(
            o.site4, 1, 1
        ) IN ( 'C', 'D' )
        AND EXTRACT(YEAR FROM o.diagdate) BETWEEN 1995 AND 2007
        AND nspl.ccg IN ( 'E38000056', 'E38000151', 'E38000189', 'E38000200' )
), vs_ons_patch_pats AS (
    SELECT
        vitalstatus,
        vitalstatusdate,
        nhsnumber,
        patientid
    FROM
        (
            SELECT DISTINCT
                vitalstatus,
                MAX(vitalstatusdate)
                OVER(PARTITION BY vitalstatus, nhsnumber) AS vitalstatusdate,
                nhsnumber,
                MIN(patientid)
                OVER(PARTITION BY vitalstatus, nhsnumber) AS patientid,
                RANK()
                OVER(PARTITION BY nhsnumber
                     ORDER BY
                         vitalstatus DESC, vs_order ASC
                )                                         AS row_num
            FROM
                (
                    SELECT
                        avp.vitalstatus,
                        avp.vitalstatusdate,
                        avp.nhsnumber,
                        avp.patientid,
                        avp.aliasflag,
                        MIN(avp.aliasflag)
                        OVER(PARTITION BY avp.nhsnumber) AS min_aliasflag,
                        avp.tumourcount,
                        MAX(avp.tumourcount)
                        OVER(PARTITION BY avp.nhsnumber) AS max_tum_count,
                        CASE
                        WHEN avp.vitalstatus = 'D' THEN
                        1
                        WHEN avp.vitalstatus = 'A' THEN
                        2
                        WHEN avp.vitalstatus IN ( 'X', 'X3' ) THEN
                        3 /*X3 is embarked or cannot be successfully traced*/
                        WHEN avp.vitalstatus IN ( 'D4', 'D5' ) THEN
                        4 /*D4= treated after death, D5 = traced alive after death*/
                        ELSE
                        9 /*values assigned to 9 is missing, D3 (diagnosed after death), X2 (not traced yet) and I (aliased off)*/
                        END                              vs_order
                    FROM
                        op
                        INNER JOIN avp ON op.nhsnumber = avp.nhsnumber
                )
            WHERE
                aliasflag = min_aliasflag
                AND tumourcount = max_tum_count
        )
    WHERE
        row_num = 1
)
SELECT /*+ USE_HASH(g ca) + USE_HASH(g imd)*/
    to_char(
        t1.tumourid
    )                        tumourid,
    to_char(
        t1.patientid
    )                        patientid,
    t1.sex,
    t1.age,
    t1.birthdatebest,
    t1.diagnosisdatebest,
    t1.site_coded,
    t1.site_icd10_o2,
    t1.site_icd10r4_o2_from2013,
    CASE
    WHEN t1.diagnosisyear < 2013  THEN
    t1.site_icd10_o2
    WHEN t1.diagnosisyear >= 2013 THEN
    t1.site_icd10r4_o2_from2013
    ELSE
    NULL
    END                      AS icd10code,
    CASE
    WHEN t1.diagnosisyear < 2013  THEN
    t1.site_icd10_o2_3char
    WHEN t1.diagnosisyear >= 2013 THEN
    t1.site_icd10r4_o2_3char_from2013
    ELSE
    NULL
    END                      AS icd10code_3char,
    t1.site_coded_desc,
    t1.site_coded_3char,
    t1.behaviour_coded,
    t1.morph_coded,
    t1.morph_icd10_o2,
    t1.coding_system,
    t1.coding_system_desc,
    t1.behaviour_icd10_o2,
    t1.behaviour_coded_desc,
    t1.stage_best,
    t1.stage_best_system,
    t1.stage_pi,
    t1.stage_pi_detail,
    t1.m_best,
    t1.her2_status,
    t1.er_status,
    t1.pr_status,
    t1.grade,
    t1.gleason_primary,
    t1.gleason_secondary,
    t1.gleason_combined,
    g.lsoa11_code,
    g.ccg_2021_code          ccg21cd,
    g.ccg_2021_name          ccg21nm,
    icb.loc22cd,
    icb.loc22cdh,
    icb.loc22nm,
    icb.icb22cd,
    icb.icb22cdh,
    icb.icb22nm,
    g.gor_code,
    g.gor_name,
    imd.imd19_quintile_lsoas imd_quintile
FROM
    t1
    INNER JOIN g ON g.tumourid = t1.tumourid
    LEFT OUTER JOIN ca ON g.lsoa11_code = ca.lsoa11cd
    LEFT JOIN icb ON g.lsoa11_code = icb.lsoa11cd
    LEFT JOIN ats ON t1.tumourid = ats.tumourid
    LEFT JOIN imd ON g.lsoa11_code = imd.lsoa11_code
                                                      AND t1.diagnosisyear = ? --lastyear
WHERE
    t1.diagnosisyear = ? --year
    AND ctry_code = 'E'
    AND statusofregistration = 'F'
    AND dedup_flag = 1
    AND t1.sex IN ( '1', '2' )
    AND ( --All cases except NMSC ,melanoma taken from at_tumour instaed at at_tumour_skin
     ( (
        CASE
        WHEN t1.diagnosisyear < 2013  THEN
        t1.site_icd10_o2_pre2013
        WHEN t1.diagnosisyear >= 2013 THEN
        t1.site_icd10r4_o2_from2013
        ELSE
        NULL
        END
    ) NOT LIKE 'C44%'
            AND t1.diagnosisyear BETWEEN 1995 AND ? --lastyear
             )
          OR
--All NMSC cancer cases from at_tumour_skin
           ( skinid IS NOT NULL
               AND ( tumour_type_5 = 'First BCC'
                     OR tumour_type_5 = 'First cSCC'
                     OR tumour_type_1 = 'Extramammary paget disease'
                     OR tumour_type_2 = 'Rare' )
               AND site_icd10rns_o2_3char = 'C44'
               AND ats.diagnosisyear BETWEEN 2013 AND ? --lastyear
                )
          OR --All NMSC between 1995 and 2012 not in at_tumour_skin
           ( t1.site_icd10_o2_pre2013 LIKE 'C44%'
               AND t1.diagnosisyear BETWEEN 1995 AND 2012 ) )
    AND ( t1.diagnosisyear NOT BETWEEN 1995 AND 2007
          OR ca.ccg17cd NOT IN ( 'E38000056', 'E38000151', 'E38000189', 'E38000200' ) )
UNION

/* Cases for 4 NW CCGs from 1995-2007 from ONS data */

SELECT   /*+ USE_HASH(o1 vs)  + USE_HASH(vs nspl)  + USE_HASH(vs nsplnew) + USE_HASH(vs ccg) + USE_HASH(vs gor) */
/* IDs */

    o1.tumournumbermerge                        AS tumourid,
    coalesce(
        to_char(
            vs.patientid
        ), to_char(
            o1.patientnumbermerge
        )
    )                                           AS patientid,
    o1.sex,
    floor(to_number(
        trunc(o1.diagdate - o1.dob1 +(o1.dob2 - o1.dob1) / 2)
    ) / 365.25)                                 AS age,
    ( trunc(o1.dob1 +(o1.dob2 - o1.dob1) / 2) ) AS birthdatebest,
    o1.diagdate                                 AS diagnosisdatebest,
    o1.site4                                    AS site_coded,
    NULL                                        AS site_icd10_o2,
    NULL                                        AS site_icd10r4_o2_from2013,
    o1.site4                                    AS site_icd10,
    substr(
        o1.site4, 1, 3
    )                                           AS icd10code_3char,
    'ONS data'                                  AS site_coded_desc,
    substr(
        o1.site4, 1, 3
    )                                           AS site_coded_3char,
    substr(
        o1.type5, - 1, 1
    )                                           AS behaviour_coded,
    substr(
        o1.type5, 1, 4
    )                                           AS morph_coded,
    substr(
        o1.type5, 1, 4
    )                                           AS morph_icd10_o2,
    0                                           AS coding_system,
    'ONS data'                                  AS coding_system_desc,
    substr(
        o1.type5, - 1, 1
    )                                           AS behaviour_icd10_o2,
    'ONS data'                                  AS behaviour_coded_desc,
    NULL                                        AS stage_best,
    NULL                                        AS stage_best_system,
    NULL                                        AS stage_pi,
    NULL                                        AS stage_pi_detail,
    NULL                                        AS m_best,
    NULL                                        AS her2_status,
    NULL                                        AS er_status,
    NULL                                        AS pr_status,
    NULL                                        AS grade,
    NULL                                        AS gleason_primary,
    NULL                                        AS gleason_secondary,
    NULL                                        AS gleason_combined,
    nsplnew.lsoa11,
    ccg.ccg21cd,
    ccg.ccg21nm,
    icb.loc22cd,
    icb.loc22cdh,
    icb.loc22nm,
    icb.icb22cd,
    icb.icb22cdh,
    icb.icb22nm,
    gor.gor10cd                                 gor_code,
    gor.gor10nm                                 gor_name,
    NULL                                        AS imd_quintile
FROM
    o1
    LEFT OUTER JOIN vs ON o1.nhsno = vs.nhsnumber
    LEFT OUTER JOIN nspl ON replace(
        o1.postcode, ' '
    ) = replace(
        nspl.pcd, ' '
    )
    LEFT OUTER JOIN nsplnew ON replace(
        o1.postcode, ' '
    ) = replace(
        nsplnew.pcd, ' '
    )
    LEFT JOIN gor ON nsplnew.rgn = gor.gor10cd
    LEFT JOIN ccg ON ccg.lsoa11cd = nsplnew.lsoa11
    LEFT JOIN icb ON nsplnew.lsoa11 = icb.lsoa11cd
WHERE
    nspl.ccg IN ( 'E38000056', 'E38000151', 'E38000189', 'E38000200' )
    AND substr(
        o1.site4, 1, 1
    ) IN ( 'C', 'D' )
    AND EXTRACT(YEAR FROM o1.diagdate) BETWEEN 1995 AND 2007
    AND EXTRACT(YEAR FROM o1.diagdate) = ? --year
"

# loop through years and extract data ----
years <- first_year:last_year
for (year in years) {
  # extract data for particular years.
  inc <-
    dbGetQueryOracle(CAS,
                     incQuery,
                     last_year,
                     year,
                     last_year,
                     last_year,
                     year,
                     rowlimit = NA) # this is for all the ? diagnosisyear to define which we use
  
  
  if (year < 2013) {
    inc$STAGE_PI_DETAIL <- NA
  }
  # set date format to remove the time stamp
  #for 2019 registrations used as.Date changed to as.POSIXct for 2020 registrations due to issues with date formats in R encountered in NPCA
  #Version of as.date extract saved for archive
  inc$BIRTHDATEBEST <- as.POSIXct(inc$BIRTHDATEBEST)
  inc$DIAGNOSISDATEBEST <- as.POSIXct(inc$DIAGNOSISDATEBEST)
  # set names to lower case
  names(inc) <- str_to_lower(names(inc))
  # save data
  saveRDS(inc,
          file = paste0(""))
}


# close and rm connection ----
dbDisconnect(CAS)

rm(CAS)
