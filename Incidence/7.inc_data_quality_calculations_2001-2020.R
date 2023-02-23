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


# create data quality query which creates DQ row for most recent year to add table 5 in the incidence xlsx----
DQ_table_query <- "WITH ons_patch_pats AS (
    SELECT DISTINCT
        o.nhsno AS nhsnumber
    FROM
        o
        LEFT OUTER JOIN nspl ON replace(
            o.postcode, ' '
        ) = replace(
            nspl.pcd, ' '
        )
    WHERE
        substr(
            o.site4, 1, 1
        ) IN ( 'C', 'D' )
        AND EXTRACT(YEAR FROM o.diagdate) BETWEEN 1995 AND 2007
        AND nspl.ccg IN ( 'E38000056', 'E38000151', 'E38000189', 'E38000200' )
), vs_ons_patch_pats AS (
    SELECT
        vitalstatus,
        vitalstatusdate
        ,
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
                        avp.vitalstatusdate
                        ,
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
), at_tumour AS ( SELECT /*+ USE_HASH(g ca) + USE_HASH(g imd)*/
    to_char(
        t1.tumourid
    )   tumourid,
    to_char(
        t1.patientid
    )   patientid,
    t1.sex,
    t1.age,
    ctry_code,
    t1.diagnosisyear,
    CASE
    WHEN t1.diagnosisyear < 2013  THEN
    t1.site_icd10_o2
    WHEN t1.diagnosisyear >= 2013 THEN
    t1.site_icd10r4_o2_from2013
    ELSE
    NULL
    END AS icd10code
FROM
    t1
    INNER JOIN g ON g.tumourid = t1.tumourid
    LEFT JOIN ats ON t1.tumourid = ats.tumourid
    LEFT OUTER JOIN ca ON g.lsoa11_code = ca.lsoa11cd
WHERE
    t1.diagnosisyear BETWEEN 2013 AND ? --lastyear
    AND ctry_code = 'E'
    AND statusofregistration = 'F'
    AND dedup_flag = 1
    AND t1.sex IN ( '1', '2' )
    AND ( ( (--All cases except NMSC ,melanoma taken from at_tumour instaed at at_tumour_skin
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
        --All NMSC cases from at_tumour_skin
           ( skinid IS NOT NULL
               AND ( tumour_type_2 = 'Keratinocyte cancer'
                     OR tumour_type_1 = 'Extramammary paget disease'
                     OR tumour_type_2 = 'Rare' )
               AND site_icd10rns_o2_3char = 'C44'
               AND ats.diagnosisyear = ? --lastyear
                )
          OR --All NMSC C44 between 1995 and 2012 not in at_tumour_skin
           ( t1.site_icd10_o2_pre2013 LIKE 'C44%'
               AND t1.diagnosisyear BETWEEN 1995 AND 2012 ) )
UNION

/* Cases for 4 NW CCGs from 1995-2007 from ONS data */

SELECT   /*+ USE_HASH(o1 vs)  + USE_HASH(vs nspl)  + USE_HASH(vs nsplnew) + USE_HASH(vs ccg) + USE_HASH(vs gor) */
  /* IDs */

    o1.tumournumbermerge                                                             AS tumourid,
    coalesce(
        to_char(
            vs.patientid
        ), to_char(
            o1.patientnumbermerge
        )
    )                                                                                AS patientid,
    o1.sex,
    floor(to_number(o1.diagdate - trunc(o1.dob1 +(o1.dob2 - o1.dob1) / 2)) / 365.25) AS age,
    'E'                                                                              AS ctry_code,
    EXTRACT(YEAR FROM o1.diagdate)                                                   diagnosisyear,
    o1.site4                                                                         AS site_icd10
FROM
    o1
    LEFT OUTER JOIN vs ON o1.nhsno = vs.nhsnumber
    LEFT OUTER JOIN nspl ON replace(
        o1.postcode, ' '
    ) = replace(
            nspl.pcd, ' '
    WHERE
        nspl.ccg IN ( 'E38000056', 'E38000151', 'E38000189', 'E38000200' )
        AND EXTRACT(YEAR FROM o1.diagdate) BETWEEN 2001 AND 2007
), data AS (
    SELECT
        diagnosisyear year,
        CASE
        WHEN ctry_code IS NULL THEN
        '3'
        WHEN sex NOT IN ( 1, 2 ) THEN
        '3'
        WHEN age NOT BETWEEN 0 AND 200 THEN
        '3'
        WHEN icd10code IS NULL THEN
        '3'
        WHEN substr(
            icd10code, 1, 1
        ) NOT IN ( 'C', 'D' ) THEN
        '3'
        WHEN sex = 1
             AND substr(
            icd10code, 1, 3
        ) BETWEEN 'C51' AND 'C58' THEN
        '2'
        WHEN sex = 2
             AND substr(
            icd10code, 1, 3
        ) BETWEEN 'C60' AND 'C63' THEN
        '2'
        WHEN substr(
            icd10code, 1, 1
        ) = 'D'                   THEN
        'D'
        WHEN substr(
            icd10code, 1, 3
        ) = 'C44'                 THEN
        'NMSC'
        ELSE
        '1'
        END           dq,
        COUNT(*)      howmany
    FROM
        at_tumour
    WHERE
        diagnosisyear = ? --lastyear

    GROUP BY
        diagnosisyear,
        CASE
            WHEN ctry_code IS NULL THEN
            '3'
            WHEN sex NOT IN ( 1, 2 ) THEN
            '3'
            WHEN age NOT BETWEEN 0 AND 200 THEN
            '3'
            WHEN icd10code IS NULL THEN
            '3'
            WHEN substr(
                icd10code, 1, 1
            ) NOT IN ( 'C', 'D' ) THEN
            '3'
            WHEN sex = 1
                 AND substr(
                icd10code, 1, 3
            ) BETWEEN 'C51' AND 'C58' THEN
            '2'
            WHEN sex = 2
                 AND substr(
                icd10code, 1, 3
            ) BETWEEN 'C60' AND 'C63' THEN
            '2'
            WHEN substr(
                icd10code, 1, 1
            ) = 'D'                   THEN
            'D'
            WHEN substr(
                icd10code, 1, 3
            ) = 'C44'                 THEN
            'NMSC'
            ELSE
            '1'
        END
    ORDER BY
        1
)
SELECT
    *
FROM
    data PIVOT (
        MAX ( howmany )
        FOR dq
        IN ( 1 AS status_1, 2 AS status_2, 3 AS status_3 )
    )
ORDER BY
    1"
# extract DQ table ----
DQ_table_raw <-
  dbGetQueryOracle(
    CAS,
    DQ_table_query,
    last_year,
    last_year,
    last_year,
    last_year,
    timeit = TRUE,
    rowlimit = NA
  )
## format table ----
DQ_table <- DQ_table_raw %>%
  mutate(
    STATUS_1 = ifelse(is.na(STATUS_1), 0, STATUS_1),
    STATUS_2 = ifelse(is.na(STATUS_2), 0, STATUS_2),
    STATUS_3 = ifelse(is.na(STATUS_3), 0, STATUS_3)
  ) %>%
  mutate(
    Total = STATUS_1 + STATUS_2 + STATUS_3,
    Percentage_of_Status_3 = round(100 * STATUS_3 / Total, 1)
  ) %>%
  select(
    Year = YEAR,
    Total,
    Status_1 = STATUS_1,
    Status_2 = STATUS_2,
    Status_3 = STATUS_3,
    Percentage_of_Status_3
  ) %>%
  arrange(Year)

## save DQ table ----
fwrite(DQ_table, file = "")

# close and rm connection ----
dbDisconnect(CAS)

rm(CAS)
