#
# Pre-process R file
# Imports and cleans the SIREN data and generates datasets
#

here::i_am("r/pre-process.R")

# libraries
library(readstata13)
library(tidyverse)
library(janitor)
library(here)

# load data
siren_raw <- read.dta13(here("data/SIREN_interim3_20220228_Omicron.dta")) %>%
    clean_names() %>%
    rename(
        start_date_pos_c = start_date_pos_c_30nov,
        first_pcr_pos_date = fir_pc_rpos_date,
        second_pcr_pos_date = sec_pc_rpos_date,
        last_pcr_neg_date = last_pc_rneg_date
    )

# determine the start date for the analysis
start_time <- lubridate::as_date("2021-12-01")

# remove records with inconsistencies from the SIREN data
siren_cohort <- siren_raw %>%
    mutate(
        ar = start_time
    ) %>%
    filter(
        (is.na(second_pcr_pos_date) | second_pcr_pos_date >= ar),
        !(start_date_pos_c > vaccine_date1 & !is.na(start_date_pos_c) & !is.na(vaccine_date1))
    )

# n = 18,484

# generate the time and event fields for each participant
siren <- siren_cohort %>%
    mutate(
        cohort_final = case_when(
            start_date_pos_c < ar ~ 1,
            TRUE ~ 0
        ),
        event = case_when(
            !is.na(first_pcr_pos_date) & first_pcr_pos_date >= ar & cohort_final == 0 ~ 1,
            !is.na(second_pcr_pos_date) & second_pcr_pos_date >= ar & cohort_final == 1 ~ 1,
            TRUE ~ 0
        ),
        time = case_when(
            event == 1 & !is.na(cohort_final) ~ first_pcr_pos_date,
            event == 1 & cohort_final == 1 ~ second_pcr_pos_date,
            event == 0 ~ last_pcr_neg_date
        ),
        dose_3_regimen = case_when(
            vaccine_name1=="Pfizer-BioNTech" & vaccine_name2=="Pfizer-BioNTech" & vaccine_name3=="Pfizer-BioNTech" ~ 1,
            vaccine_name1=="AstraZeneca" & vaccine_name2=="AstraZeneca" & vaccine_name3=="Pfizer-BioNTech" ~ 2,
            TRUE ~ 0
        )
    ) %>%
    filter(
        !is.na(time),
        time > ar
    )

# generate relative time variables to use in Cox proportional hazards model
siren <- siren %>%
    mutate(
        start_date_pos_c = start_date_pos_c - start_time,
        time = time - start_time,
        ar = ar - start_time,
        vd1 = vaccine_date1 - start_time,
        vd2 = vaccine_date2 - start_time,
        vd3 = vaccine_date3 - start_time,
        follow_up_time = time,
        region = factor(region, labels = c("East Midlands","East of England","London","North East",
                                           "North West","South East","South West","West Midlands",
                                           "Yorkshire and Humber","Scotland","Northern Ireland","Wales")),
        agegr = factor(agegr, labels = c("<25","25-34","35-44","45-54","55-64","65+")),
        gender = factor(gender, labels = c("Male","Female","Non-binary")),
        ethngr = factor(ethngr, labels = c("White","Asian","Black","Mixed","Other ethnicity","Unknown ethnicity")),
        work_exposure_frequency = factor(work_exposure_frequency, labels = c("Every day","Once a week","Once a month","Less than once a month","Never")),
        occ_set = factor(occ_set, labels = c("Office","Patient facing (non-clinical)","Outpatient",
                                                     "Maternity,Labour Ward","Ambulance,Emergency Department,Inpatient",
                                                     "Intensive care","Theatres","Other"))
    )

siren %>% count() # n = 17,425

saveRDS(siren, here("data/siren_pre_processing.RDS"))

