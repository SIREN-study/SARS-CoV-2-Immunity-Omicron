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
library(lubridate)

# load data
siren_raw <- read.dta13("~/coviddata/SIREN_Interim3_20220228.dta") %>%
    clean_names() %>%
    rename(
        start_date_pos_c = start_date_pos_c_fin,
        first_pcr_pos_date = fir_pc_rpos_date,
        second_pcr_pos_date = sec_pc_rpos_date,
        last_pcr_neg_date = last_pc_rneg_date
    )

# determine the start date for the analysis
start_time <- min(siren_raw$vaccine_date1, na.rm = TRUE) - 1 # this date is 7th December

# remove records with inconsistencies from the SIREN data
siren_cohort <- siren_raw %>%
    mutate(
        ar = pmax(date_enrolled, start_time)
    ) %>%
    filter(
        # (vaccine_name1 %in% c("AstraZeneca","Pfizer-BioNTech") | vaccine_name1==""),
        # (vaccine_name2 %in% c("AstraZeneca","Pfizer-BioNTech") | vaccine_name2==""),
        # (vaccine_name3 %in% c("AstraZeneca","Pfizer-BioNTech") | vaccine_name3==""),
        # (vaccine_name1 == vaccine_name2 | vaccine_name1=="" | vaccine_name2==""),
        (vaccine_date1 < vaccine_date2 | is.na(vaccine_date2) | is.na(vaccine_date1)), # vaccine date checks
        (vaccine_date1 < vaccine_date3 | is.na(vaccine_date3) | is.na(vaccine_date1)),
        (vaccine_date2 < vaccine_date3 | is.na(vaccine_date3) | is.na(vaccine_date2)),
        (is.na(second_pcr_pos_date) | second_pcr_pos_date >= ar), # exclude people who have second infection prior to joining study
        # !(start_date_pos_c > vaccine_date1 & !is.na(start_date_pos_c) & !is.na(vaccine_date1)), # screen out people who become positive after vaccination
        !(cohort_final == 1 & is.na(start_date_pos_c)) # exclude people in positive cohort but with unknown infection date
    )

# n = 38,993

# generate the time and event fields for each participant
siren <- siren_cohort %>%
    # allow people to re-enter the cohort after primary infection but before vaccination
    bind_rows(
        siren_cohort %>%
            filter(
                cohort_final == 0, !is.na(first_pcr_pos_date),
                first_pcr_pos_date <= as_date("2021-09-01"),
                # (first_pcr_pos_date < vaccine_date1 | is.na(vaccine_date1)),
                ar < first_pcr_pos_date
            ) %>%
            mutate(
                cohort_final = 1,
                study_id = paste0(study_id, "_2"),
                ar = first_pcr_pos_date,
                start_date_pos_c = first_pcr_pos_date
            )
    ) %>%
    mutate(
        event = case_when(
            !is.na(first_pcr_pos_date) & first_pcr_pos_date >= ar & cohort_final == 0 ~ 1,
            !is.na(second_pcr_pos_date) & second_pcr_pos_date >= ar & second_pcr_pos_date >= start_date_pos_c & cohort_final == 1 ~ 1,
            TRUE ~ 0
        ),
        time = case_when(
            event == 1 & cohort_final == 0 ~ first_pcr_pos_date,
            event == 1 & cohort_final == 1 ~ second_pcr_pos_date,
            event == 0 ~ last_pcr_neg_date
        ),
        dose_3_regimen = case_when(
            vaccine_name1 == "Pfizer-BioNTech" & vaccine_name2 == "Pfizer-BioNTech" & vaccine_name3 == "Pfizer-BioNTech" ~ 1,
            vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" & vaccine_name3 == "Pfizer-BioNTech" ~ 2,
            TRUE ~ 0
        )
    ) %>%
    filter(
        !is.na(time),
        time > ar
    )

# 40,449 records

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
        region = factor(region, labels = c(
            "East Midlands", "East of England", "London", "North East",
            "North West", "South East", "South West", "West Midlands",
            "Yorkshire and Humber", "Scotland", "Northern Ireland", "Wales"
        )),
        agegr = factor(agegr, labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+")),
        gender = factor(gender, labels = c("Male", "Female", "Non-binary")),
        ethngr = factor(ethngr, labels = c("White", "Asian", "Black", "Mixed", "Other ethnicity", "Unknown ethnicity")),
        work_exposure_frequency = factor(work_exposure_frequency, labels = c("Every day", "Once a week", "Once a month", "Less than once a month", "Never")),
        occ_set = factor(occ_set, labels = c(
            "Office", "Patient facing (non-clinical)", "Outpatient",
            "Maternity,Labour Ward", "Ambulance,Emergency Department,Inpatient",
            "Intensive care", "Theatres", "Other"
        ))
    )

siren %>% count() # n = 40,449 records

saveRDS(siren, here("data/siren_pre_processing.RDS"))