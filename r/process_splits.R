#
# Process splits R file
# Uses the survival library and tmerge function to split data into time chunks for Cox proportional hazards models
#

here::i_am("r/process_splits.R")

# libraries
library(tidyverse)
library(survival)
library(here)

# load data
siren <- readRDS(here("data/siren_pre_processing.RDS"))
len <- nrow(siren)

# Vaccine effectiveness dataset

split_ve <- tmerge(
    data1 = siren,
    data2 = siren,
    id = study_id,
    tstop = time,
    event = event(time, event),
    eligible = tdc(ar),
    alpha_delta = tdc(rep(359,len)), # 1st December 2021 cutoff
    omicron = tdc(rep(369,len)), # 10th December 2021 cutoff
    primary_inf = cumtdc(start_date_pos_c),
    primary_inf = cumtdc(start_date_pos_c + 90),
    primary_inf = cumtdc(start_date_pos_c + 182),
    primary_inf = cumtdc(start_date_pos_c + 273),
    primary_inf = cumtdc(start_date_pos_c + 365),
    primary_inf = cumtdc(start_date_pos_c + 486),
    vaccine = event(vd1, rep(1, len)),
    vaccine = event(if_else(vd1 + 20 < vd2 & !is.na(vd2), vd1 + 20, NA_real_), rep(2, len)),
    vaccine = event(if_else(is.na(vd2), vd1 + 20, NA_real_), rep(2, len)), # to deal with the vd2==NA case
    vaccine = event(vd2, rep(3, len)),
    vaccine = event(if_else(vd2 + 13 < vd3 & !is.na(vd3), vd2 + 13, NA_real_), rep(4, len)),
    vaccine = event(if_else(is.na(vd3), vd2 + 13, NA_real_), rep(4, len)), # to deal with the vd3==NA case
    vaccine = event(if_else(vd2 + 73 < vd3 & !is.na(vd3), vd2 + 73, NA_real_), rep(5, len)),
    vaccine = event(if_else(is.na(vd3), vd2 + 73, NA_real_), rep(5, len)), # to deal with the vd3==NA case
    vaccine = event(if_else(vd2 + 133 < vd3 & !is.na(vd3), vd2 + 133, NA_real_), rep(6, len)),
    vaccine = event(if_else(is.na(vd3), vd2 + 133, NA_real_), rep(6, len)), # to deal with the vd3==NA case
    vaccine = event(if_else(vd2 + 193 < vd3 & !is.na(vd3), vd2 + 193, NA_real_), rep(7, len)),
    vaccine = event(if_else(is.na(vd3), vd2 + 193, NA_real_), rep(7, len)), # to deal with the vd3==NA case
    vaccine = event(if_else(vd2 + 253 < vd3 & !is.na(vd3), vd2 + 253, NA_real_), rep(8, len)),
    vaccine = event(if_else(is.na(vd3), vd2 + 253, NA_real_), rep(8, len)), # to deal with the vd3==NA case
    vaccine = event(vd3, rep(9, len)),
    vaccine = event(vd3 + 6, rep(10, len)),
    vaccine = event(vd3 + 66, rep(11, len)),
    vaccine = event(vd3 + 126, rep(12, len))
) %>%
    group_by(study_id) %>%
    # carry over the 0 vaccine status
    mutate(
        vaccine = lag(vaccine),
        vaccine = if_else(vaccine == 0, lag(vaccine), vaccine),
        vaccine = if_else(vaccine == 0, lag(vaccine), vaccine),
        vaccine = if_else(vaccine == 0, lag(vaccine), vaccine),
        vaccine = if_else(vaccine == 0, lag(vaccine), vaccine),
        vaccine = if_else(is.na(vaccine), 0, vaccine)
    ) %>%
    ungroup() %>%
    mutate(
        vaccine_cat = case_when(
            # AZ vaccines
            vaccine == 3 & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" ~ 13,
            vaccine == 4 & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" ~ 14,
            vaccine == 5 & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" ~ 15,
            vaccine == 6 & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" ~ 16,
            vaccine %in% c(7, 8) & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" ~ 17,
            vaccine == 9 & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" & vaccine_name3 == "AstraZeneca" ~ 18,
            vaccine == 10 & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" & vaccine_name3 == "AstraZeneca" ~ 19,
            vaccine %in% c(11, 12) & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" & vaccine_name3 == "AstraZeneca" ~ 20,
            vaccine == 9 & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" & vaccine_name3 == "Pfizer-BioNTech" ~ 21,
            vaccine == 10 & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" & vaccine_name3 == "Pfizer-BioNTech" ~ 22,
            vaccine %in% c(11, 12) & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" & vaccine_name3 == "Pfizer-BioNTech" ~ 23,
            # Pre-vaccination
            TRUE ~ vaccine
        ),
        vaccine_cat = factor(vaccine_cat, levels = c(0:23)),
        primary_inf = factor(primary_inf),
        alpha_delta = 1 - alpha_delta, # recode alpha_delta flag

        # eligibility checks
        eligible = case_when(cohort_final == 9 ~ as.integer(0), # don't consider unknown cohort
                             primary_inf == 1 ~ as.integer(0), # don't consider reinfection within 90 days
                             # non-AZ or PF vaccines
                             vaccine_name1 != "AstraZeneca" & vaccine_name1 != "Pfizer-BioNTech" & vaccine >= 1 ~ as.integer(0),
                             vaccine_name2 != "AstraZeneca" & vaccine_name2 != "Pfizer-BioNTech" & vaccine >= 3 ~ as.integer(0),
                             vaccine_name3 != "AstraZeneca" & vaccine_name3 != "Pfizer-BioNTech" & vaccine >= 9 ~ as.integer(0),

                             vaccine_name1 == "AstraZeneca" & vaccine %in% c(1, 2) ~ as.integer(0), # single AZ vaccine
                             vaccine_name1 != vaccine_name2 & vaccine >= 3 ~ as.integer(0), # different first and second vaccine
                             vaccine_name1 == "Pfizer-BioNTech" & vaccine_name2 == "Pfizer-BioNTech" & vaccine_name3 == "AstraZeneca" & vaccine >= 9 ~ as.integer(0), # PF followed by AZ
                             TRUE ~ eligible)
    )

# check exposure and events in each category
split_ve %>%
    filter(eligible == 1, alpha_delta==1) %>%
    group_by(vaccine_cat) %>%
    summarise(
        particiants = n_distinct(study_id),
        events = sum(event),
        exposure = sum(tstop - tstart)
    ) %>%
    print(n = 100)

split_ve %>%
    filter(eligible == 1, omicron==1, vaccine_cat %in% c(8,9,10,11,12,17,21,22,23)) %>%
    group_by(vaccine_cat) %>%
    summarise(
        particiants = n_distinct(study_id),
        events = sum(event),
        exposure = sum(tstop - tstart)
    ) %>% print(n = 100)

save(split_ve, siren, file = "~/coviddata/siren_post_processing.RData")
