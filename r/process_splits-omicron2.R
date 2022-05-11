#
# Process splits R file
# Uses the survival library and tmerge function to split data into time chunks for Cox proportional hazards models
#

# libraries
library(tidyverse)
library(survival)

# load data
siren <- readRDS(here("data/siren_pre_processing.RDS"))
len <- nrow(siren)

# Vaccine effectiveness dataset

split_ve_old <- tmerge(
    data1 = siren,
    data2 = siren,
    id = study_id,
    tstop = time,
    event = event(time, event),
    eligible = tdc(ar),
    omicron = tdc(rep(369,len)), # change to 369 (from 359) for 10th December
    primary_inf = cumtdc(start_date_pos_c),
    primary_inf = cumtdc(start_date_pos_c + 90),
    primary_inf = cumtdc(start_date_pos_c + 365),
    vaccine = event(vd1, rep(1, len)),
    vaccine = event(if_else(vd1 + 20 < vd2 & !is.na(vd2), vd1 + 20, NA_real_), rep(2, len)),
    vaccine = event(if_else(is.na(vd2), vd1 + 20, NA_real_), rep(2, len)), # to deal with the vd2==NA case
    vaccine = event(vd2, rep(3, len)),
    vaccine = event(if_else(vd2 + 13 < vd3 & !is.na(vd3), vd2 + 13, NA_real_), rep(4, len)),
    vaccine = event(if_else(is.na(vd3), vd2 + 13, NA_real_), rep(4, len)), # to deal with the vd3==NA case
    vaccine = event(if_else(vd2 + 193 < vd3 & !is.na(vd3), vd2 + 193, NA_real_), rep(5, len)),
    vaccine = event(if_else(is.na(vd3), vd2 + 193, NA_real_), rep(5, len)), # to deal with the vd3==NA case
    vaccine = event(if_else(vd2 + 253 < vd3 & !is.na(vd3), vd2 + 253, NA_real_), rep(6, len)),
    vaccine = event(if_else(is.na(vd3), vd2 + 253, NA_real_), rep(6, len)), # to deal with the vd3==NA case
    vaccine = event(vd3, rep(7, len)),
    vaccine = event(vd3 + 6, rep(8, len)),
    vaccine = event(vd3 + 66, rep(9, len)),
    vaccine = event(vd3 + 126, rep(10, len))
) %>%
    group_by(study_id) %>%
    # carry over the 0 vaccine status
    mutate(
        vaccine = lag(vaccine),
        vaccine = if_else(vaccine == 0, lag(vaccine), vaccine),
        vaccine = if_else(vaccine == 0, lag(vaccine), vaccine),
        vaccine = if_else(vaccine == 0, lag(vaccine), vaccine),
        vaccine = if_else(is.na(vaccine), 0, vaccine)
    ) %>%
    ungroup() %>%
    mutate(
        vaccine_cat = case_when(
            # AZ+PF 3
            vaccine == 7 & dose_3_regimen == 2 ~ 12,
            vaccine == 8 & dose_3_regimen == 2 ~ 13,
            vaccine %in% c(9,10) & dose_3_regimen == 2 ~ 14,
            # AZ 2
            vaccine %in% c(5,6) & vaccine_name1 == "AstraZeneca" & vaccine_name2 == "AstraZeneca" ~ 11,
            # Pre-vaccination
            TRUE ~ vaccine
        ),
        vaccine_cat = factor(vaccine_cat, levels = c(0:15)),
        primary_inf = factor(primary_inf)
    ) %>%
    mutate(
        # vaccine eligibility
        # don't consider non-AZ/PF follow-up post-first dose
        eligible = if_else(vaccine_name1 !="AstraZeneca" & vaccine_name1 != "Pfizer-BioNTech" & vaccine>0, as.integer(0), eligible),
        # don't consider follow-up period between AZ vaccine and PF booster
        eligible = if_else(vaccine_name1 == "AstraZeneca" & vaccine>0, as.integer(0), eligible),
        # don't consider PF followed by non-PF
        eligible = if_else(vaccine_name1 == "Pfizer-BioNTech" & vaccine_name2 != "Pfizer-BioNTech" & vaccine > 2, as.integer(0), eligible),
        # do consider 2AZ + PF booster
        eligible = if_else(dose_3_regimen == 2 & vaccine>=7 & tstart>ar, as.integer(1), eligible),
        # don't consider any other booster
        eligible = if_else(dose_3_regimen == 0 & vaccine>=7, as.integer(0), eligible),
        # do consider AZ naive
        eligible = if_else(vaccine_cat == 11 & primary_inf==0 & tstart>ar, as.integer(1), eligible),

        # exclusions
        # don't consider unknown cohort
        eligible = if_else(cohort_final==9, as.integer(0), eligible),
        # don't consider records with an infection within 90 days
        eligible = if_else(primary_inf == 1, as.integer(0), eligible),
    )

# check exposure and events in each category
split_ve %>%
    filter(eligible == 1, omicron == 1) %>%
    filter(!(vaccine_cat %in% c(0,1,2,3,4) & primary_inf %in% c(2,3)),
           !(dose_3_regimen == 2  & primary_inf %in% c(2,3))) %>%
    group_by(primary_inf, vaccine_cat) %>%
    summarise(
        particiants = n(),
        events = sum(event),
        exposure = sum(tstop - tstart)
    ) %>% print(n = 100)


save(split_ve, siren, file = "~/coviddata/siren_post_processing.RData")
