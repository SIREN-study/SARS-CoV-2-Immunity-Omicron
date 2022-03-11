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

## time splits:
## PF
## 1. pre-vaccination
## 2. at(0 20) after(vd1 PF)
## 3. at(0 13 193) after(vd2 PF)
## 4. at(0 6 66) after(vd3 3PF)
## 4. at(0 6 66) after(vd3 2AZ/1PF)
## at(alpha/delta/359 (omicron)) after(time 0) for regression

split_ve <- tmerge(
    data1 = siren,
    data2 = siren,
    id = study_id,
    # tstart = -359, # tstart required for omicron-only analysis
    tstop = time,
    event = event(time, event),
    eligible = tdc(ar),
    omicron = tdc(rep(359,len)), # change to 0 (from 359) for omicron-only analysis
    primary_inf = cumtdc(start_date_pos_c),
    primary_inf = cumtdc(start_date_pos_c + 90),
    vaccine = event(vd1, rep(1, len)),
    vaccine = event(if_else(vd1 + 20 < vd2 & !is.na(vd2), vd1 + 20, NA_real_), rep(2, len)),
    vaccine = event(if_else(is.na(vd2), vd1 + 20, NA_real_), rep(2, len)), # to deal with the vd2==NA case
    vaccine = event(vd2, rep(3, len)),
    vaccine = event(if_else(vd2 + 13 < vd3 & !is.na(vd3), vd2 + 13, NA_real_), rep(4, len)),
    vaccine = event(if_else(is.na(vd3), vd2 + 13, NA_real_), rep(4, len)), # to deal with the vd3==NA case
    vaccine = event(if_else(vd2 + 193 < vd3 & !is.na(vd3), vd2 + 193, NA_real_), rep(5, len)),
    vaccine = event(if_else(is.na(vd3), vd2 + 193, NA_real_), rep(5, len)), # to deal with the vd3==NA case
    vaccine = event(vd3, rep(6, len)),
    vaccine = event(vd3 + 6, rep(7, len)),
    vaccine = event(vd3 + 66, rep(8, len))
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
            # PF 3 Delta
            vaccine == 6 & dose_3_regimen == 1 & omicron == 0 ~ 6,
            vaccine == 7 & dose_3_regimen == 1 & omicron == 0 ~ 7,
            vaccine == 8 & dose_3_regimen == 1 & omicron == 0 ~ 7, # group 0-6 and 7+
            # AZ+PF Delta
            vaccine == 6 & dose_3_regimen == 2 & omicron == 0 ~ 8,
            vaccine == 7 & dose_3_regimen == 2 & omicron == 0 ~ 9,
            vaccine == 8 & dose_3_regimen == 2 & omicron == 0 ~ 9, # group 0-6 and 7+
            # PF 3 Omicron
            vaccine == 6 & dose_3_regimen == 1 & omicron == 1 ~ 11,
            vaccine == 7 & dose_3_regimen == 1 & omicron == 1 ~ 12,
            vaccine == 8 & dose_3_regimen == 1 & omicron == 1 ~ 13,
            # AZ+PF Omicron
            vaccine == 6 & dose_3_regimen == 2 & omicron == 1 ~ 14,
            vaccine == 7 & dose_3_regimen == 2 & omicron == 1 ~ 15,
            vaccine == 8 & dose_3_regimen == 2 & omicron == 1 ~ 16,
            # PF 2 Delta
            vaccine == 5 & omicron == 0 ~ 5,
            # PF 2 Omicron
            vaccine == 5 & omicron == 1 ~ 10,
            # Pre-vaccination
            TRUE ~ vaccine
        ),
        vaccine_cat = factor(vaccine_cat, levels = c(0:16)),
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
        eligible = if_else(dose_3_regimen == 2 & vaccine>=6, as.integer(1), eligible),
        # don't consider any other booster
        eligible = if_else(dose_3_regimen == 0 & vaccine>=6, as.integer(0), eligible),

        # exclusions
        # don't consider unknown cohort
        eligible = if_else(cohort_final==9, as.integer(0), eligible),
        # don't consider records with an infection within 90 days
        eligible = if_else(primary_inf == 1, as.integer(0), eligible),

        # for omicron-only analysis, don't consider pre-omicron exposure
        # eligible = if_else(omicron==0, as.integer(0), eligible)
    )

# check exposure and events in each category
split_ve %>%
    filter(eligible == 1, omicron==1) %>%
    group_by(primary_inf, vaccine_cat) %>%
    summarise(
        particiants = n(),
        events = sum(event),
        exposure = sum(tstop - tstart)
    ) %>% print(n = 100)

# 7A110107 RAN10106 RBT10141 RJ710376 # something odd in the omicron only data? these have a second pcr pos date but no start_date_pos_c

save(split_ve, siren, file = "~/coviddata/siren_post_processing.RData")
