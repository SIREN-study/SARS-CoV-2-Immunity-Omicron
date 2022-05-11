est use "C:\Users\peter.kirwan\Documents\Repos\SARS-CoV-2-Immunity_v2\Stata\negc.ster"
stcox

est use "C:\Users\peter.kirwan\Documents\Repos\SARS-CoV-2-Immunity_v2\Stata\posc.ster"
stcox

est use "C:\Users\peter.kirwan\Documents\Repos\immunity-v2\Stata\Alpha_Delta_PI.ster"
stcox

est use "C:\Users\peter.kirwan\Documents\Repos\immunity-v2\Stata\Omicron_PI.ster"
stcox

// exposure event rates
use "C:\Users\peter.kirwan\Documents\Repos\SARS-CoV-2-Immunity_v2\data\SIREN_interim3_20220104.dta"

count if cohort_final==0 & FirstPCRpos_date!=. & FirstPCRpos_date > Date_Enrolled & ///
 Date_Enrolled>=date("2020-12-07","YMD") & FirstPCRpos_date<Vaccine_date1
