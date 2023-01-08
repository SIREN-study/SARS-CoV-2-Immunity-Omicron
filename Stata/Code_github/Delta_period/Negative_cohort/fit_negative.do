replace _st=0

replace _st=1 if x==8 | x ==9 | x ==10 | x ==15 | x ==19 | x ==20

drop if Primary_inf!=0




stcox i.x i.GENDER i.ETHNGR i.STAFF_TYPE, vce(cluster Trust_Code) nolog strata(AGEGR region WORK_EXPOSURE_FREQUENCY ) cformat(%9.2f) allbaselevels efron



