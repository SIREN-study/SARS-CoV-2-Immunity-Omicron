replace _st=1

replace _st=0 if x<30
replace _st=1 if x==11

*replace _st=0 if x==32 | x==42


stcox i.x i.GENDER i.ETHNGR i.STAFF_TYPE, vce(cluster Trust_Code) nolog strata(AGEGR region WORK_EXPOSURE_FREQUENCY) baselevel efron cformat(%9.2f)

