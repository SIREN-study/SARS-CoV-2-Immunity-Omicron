replace _st=1

replace _st=0 if   x>20 |  x<11
replace _st=0 if x==17 | x==.

drop if Primary_inf!=0

stcox i.x  i.GENDER i.ETHNGR i.STAFF_TYPE, vce(cluster Trust_Code) nolog strata(AGEGR region WORK_EXPOSURE_FREQUENCY) efron allbaselevels  cformat(%9.2f) 


