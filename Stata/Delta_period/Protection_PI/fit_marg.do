replace _st=1

replace _st=0 if x<6 | x>10 

replace _st=1 if x==15 | x==19 |  x==20 
stcox i.Primary_inf i.x i.GENDER i.ETHNGR i.STAFF_TYPE, vce(cluster Trust_Code) nolog strata(AGEGR region WORK_EXPOSURE_FREQUENCY) efron cformat(%9.2f) baselevel



