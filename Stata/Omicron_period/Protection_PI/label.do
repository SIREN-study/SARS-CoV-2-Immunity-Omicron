clear
set more off
use split_all, clear

*drop if after_primary==1 & ar>268
*************************GROUPING INTO CATEGORIES AND LABELLING***********************
*NAIVE NO PREVIOUS INFECTION

*Two doses PF >8mo
gen x= 11 if  (d2==253) & variant==1 & vaccine_name1==7 & vaccine_name2==6  

*Three doses Pfizer
replace x= 12 if d3==0     			& variant==1  & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5
replace x= 13 if d3==6     			& variant==1  & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5     
replace x= 14 if d3==66    			& variant==1  & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5   
replace x= 15 if d3==126     		& variant==1  & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5   

*Two doses AZ >6mo
replace x= 16 if (d2==193 | d2==253)  & variant==1  & vaccine_name1==1 & vaccine_name2==1 

*Two AZ + 1PF
replace x= 17 if d3==0     			& variant==1  & vaccine_name1==1 & vaccine_name2==1 & vaccine_name3==5
replace x= 18 if d3==6     			& variant==1  & vaccine_name1==1 & vaccine_name2==1 & vaccine_name3==5     
replace x= 19 if (d3==66 | d3==126) & variant==1  & vaccine_name1==1 & vaccine_name2==1 & vaccine_name3==5   
 



*Labelling the values of x
label define xlab  11 "d2 254+ PF  Omicron" ///
12 "d3 0-6 3PF  Omicron" 13 "d3 7-66 3PF  Omicron" 14 "d3 67-126 3PF  Omicron" 15 "d3 127+ 3PF  Omicron" ///
16 "d2 194+ AZ  Omicron" ///
17 "d3 0-6 2AZ/1PF  Omicron" 18 "d3 7-66 2AZ/1PF  Omicron" 19 "d3 67+ 2AZ/1PF  Omicron"


*Drop "d2 0-13 AZ", as there are not enough people for that parameter estimate to converge. This does not affect results.
*drop if x==21 //Comment this line for calculation of exposures.
label values x xlab
			
replace STAFF_TYPE =12 if STAFF_TYPE ==11 & Patient_Contact ==2


*People who move cohort have now a spurious pseudo-observation crated by the stset. This drops it.
drop if after_primary ==1 & moving ==1 & cohort_final ==1 & x==0 & Primary_inf ==0

*Exposure is used to show the information when visualising splitted observations, and generate below tables.
generate exposure=_t - _t0

gen LastPCRneg= LastPCRneg_date - start_time
order StudyId  Enrolment ar follow_up_time  firPCRposDate secPCRpos LastPCRneg vax_date1 vax_date2 vax_date3   _t0 _t time x d1  d2 d3 exposure _d _st


exit
stcox i.x i.GENDER i.ETHNGR , vce(cluster Trust_Code) nolog strata(AGEGR region WORK_EXPOSURE_FREQUENCY OCC_SET)


