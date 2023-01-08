clear
set more off
use split_all, clear

*drop if after_primary==1 & ar>268

*************************GROUPING INTO CATEGORIES AND LABELLING***********************
*NAIVE NO PREVIOUS INFECTION
*Unvaccinated
generate x=0  if d1==-1 & Primary_inf ==0 


*Between first and second dose, Pfizer.
replace x=1   if d1==0        		 & vaccine_name1==7 & Primary_inf ==0 
replace x=2   if d1==20		         & vaccine_name1==7 & Primary_inf ==0 


*Two doses Pfizer
replace x= 3  if d2==0      		 	 & vaccine_name1==7 & vaccine_name2==6  & Primary_inf ==0 
replace x= 4  if d2==13 			     & vaccine_name1==7 & vaccine_name2==6  & Primary_inf ==0  
replace x= 5  if d2==73 			     & vaccine_name1==7 & vaccine_name2==6  & Primary_inf ==0  
replace x= 6  if d2==133 			     & vaccine_name1==7 & vaccine_name2==6  & Primary_inf ==0  
replace x= 7  if d2==193    			 & vaccine_name1==7 & vaccine_name2==6  & Primary_inf ==0 
replace x= 8  if d2==253    			 & vaccine_name1==7 & vaccine_name2==6  & Primary_inf ==0  

*Three doses Pfizer
replace x= 9  if d3==0 		   & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5 & Primary_inf ==0 
replace x= 10  if d3==6  	   & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5 & Primary_inf ==0 


*Two doses AZ
replace x= 11  if d2==0      			 & vaccine_name1==1 & vaccine_name2==1 & Primary_inf ==0 
replace x= 12  if d2==13 			     & vaccine_name1==1 & vaccine_name2==1 & Primary_inf ==0   
replace x= 13  if d2==73			     & vaccine_name1==1 & vaccine_name2==1 & Primary_inf ==0   
replace x= 14  if d2==133			     & vaccine_name1==1 & vaccine_name2==1 & Primary_inf ==0   
replace x= 15 if (d2==193 | d2==253)   	 		 & vaccine_name1==1 & vaccine_name2==1 & Primary_inf ==0 

*Three doses AZ
replace x= 16 if d3==0 			 & vaccine_name1==1 & vaccine_name2==1 & vaccine_name3==1 & Primary_inf ==0 
replace x= 17 if d3==6			 & vaccine_name1==1 & vaccine_name2==1 & vaccine_name3==1 & Primary_inf ==0 


*Three doses, primary course AZ + Pfizer Booster
replace x= 19 if d3==0 				     & vaccine_name1==1 & vaccine_name2==1 & vaccine_name3==5 & Primary_inf ==0 
replace x= 20 if d3==6       	         & vaccine_name1==1 & vaccine_name2==1 & vaccine_name3==5 & Primary_inf ==0  


*Three doses Pfizer, PI <= 1 year ago
replace x= 21  if d3==0 				& vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5 & Primary_inf == 1
replace x= 22  if d3==6  	 			& vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5 & Primary_inf == 1

*Three doses Pfizer, PI > 1 year ago
replace x= 31  if d3==0 	   			& vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5 & Primary_inf == 2
replace x= 32  if d3==6  	   			& vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5 & Primary_inf == 2


*Labelling the values of x
label define xlab 0 "Unvaccinated naive " ///
1 "d1  0-20 PF naive " 2 "d1 20+ PF naive " /// 
3 "d2  0-13 PF naive " 4 "d2 14-73 PF naive " 5 "d2 74-133 PF naive " 6 "d2 134-193 PF naive " 7 "d2 194-253 PF naive " 8 "d2 254+ PF naive "  ///
9 "d3  0-6 3PF naive " 10 "d3 7+ 3PF naive " ///
11 "d2 0-13 AZ naive " 12 "d2 14-73 AZ naive " 13 "d2 74-133 AZ naive " 14 "d2 133-193 AZ naive " 15 "d2 194+ AZ naive "  ///
16 "d3 0-6 3AZ naive " 17 "d3 7+ 3AZ naive " ///
19 "d3 0-6 2AZ/1PF naive " 20 "d3 7+ 2AZ/1PF naive " ///
21 "d3  0-6 3PF naive PI <=1yr ago" 22 "d3 7+ 3PF naive PI <=1yr ago" ///
31 "d3  0-6 3PF naive PI > 1yr ago" 32 "d3 7+ 3PF naive PI > 1yr ago" ///

replace STAFF_TYPE =12 if STAFF_TYPE ==11 & Patient_Contact ==2


*drop if x==21 //Comment this line for calculation of exposures.
label values x xlab
			

*People who move cohort have now a spurious pseudo-observation crated by the stset. This drops it.
drop if after_primary ==1 & moving ==1 & cohort_final ==1 & x==0 & Primary_inf ==0

*Exposure is used to show the information when visualising splitted observations, and generate below tables.
generate exposure=_t - _t0

gen LastPCRneg= LastPCRneg_date - start_time
order StudyId  Enrolment ar follow_up_time  firPCRposDate secPCRpos LastPCRneg vax_date1 vax_date2 vax_date3   _t0 _t time x d1  d2 d3 exposure _d _st


exit
stcox i.x i.GENDER i.ETHNGR , vce(cluster Trust_Code) nolog strata(AGEGR region WORK_EXPOSURE_FREQUENCY OCC_SET)


