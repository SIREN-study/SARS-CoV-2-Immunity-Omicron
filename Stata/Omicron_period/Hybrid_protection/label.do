clear
set more off
use split_all, clear

*drop if after_primary==1 & ar>268

*************************GROUPING INTO CATEGORIES AND LABELLING***********************
*NAIVE NO PREVIOUS INFECTION
*Unvaccinated
generate x=0 if d1==-1 & Primary_inf ==0


*Between first and second dose, Pfizer.
replace x=1 if d1==0        		 & Primary_inf==0 & vaccine_name1==7
replace x=2 if d1==20		         & Primary_inf==0 & vaccine_name1==7


*Two doses Pfizer
replace x= 3 if d2==0      			& Primary_inf==0 & vaccine_name1==7 & vaccine_name2==6 
replace x= 4 if d2==13 			    & Primary_inf==0 & vaccine_name1==7 & vaccine_name2==6   

*Two doses >6mo, 
replace x= 10 if d2==193		    & Primary_inf==0 & variant==1 & vaccine_name1==7 & vaccine_name2==6  
replace x= 11 if d2==253    		& Primary_inf==0 & variant==1 & vaccine_name1==7 & vaccine_name2==6  

*Three doses Pfizer, 
replace x= 12 if d3==0     			& variant==1 & Primary_inf==0 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5
replace x= 13 if d3==6     			& variant==1 & Primary_inf==0 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5     
replace x= 14 if d3==66    			& variant==1 & Primary_inf==0 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5   
replace x= 15 if d3==126     		& variant==1 & Primary_inf==0 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5   

replace x= 20 if (d2==193 | d2==253) & variant==1 & Primary_inf==0 & vaccine_name1==1 & vaccine_name2==1 
replace x= 21 if d3==0     			 & variant==1 & Primary_inf==0 & vaccine_name1==1 & vaccine_name2==1 & vaccine_name3==5
replace x= 22 if d3==6     			 & variant==1 & Primary_inf==0 & vaccine_name1==1 & vaccine_name2==1 & vaccine_name3==5     
replace x= 23 if (d3==66 | d3==126)  & variant==1 & Primary_inf==0 & vaccine_name1==1 & vaccine_name2==1 & vaccine_name3==5   


*****PRIMARY INFECTION <1yr******
*Two doses Pfizer >6mo, 
replace x= 30 if d2==193      		 & Primary_inf>0 & Primary_inf<3 & variant==1 & vaccine_name1==7 & vaccine_name2==6 
replace x= 31 if d2==253      		 & Primary_inf>0 & Primary_inf<3 & variant==1 & vaccine_name1==7 & vaccine_name2==6 

*Three doses Pfizer 
replace x= 32 if d3==0     			 & variant==1 & Primary_inf>0 & Primary_inf<3 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5
replace x= 33 if d3==6     			 & variant==1 & Primary_inf>0 & Primary_inf<3 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5    
replace x= 34 if d3==66     		 & variant==1 & Primary_inf>0 & Primary_inf<3 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5     
replace x= 35 if d3==126     		 & variant==1 & Primary_inf>0 & Primary_inf<3 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5     

*replace x= 36 if d2==13 			 & Primary_inf>0 & Primary_inf<3 & vaccine_name1==7 & vaccine_name2==6   

*Two doses Pfizer >6mo, 
replace x= 40 if d2==193      		 & Primary_inf==3 & variant==1 & vaccine_name1==7 & vaccine_name2==6 
replace x= 41 if d2==253      		 & Primary_inf==3 & variant==1 & vaccine_name1==7 & vaccine_name2==6 

*Three doses Pfizer 
replace x= 42 if d3==0     			 & variant==1 & Primary_inf==3 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5
replace x= 43 if d3==6     			 & variant==1 & Primary_inf==3 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5    
replace x= 44 if d3==66     		 & variant==1 & Primary_inf==3 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5     
replace x= 45 if d3==126     		 & variant==1 & Primary_inf==3 & vaccine_name1==7 & vaccine_name2==6 & vaccine_name3==5     
*replace x= 46 if d2==13 			 & Primary_inf==3 & vaccine_name1==7 & vaccine_name2==6   


*Labelling the values of x
label define xlab 0 "Unvaccinated naive  " ///
1 "d1 0-20 PF naive" 2 "d1 20+ PF naive" /// 
3 "d2 0-13 PF naive" 4 "d2 14-193 PF naive" ///
10 "d2 194-253 PF naive "  11 "d2 254+ PF naive " ///
12 "d3 0-6 3PF naive " 13 "d3 7-66 3PF naive " 14 "d3 67-126 3PF naive " 15 "d3 127+ 3PF naive " ///
16 "d3 0-6 2AZ/1PF naive " 17 "d3 7-66 2AZ/1PF naive " 18 "d3 67-126 2AZ/1PF naive " 19 "d3 127+ 2AZ/1PF naive " /// 
20 "d2 194+ AZ naive" 21 "d3 0-6 2AZ/1PF naive" ///
22 "d3 7-66 2AZ/1PF naive" 23 "d3 67+ 2AZ/1PF naive" ///
30 "d2 194-253 PF PI <=1yr "  31 "d2 254+ PF PI <=1yr " ///
32 "d3 0-6 3PF PI <=1yr " 33 "d3 7-66 3PF PI <=1yr " 34 "d3 67-126 3PF PI <=1yr " 35 "d3 127+ 3PF PI <=1yr " ///
40 "d2 194-253 PF PI >1yr "  41 "d2 254+ PF PI >1yr " ///
42 "d3 0-6 3PF PI >1yr " 43 "d3 7-66 3PF PI >1yr " 44 "d3 67-126 3PF PI >1yr " 45 "d3 127+ 3PF PI >1yr " ///


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

