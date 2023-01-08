clear
set more off

use before_split, clear

*keep if Vaccine_name3 == "Moderna" | Vaccine_name3 == ""

*************************SPLITTING TIME BETWEEN JABS 1 and 2********************
*In order to split the time between jabs, we create a fake failure variable (fail) always equal to 1. The time of failure is the day of the second jab or,
*if the participant did not receive a second jab, it will coincide with the "time" variable (either infection or censoring time).
*This approach has "side effects": it creates spurious pseudo observations and the variable event is assigned missing (.) value when it should be zero.
*These are taken care of at the end.
	
keep if cohort_final <2 												
*The below line creates a fake failure variable, to split time between jabs. Time of failure will be the date of second jab.
gen fail=1

*The below line adapts the strategy to people who have not received second jab or leave the cohort before first dose.
replace vd2=time if (vd2==. | time < vd1)

*For STATA to accept the data as survival data, the pseudo-observation after primary infection of people who move chort (see pre_process.do) must have a different StudyId.
*Without the following line, Stata gives an error because of a clash between start time and failure time of pseudo-observations with same StudyId.
gen original_id  =  StudyId
replace StudyId =StudyId+"pos" if after_primary ==1



*stset and split, to split time between jabs.
stset vd2, id(StudyId)  fail(fail) origin(time 0) 
stsplit d1, at(0 20) after(vd1)

**pause "Before cleaning up side effects"
*Clean side effects of the previous stset and stsplit:

*The below line deletes pseudo-observations which start beyond second dose. 
*We do not consider the time since first dose when a participant has received thei second dose.
drop if _t0>=time

*The below line deals with people without first jab
replace d1=-1 if Vaccine_date1 ==. & d1!=-1


**pause
*Clean the rest
sort StudyId
by StudyId: generate n1 = _n
by StudyId: generate N = _N
replace time=_t if n1!=N
replace event=0 if n1!=N
gen Enrolment= Date_Enrolled - start_time
drop _t0 N n1 _t _st 

*************************SPLITTING TIME BETWEEN JABS 2 and 3********************
					
*The below line adapts the strategy to people who have not received a third jab or leave the cohort before second dose.
replace vd3=time if (vd3==.  | time < vax_date2 )

*stset and split, to split time between jabs.
stset vd3, id(StudyId)  fail(fail) origin(time 0) //if(Vaccine_date2!=.)


*pause "After problematic setting"

stsplit d2, at(0 13 73 133 193 253) after(vax_date2) 
*stsplit d2, at(0 13 193 253) after(vax_date2)



**pause "right after splitting"
*Set d1=. for the pseudo-observations after second dose.
replace d1=. if d2>=0 & d2!=. & Vaccine_date2!=.





*Clean side effects of the previous stset and stsplit:
*The below line deletes pseudo-observations which start beyond second dose. 
*We do not consider the time since first dose when a participant has received their second dose.
drop if _t0>=time &  Vaccine_date2!=.
*The second condition ( Vaccine_date2!=.) prevents people withou second dose (who currently have _t0=.) from being dropped.

*The below line deals with people without first jab
replace d2=-1 if Vaccine_date2 ==.


*pause "Before second cleanup"
*Clean the rest
sort StudyId d1 d2
by StudyId: generate n1 = _n
by StudyId: generate N = _N
replace time=_t if n1!=N
replace event=0 if n1!=N




drop fail N n1 _t0 _t _st 
*************************SPLITTING TIME AFTER THIRD JAB*************************


stset time, id(StudyId) failure(event) origin(time 0)
*generate exposure=_t - _t0

stsplit d3, at(0 6) after(vd3) 
*stsplit d3, at(0 6) after(vd3) 


replace d2=. if d3>=0 & d3!=.
*replace d1=. if d3>=0

pause
sort StudyId d1 d2 d3

*pause "After last splitting"

*************************TIME AFTER PRIMARY INFECTION (for positive cohort only)*************************
	
stsplit Primary_inf if(cohort_final==1), at(0 90 181 274 365 456) after(start_date_posC_2)
*The above stsplit (line ??) creates spurious pseudo-observations, starting and ending on the same day. The next line drops them:
drop if after_primary ==1 & moving ==1 & cohort_final ==1 & d1==-1 & ar==time & Primary_inf ==-1


*Next line drops the first 90 days after a primary infection, since by definition of reinfection (two positive PCR at least 90 days apart)
*the participant is not at risk during that time.
drop if Primary_inf==0
recode Primary_inf -1=0 90=1 181=2 274=3 365=4 456=5  


*Next line labels the pseudo observation (line ??) and observations (line ??) where there is no previous primary infection with value Primary_inf=0.
*"Primary_inf" is currently -1 for the pseudo-observation before Primary infection. The value 0 is more logical.
*Also other values of Primary_inf are affected, but they are recoded below (line ??).

label define prim_inf_lab 0 "No previous inf" 1 "3-6 mth ago" 2 "6-9 mth ago" 3 "9-12 mth ago" 4 "12-15 mth ago" 5 "15+ mth ago"
label values Primary_inf prim_inf_lab
*Drop spurious pseudo-obvservations
drop if Primary_inf ==0 & after_primary ==1
drop if Primary_inf ==0 & cohort_final==1

*All pseudo-observations of people in the negative cohort have Primary_inf=".", as the splitting affected only people in the positive cohort.
*We replace this missing value with the value 0, so these participant contribute follow-up time before/without primary infection.
replace Primary_inf=0 if Primary_inf==.



*************************SPLIT IN BEFORE AND AFTER ENROLMENT*************************
*This is needed because some people enrolled after Dec 7th, and we need to tell Stata to discard the time between enrolment and Dec 7th.

stsplit is_enrolled, at(0) after(Enrolment)

*Label as is_enrolled=0 the pseudo-observations before enrolment, with 1 those after enrolment (currently -1 and 0).
replace is_enrolled=is_enrolled+1

*Before this line, _st==1. always. Next line tell stata not to use time before enrolment.
replace _st=is_enrolled

drop if _st==0
*The above line does not affect the HR estimates, only affects the calculation of exposure and rates per category.
*Without dropping _st==0, exposure that should not be considered is counted, and crude rates of infection are affected too.


*drop if Vaccine_name1=="AstraZeneca"  | Vaccine_name2=="AstraZeneca"

stsplit variant, at(0) after(time=359) //Check precise number

********
*stsplit variant, at(0 25) after(time=345) //Check precise number
*drop if variant==0
*replace variant=0 if variant>0
********


*Drop Omicron follow-up time
drop if variant==0

save split_all, replace

exit











