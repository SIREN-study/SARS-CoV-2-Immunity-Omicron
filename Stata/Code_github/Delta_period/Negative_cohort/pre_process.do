*Ferdinando Insalata, ferdinando.insalata@phe.gov.uk
*18/11/2021

clear
set more off

*use ../SIREN_interim3_20220104.dta, clear
use ../SIREN_interim3_20220228.dta, clear

gen sd =1 if start_date_posC_fin ==.
replace sd=0 if sd==.
label define sdmlab 0 "Not missing" 1 "Missing"
label values sd sdmlab 

label define cohortlab 0 "Negative" 1 "Positive"
label values cohort_final cohortlab
drop if cohort_final ==9


*Drop positive cohort people with missing start_date (mostly or all people for which we only have first AB positive)
drop if cohort_final ==1 & start_date_posC_fin ==.


tab cohort_final sd

encode Region, gen(region)
encode Vaccine_name1, gen(vaccine_name1) 
encode Vaccine_name2, gen(vaccine_name2) 
encode Vaccine_name3, gen(vaccine_name3)
*gen DoseSchedule = 0 if Vaccine_name1 !="" & Vaccine_name2!="" & Vaccine_date2 - Vaccine_date1 <42
*replace DoseSchedule = 1 if Vaccine_name1 !="" & Vaccine_name2!="" & Vaccine_date2 - Vaccine_date1 >41



*Define December 7th as start_time (day before first jab administered)
egen start_time=min(Vaccine_date1-1)


*generate a variable, named "ar", containing the day a person becomes at risk (in this analysis it is the latest between Enrolment and Dec 7th)
gen ar=max(Date_Enrolled, start_time)

*Drop people with two infections before at risk, as in this analysis we investigate protection from one infection only . 
drop if secPCRposDate < ar



*************************GENERATE TIME AND EVENT VARIABLES*************************

*First we focus on people who are in the negative cohort (no evidence of previous infection) on December 7th.
*Among these poeple, those with a PCR-confirmed primary infection after the time they are at risk are assigned event=1.
generate event=1 if firPCRposDate !=. & firPCRposDate > ar & cohort_final==0
*And the data of this primary infection is the time-to-event for them.
gen time = firPCRposDate if event==1


*Now we focus on people who are in the positive cohort (evidence of previous infection, either PCR positive OR Covid symptoms followed by AB+ result) on December 7th.

*Among these people, those with a PCR-confirmed reinfection are assigned event=1
replace event=1 if secPCRposDate !=. & secPCRposDate >= ar & cohort_final==1
*And the date of this PCR reinfection is their time-to-event.
replace time = secPCRposDate if event==1 & cohort_final==1 & secPCRposDate >= ar

*The people whose event variable is not 1 (i.e. no primary infection or reinfection in follow up) are assigned event=0 ...
replace event=0 if event==.

*... and they are censored at the time of their latest negative PCR test.
replace time = LastPCRneg_date if event==0 

*Drop those people with no test or for which the only test is before they were at risk (ar variable).
drop if time==.
drop if time < = ar 


*************************SPLITTING NEGATIVE COHORT AT FAILURE*************************
*The aim of this is to allow negative-cohort people infected during follow up (after ar) to contribute also to the positive cohort.
*Done for people infected before and after vaccination. Compared to Interim2, we add those infected after vaccination.



*Select the people concerned and label them with the variable moving=1 (meaning that they move cohort).								
gen moving = 1 if cohort_final ==0 & firPCRposDate > ar & firPCRposDate !=.  // & firPCRposDate < Vaccine_date1 
replace moving=0 if moving==.
*pause "variable "moving" just created"
*For the people concerned, their time-to-event is now reinfection date (if they are reinfected) or latest PCR negative (after the primary infection).
*See below to understand why this replacement is needed and works well with ststplit command below.
replace time = secPCRposDate if moving ==1 & secPCRposDate !=.
replace time = LastPCRneg_date if moving==1 & secPCRposDate ==. & LastPCRneg_date >firPCRposDate & LastPCRneg_date !=.



*Declare the data as survival data. This is needed to use the stsplit command below, which allow people to move cohorts.
stset time, id(StudyId) failure(event) origin(time ar)
pause  "1"
*If people have moving=1 (i.e. can contribute to both negative and positive cohort), their observation is split into two
*on the day of their primary infection.
*the variable "after_primary" indicated which pseudo-observation, i.e. after_primary=1 for the follow-up time after primary infection.
stsplit after_primary if(moving==1), at(0) after(firPCRposDate)
pause "2"
replace after_primary=after_primary+1
*IMPORTANT: The splitting command also changed the time-to-event ("time" variable) of the first pseudo-observation (that with after_primary=0) to the argument of after().
*The argument of after() is the date of Primary infection. The first pseudo-observation ends on the date of the primary infection. 
*As a result, the "time" variable of the first pseudo-observation (referring to before primary infection, encoded by after_primary=0) is correctly the day of primary infection.
pause "3"
*The pseudo-observation after infection is labelled as belonging to the positive cohort.
***THIS CREATES MSSING START DATES, OR RATHER not consistent sd values (inherited value should be changed)
replace cohort_final=1 if after_primary==1
pause "4"
*The start date ("start_date_posC_fin") and the time at risk ("ar") of the pseudo-observation after infection becomes the day of primary infection.
replace start_date_posC_fin = firPCRposDate if after_primary ==1
replace sd=0 if  start_date_posC_fin!=. //replace missing values with day of primary infection for people just moved to positive cohort.
replace ar=firPCRposDate if after_primary==1
pause "5"
*The second pseudo observation has event=. after the splitting. It must be one if reinfection happens, or 0 otherwise (if the time variable is that of the latest negative PCR).
**triple check this
replace event =1 if event==. & time ==  secPCRposDate
****NEW, PROPERLY ANNOTATE THIS
replace event=1 if moving==1 & after_primary ==0 
replace event=0 if after_primary ==1 & time==LastPCRneg_date
pause "Before dropping _st==0"
*Drop any remaming people whose latest test is before they become at risk (_st==0)
drop if _st==0

*Drop variables created by stset
drop _t _t0 _st _d


*Set December 7th to be time zero, start of analysis time. Dates are transformed into number of days since December 7th.
*Some variables are copied because it is useful to keep them as dates for future visualisations.
gen start_date_posC_2 = start_date_posC_fin - start_time
gen follow_up_time = time - ar	
replace time = time - start_time
replace firPCRposDate=firPCRposDate - start_time +0
gen LastPCR_Neg=  LastPCRneg_date  -start_time
gen First_Inf=firPCRposDate
replace ar = ar - start_time

*The variables vd1 and vd2  are needed for the splitting below (lines 40 and 67).
gen vd1= Vaccine_date1 - start_time
gen vd2 = Vaccine_date2 - start_time
gen vd3 = Vaccine_date3 - start_time

*hence they are also copied (as days from De 7th) because the splitting will modify the values of vd1 and vd2.
gen vax_date1=Vaccine_date1- start_time
gen vax_date2=Vaccine_date2 - start_time
gen vax_date3=Vaccine_date3 - start_time

**TO ERASE IN GITHUB VERSION
*Generate a variable to store, before splitting, information on the whole time that elapses from day at risk to event/censoring.
gen whole_time=time		

pause "Before last thing to check"
*Check this works well even now, that also people infected after vaccination and enrolment change cohort
gen pinf_av=1 if cohort_final==1 & start_date_posC_fin > Vaccine_date1 & start_date_posC_fin !=. & Vaccine_date1   !=.
replace pinf_av=0 if cohort_final==1 & start_date_posC_fin !=. &( start_date_posC_fin <= Vaccine_date1 | Vaccine_date1==.)


*Save file
save before_split, replace



exit




