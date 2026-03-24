use "full_final_dataset.dta", clear

**PREPARING INDIVIDUAL-LEVEL PREDICTORS** 

*RESIDENCE: changing labels
label define residence_lbl 1 "Big city" 2 "Suburbs" 3 "Town" 4 "Village" 5 "Countryside", replace
label values residence residence_lbl
tab residence


*INCOME*
**making new continuous income variable 
gen income_cont = hh_income
* Set the missing category to system missing
replace income_cont = . if hh_income == 11
label variable income_cont "Household Income (Decile, continuous 1–10)"
tab income_cont
summ income_cont

*testing linearity for income (continuous check) (don't run each time, it takes forever) !!!!!!!!!!!!!!!!!!!!!!!
lowess imm_attitude income_cont

**center continuous variable income (calculate country mean income, create centered income)
egen income_country_mean = mean(income_cont), by(cntry)
gen income_c = income_cont - income_country_mean
label variable income_c "Income (centered within country)"



**Dropping missings
drop if missing(income_cont)
drop if missing(imm_attitude)
drop if missing(econ_impact)
tab econ_impact, missing
summarize imm_attitude econ_impact
tab imm_attitude

*AGE*
* Center continuous variable age (compute mean, then substract to centre)
summarize age
gen age_c = age - r(mean)
label variable age_c "Age (mean-centered)"

*POLITICAL ORIENTATION
tab lrscale
*centering the mean
summarize lrscale
gen lrscale_c = lrscale - r(mean)
label variable lrscale_c "Left-right self-placement (centered)"

**drop other variables	
drop anweight
drop pweight


 
*descriptive statistics table
tabstat age education income_c residence lrscale_c female, ///
    stats(n mean sd min max) columns(stat) format(%9.2f) save


 
 
 **PREPARING COUNTRY LEVEL PREDICTORS**
 
 
egen country_wave = group(cntry essround)
label variable country_wave "Country-wave identifier"
 
bysort cntry essround: assert mipex_change == mipex_change[1]
bysort cntry essround: assert fb_change == fb_change[1]	  
 
 
*centering wave-level predictors around the mean*
*MIPEX change within country
bysort cntry: egen mipex_change_c = mean(mipex_change)   // country mean
gen mipex_change_w = mipex_change - mipex_change_c          // centered deviation
 
*Immigrant share change within country
 bysort cntry: egen fb_change_c = mean(fb_change)
gen fb_change_w = fb_change - fb_change_c
 
    
*Checking that wave-level variables are constant within country-wave
bysort cntry essround: assert mipex_change_w == mipex_change_w[1]
bysort cntry essround: assert fb_change_w == fb_change_w[1]

**Centering absolute variables
summarize mipex_score
gen mipex_c = mipex_score - r(mean)
summarize mipex_c

summarize fb_share_pct
gen fb_c = fb_share_pct - r(mean)
summarize fb_c


**ANALYSIS**
	
**Null model**
mixed imm_attitude || cntry: || country_wave:
estat icc
estimates store m0

**Model 1**
mixed imm_attitude income_c lrscale_c female age_c education i.residence ///
      || cntry: || country_wave:
estimates store m1


**Model 2**
mixed imm_attitude income_c lrscale_c female age_c education i.residence ///
      mipex_change_w fb_change_w ///
      || cntry: || country_wave:
estimates store m2


**Model 3: added interaction**
gen mipex_fb_interaction = mipex_change_w * fb_change_w

mixed imm_attitude income_c lrscale_c female age_c education i.residence ///
      mipex_change_w fb_change_w mipex_fb_interaction ///
      || cntry: || country_wave:
margins, at(mipex_change_w=(-2 0 2) fb_change_w=(-2 0 2))
marginsplot

estimates store m3

**Model 4 (Adding absolute levels)
mixed imm_attitude income_c lrscale_c female age_c education i.residence ///
      mipex_change_w fb_change_w mipex_fb_interaction ///
	   mipex_c fb_c ///
      || cntry: || country_wave:
estimates store m4

*export full table
esttab m0 m1 m2 m3 m4 using "immigration_models.html", ///
replace se star(* 0.05 ** 0.01 *** 0.001) ///
label compress ///
title("Multilevel Models Predicting Immigration Attitudes")




**ROBUSTNESS: ECON IMPACT
**Null model**
mixed econ_impact || cntry: || country_wave:
estat icc
estimates store e0

**Model 1**
mixed econ_impact income_c lrscale_c female age education i.residence ///
      || cntry: || country_wave:
estimates store e1

**Model 2**
mixed econ_impact income_c lrscale_c female age education i.residence ///
      mipex_change_w fb_change_w ///
      || cntry: || country_wave:
estimates store e2

**Model 3 (added interaction)

mixed econ_impact income_c lrscale_c female age education i.residence ///
      mipex_change_w fb_change_w mipex_fb_interaction ///
      || cntry: || country_wave:
margins, at(mipex_change_w=(-2 0 2) fb_change_w=(-2 0 2))
marginsplot
estimates store e3

**Model 4 (with added absolute levels)
mixed econ_impact income_c lrscale_c female age education i.residence ///
      mipex_change_w fb_change_w mipex_fb_interaction ///
	    mipex_c fb_c ///
      || cntry: || country_wave:
estimates store e4

esttab e0 e1 e2 e3 e4 using "economic_threat_models.html", ///
replace se star(* 0.05 ** 0.01 *** 0.001) ///
label compress ///
title("Multilevel Models Predicting Economic Impact of Immigration")



***********DISREGARD************

***TABLES***


**To export as doc tables**

putdocx begin
putdocx paragraph, style(Title)
putdocx text ("Multilevel Regression Results")

*Step 1: null model
mixed imm_attitude || cntry: || country_wave:

estimates store m1

putdocx paragraph, style(Heading1)
putdocx text ("Model 1: Null model")

putdocx table m1 = etable, ///
    title("Null model") ///
    note("Random intercepts for countries and country–waves. Standard errors in parentheses.")
	
*Step 2: model 1
mixed imm_attitude i.hh_income female age education residence ///
      || cntry: || country_wave:

estimates store m2

putdocx paragraph, style(Heading1)
putdocx text ("Model 2: Individual-level controls")

putdocx table m2 = etable, ///
    title("Individual-level model") ///
    note("Includes gender, age, education, residence, and household income deciles.")
	
*Step 3: model 2
mixed imm_attitude i.hh_income female age education residence ///
      mipex_change_w fb_change_w ///
      || cntry: || country_wave:
	  
estimates store m3

putdocx paragraph, style(Heading1)
putdocx text ("Model 3: Wave-level predictors")

putdocx table m3 = etable, ///
    title("Wave-level model") ///
    note("MIPEX and immigrant share changes are centered within countries.")
	
*Step 4: interaction
mixed imm_attitude i.hh_income female age education residence ///
      mipex_change_w fb_change_w mipex_fb_interaction ///
      || cntry: || country_wave:

estimates store m4

putdocx paragraph, style(Heading1)
putdocx text ("Model 4: Interaction model")

putdocx table m4 = etable, ///
    title("Interaction model") ///
    note("Interaction between MIPEX change and immigrant share change. Centered variables.")	
	
putdocx save "ESS_multilevel_results.docx", replace	
	
	
*To export as one overall table
*Null model
mixed imm_attitude || cntry: || country_wave:
estimates store m1

* Individual-level controls
mixed imm_attitude i.hh_income female age education residence ///
      || cntry: || country_wave:
estimates store m2

* Add wave-level predictors
mixed imm_attitude i.hh_income female age education residence ///
      mipex_change_w fb_change_w ///
      || cntry: || country_wave:
estimates store m3

* Interaction model

mixed imm_attitude i.hh_income female age education residence ///
      mipex_change_w fb_change_w mipex_fb_interaction ///
      || cntry: || country_wave:
estimates store m4
	
estimates dir
	
putdocx begin
putdocx paragraph, style(Title)
putdocx text ("Multilevel Models Predicting Anti-Immigrant Attitudes")

putdocx table results = etable, estimates(m1 m2 m3 m4) ///
    title("Table 1. Multilevel regression models") ///
    note("Coefficients shown with standard errors in parentheses. " ///
         "MIPEX and immigrant share changes are centered within countries. " ///
         "Income included as categorical deciles (missing included). " ///
         "Random intercepts for countries and country–waves.")
		 

label variable imm_attitude "Anti-Immigrant Attitudes"
label variable female "Female (ref. male)"
label variable age "Age"
label variable education "Years of Education"
label variable residence "Residence (urban–rural)"
label variable hh_income "Household Income (Deciles)"
label variable mipex_change_w "MIPEX Change (within-country)"
label variable fb_change_w "Immigrant Share Change (within-country)"
label variable mipex_fb_interaction "MIPEX × Immigrant Share Change"
	