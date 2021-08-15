/* 
				Stochastic Dominance Analysis of Ordinal Data

Deriving Cumulative Distribution Functions (CDF) based on Jenkins(2020)	 */

/*
Notes: 

1. The World Values Survey (WVS) repeated cross-sectional data for the Philippines
was used for analysis. WVS data is publicly accessible through:  
https://www.worldvaluessurvey.org/wvs.jsp 

*/


** Install -ineqord- package **

ssc install ineqord, replace

** Generate groups (preferrably two or three) **

*Generate two income groups

gen incgrp=.
replace incgrp=1 if income>=1 & income<=5
replace incgrp=2 if income>=6 & income<=10

*Generate three education groups

gen edgrp=. 
replace edgrp=1 if edu<=2 
replace edgrp=2 if edu>=3 & edu<=6
replace edgrp=3 if edu>=7 & edu<=8 

*Generate Health Groups

gen healthgrp=. 
replace healthgrp=1 if health==1 |health==2 | health==3
replace healthgrp=2 if health==4 | health==5 


*Congress CDF
gen congcdf=. 
replace congcdf=1 if congress>=1 & congress <=2
replace congcdf=2 if congress>=3 & congress <=4  

*Executive CDF 

gen govtcdf=. 
replace govtcdf=1 if govt>=1 & govt <=2
replace govtcdf=2 if govt>=3 & govt <=4

*Police CDF
gen policecdf=. 
replace policecdf=1 if police>=1 & police <=2
replace policecdf=2 if police>=3 & police <=4

*Civil Service CDf 
gen civilservcdf=. 
replace civilservcdf=1 if civilserv>=1 & civilserv <=2
replace civilservcdf=2 if civilserv>=3 & civilserv <=4

*Judiciary
 
gen judicdf=. 
replace judicdf=1 if judi>=1 & judi <=2
replace judicdf=2 if judi>=3 & judi <=4

**For this part, see Jenkins' (2020) discussions**

tab ls
gen sat= ls+1
label variable sat "= ls + 1" 


**CDFs by sex 

ineqord sat
tab sat sex, column nofreq

ineqord sat if sex == 1 & sat != 1, alpha(.9) ///
catv(v_male) catpr(f_male) catcpr(F_male) catspr(S_male) ///
gldvar(gld_male) gluvar(glu_male) hplus(hp_male) hminus(hm_male)

ineqord sat if sex == 2 & sat != 1, alpha(.9) ///
catv(v_fem) catpr(f_fem) catcpr(F_fem) catspr(S_fem) ///
gldvar(gld_fem) gluvar(glu_fem) hplus(hp_fem) hminus(hm_fem)

tw (line F_male v_male, sort c(stairstep) lcolor(black) ) ///
(line F_fem v_fem, sort c(stairstep) lcolor(black) lpatt(shortdash) ) ///
, xlab(1(1)11) yline(0.5, lpatt(shortdash) lcol(black)) ///
ylab(0(.1)1, angle(0)) ytitle("{it:p}") xtitle("Response (rescaled)") ///
legend(label(1 "Male") label(2 "Female") col(1) ///
ring(0) position(11) ) ///
scheme(s1color) graphregion(color(white)) ///
name(sex) 


*CDFs by incgroup

ineqord sat
tab sat incgrp, column nofreq

ineqord sat if incgrp == 1 & sat != 1, alpha(.9) ///
catv(v_bel) catpr(f_bel) catcpr(F_bel) catspr(S_bel) ///
gldvar(gld_bel) gluvar(glu_bel) hplus(hp_bel) hminus(hm_bel)

ineqord sat if incgrp == 2 & sat !=1, alpha(.9) ///
catv(v_abv) catpr(f_abv) catcpr(F_abv) catspr(S_abv) ///
gldvar(gld_abv) gluvar(glu_abv) hplus(hp_abv) hminus(hm_abv)

tw (line F_bel v_bel, sort c(stairstep) lcolor(black) ) ///
(line F_abv v_abv, sort c(stairstep) lcolor(black) lpatt(shortdash) ) ///
, xlab(1(1)11) yline(0.5, lpatt(shortdash) lcol(black)) ///
ylab(0(.1)1, angle(0)) ytitle("{it:p}") xtitle("Response (rescaled)") ///
legend(label(1 "Income<=5") label(2 "Income >=6 & Income <=10") col(1) ///
ring(0) position(11) ) ///
scheme(s1color) graphregion(color(white)) ///
name(income) 



*CDFs by education group 


ineqord sat if edgrp == 1 & sat != 1, alpha(.9) ///
catv(v_elem) catpr(f_elem) catcpr(F_elem) catspr(S_elem) ///
gldvar(gld_elem) gluvar(glu_elem) hplus(hp_elem) hminus(hm_elem)

ineqord sat if edgrp == 2 & sat !=1, alpha(.9) ///
catv(v_sec) catpr(f_sec) catcpr(F_sec) catspr(S_sec) ///
gldvar(gld_sec) gluvar(glu_sec) hplus(hp_sec) hminus(hm_sec)

ineqord sat if edgrp == 3 & sat !=1, alpha(.9) ///
catv(v_uni) catpr(f_uni) catcpr(F_uni) catspr(S_uni) ///
gldvar(gld_uni) gluvar(glu_uni) hplus(hp_uni) hminus(hm_uni)


tw (line F_elem v_elem, sort c(stairstep) lcolor(black) ) ///
(line F_sec v_sec, sort c(stairstep) lcolor(black) lpatt(shortdash) ) ///
(line F_uni v_uni, sort c(stairstep) lcolor(black) lpatt(longdash) ) ///
, xlab(1(1)11) yline(0.5, lpatt(shortdash) lcol(black)) ///
ylab(0(.1)1, angle(0)) ytitle("{it:p}") xtitle("Response (rescaled)") ///
legend(label(1 "Elementary Level-Un/completed") label(2 "Secondary Level-Un/completed") ///
label(3 "University Level-Un/completed") col(1) ///
ring(0) position(11) ) ///
scheme(s1color) graphregion(color(white)) ///
name(educ) 


*CDFs by employment status Employed vs Unemployed 


ineqord sat if employ==1,  alpha(.9) ///
catv(v_full) catpr(f_full) catcpr(F_full) catspr(S_full) ///
gldvar(gld_full) gluvar(glu_full) hplus(hp_full) hminus(hm_full)

ineqord sat if employ== 7 & sat != 1, alpha(.9) ///
catv(v_unem) catpr(f_unem) catcpr(F_unem) catspr(S_unem) ///
gldvar(gld_unem) gluvar(glu_unem) hplus(hp_unem) hminus(hm_unem)

tw (line F_full v_full, sort c(stairstep) lcolor(black) ) ///
(line F_unem v_unem, sort c(stairstep) lcolor(black) lpatt(shortdash) ) ///
, xlab(1(1)11) yline(0.5, lpatt(shortdash) lcol(black)) ///
ylab(0(.1)1, angle(0)) ytitle("{it:p}") xtitle("Response (rescaled)") ///
legend(label(1 "Full time") label(2 "Unemployed") col(1) ///
ring(0) position(11) ) ///
scheme(s1color) graphregion(color(white)) ///
name(employ) 


*CDFs by Health Status 

ineqord sat if healthgrp == 1 & sat != 1,  alpha(.9) ///
catv(v_poor) catpr(f_poor) catcpr(F_poor) catspr(S_poor) ///
gldvar(gld_poor) gluvar(glu_poor) hplus(hp_poor) hminus(hm_poor)

ineqord sat if healthgrp== 2 & sat != 1, alpha(.9) ///
catv(v_good) catpr(f_good) catcpr(F_good) catspr(S_good) ///
gldvar(gld_good) gluvar(glu_good) hplus(hp_good) hminus(hm_good)

tw (line F_poor v_poor, sort c(stairstep) lcolor(black) ) ///
(line F_good v_good, sort c(stairstep) lcolor(black) lpatt(shortdash) ) ///
, xlab(1(1)11) yline(0.5, lpatt(shortdash) lcol(black)) ///
ylab(0(.1)1, angle(0)) ytitle("{it:p}") xtitle("Response (rescaled)") ///
legend(label(1 "Very Poor to Fair") label(2 "Good to Very Good") col(1) ///
ring(0) position(11) ) ///
scheme(s1color) graphregion(color(white)) ///
name(health) 


*CDFs of main covariates 

*Congress


ineqord sat if congcdf  == 1 & sat !=1, alpha(.9) ///
catv(v_not) catpr(f_not) catcpr(F_not) catspr(S_not) ///
gldvar(gld_not) gluvar(glu_not) hplus(hp_not) hminus(hm_not)

ineqord sat if congcdf == 2 & sat !=1, alpha(.9) ///
catv(v_notvm) catpr(f_notvm) catcpr(F_notvm) catspr(S_notvm) ///
gldvar(gld_notvm) gluvar(glu_notvm) hplus(hp_notvm) hminus(hm_notvm) 


tw (line F_not v_not, sort c(stairstep) lcolor(black) ) ///
(line F_notvm v_notvm, sort c(stairstep) lcolor(black) lpatt(shortdash) ) ///
, xlab(1(1)11) yline(0.5, lpatt(shortdash) lcol(black)) ///
ylab(0(.1)1, angle(0)) ytitle("{it:p}") xtitle("Response (rescaled)") ///
legend(label(1 "None at all & Not very much") label(2 "Quite a lot & A great deal") col(1) ///
ring(0) position(11) ) ///
scheme(s1color) graphregion(color(white)) ///
name(cong) 

drop v_not gld_not glu_not hp_not hm_not f_not F_not S_not v_notvm gld_notvm glu_notvm hp_notvm hm_notvm f_notvm F_notvm S_notvm 


*Executive 


ineqord sat if govtcdf  == 1 & sat !=1, alpha(.9) ///
catv(v_not) catpr(f_not) catcpr(F_not) catspr(S_not) ///
gldvar(gld_not) gluvar(glu_not) hplus(hp_not) hminus(hm_not)

ineqord sat if govtcdf == 2 & sat !=1, alpha(.9) ///
catv(v_notvm) catpr(f_notvm) catcpr(F_notvm) catspr(S_notvm) ///
gldvar(gld_notvm) gluvar(glu_notvm) hplus(hp_notvm) hminus(hm_notvm) 


tw (line F_not v_not, sort c(stairstep) lcolor(black) ) ///
(line F_notvm v_notvm, sort c(stairstep) lcolor(black) lpatt(shortdash) ) ///
, xlab(1(1)11) yline(0.5, lpatt(shortdash) lcol(black)) ///
ylab(0(.1)1, angle(0)) ytitle("{it:p}") xtitle("Response (rescaled)") ///
legend(label(1 "None at all & Not very much") label(2 "Quite a lot & A great deal") col(1) ///
ring(0) position(11) ) ///
scheme(s1color) graphregion(color(white)) ///
name(executive) 

drop v_not gld_not glu_not hp_not hm_not f_not F_not S_not v_notvm gld_notvm glu_notvm hp_notvm hm_notvm f_notvm F_notvm S_notvm



*Police 

ineqord sat if policecdf  == 1 & sat !=1, alpha(.9) ///
catv(v_not) catpr(f_not) catcpr(F_not) catspr(S_not) ///
gldvar(gld_not) gluvar(glu_not) hplus(hp_not) hminus(hm_not)

ineqord sat if policecdf == 2 & sat !=1, alpha(.9) ///
catv(v_notvm) catpr(f_notvm) catcpr(F_notvm) catspr(S_notvm) ///
gldvar(gld_notvm) gluvar(glu_notvm) hplus(hp_notvm) hminus(hm_notvm) 


tw (line F_not v_not, sort c(stairstep) lcolor(black) ) ///
(line F_notvm v_notvm, sort c(stairstep) lcolor(black) lpatt(shortdash) ) ///
, xlab(1(1)11) yline(0.5, lpatt(shortdash) lcol(black)) ///
ylab(0(.1)1, angle(0)) ytitle("{it:p}") xtitle("Response (rescaled)") ///
legend(label(1 "No trust at all & Not very much") label(2 "Quite a lot & A great deal") col(1) ///
ring(0) position(11) ) ///
scheme(s1color) graphregion(color(white)) ///
name(police) 

drop v_not gld_not glu_not hp_not hm_not f_not F_not S_not v_notvm gld_notvm glu_notvm hp_notvm hm_notvm f_notvm F_notvm S_notvm


*Civil Service 


ineqord sat if civilservcdf == 1 & sat !=1, alpha(.9) ///
catv(v_not) catpr(f_not) catcpr(F_not) catspr(S_not) ///
gldvar(gld_not) gluvar(glu_not) hplus(hp_not) hminus(hm_not)

ineqord sat if civilservcdf == 2 & sat !=1, alpha(.9) ///
catv(v_notvm) catpr(f_notvm) catcpr(F_notvm) catspr(S_notvm) ///
gldvar(gld_notvm) gluvar(glu_notvm) hplus(hp_notvm) hminus(hm_notvm) 


tw (line F_not v_not, sort c(stairstep) lcolor(black) ) ///
(line F_notvm v_notvm, sort c(stairstep) lcolor(black) lpatt(shortdash) ) ///
, xlab(1(1)11) yline(0.5, lpatt(shortdash) lcol(black)) ///
ylab(0(.1)1, angle(0)) ytitle("{it:p}") xtitle("Response (rescaled)") ///
legend(label(1 "No trust at all & Not very much") label(2 "Quite a lot & A great deal") col(1) ///
ring(0) position(11) ) ///
scheme(s1color) graphregion(color(white)) ///
name(civil) 

drop v_not gld_not glu_not hp_not hm_not f_not F_not S_not v_notvm gld_notvm glu_notvm hp_notvm hm_notvm f_notvm F_notvm S_notvm



*Judiciary 

ineqord sat if judicdf == 1 & sat !=1, alpha(.9) ///
catv(v_not) catpr(f_not) catcpr(F_not) catspr(S_not) ///
gldvar(gld_not) gluvar(glu_not) hplus(hp_not) hminus(hm_not)

ineqord sat if judicdf == 2 & sat !=1, alpha(.9) ///
catv(v_notvm) catpr(f_notvm) catcpr(F_notvm) catspr(S_notvm) ///
gldvar(gld_notvm) gluvar(glu_notvm) hplus(hp_notvm) hminus(hm_notvm) 


tw (line F_not v_not, sort c(stairstep) lcolor(black) ) ///
(line F_notvm v_notvm, sort c(stairstep) lcolor(black) lpatt(shortdash) ) ///
, xlab(1(1)11) yline(0.5, lpatt(shortdash) lcol(black)) ///
ylab(0(.1)1, angle(0)) ytitle("{it:p}") xtitle("Response (rescaled)") ///
legend(label(1 "No trust at all & Not very much") label(2 "Quite a lot & A great deal") col(1) ///
ring(0) position(11) ) ///
scheme(s1color) graphregion(color(white)) ///
name(judi) 

drop v_not gld_not glu_not hp_not hm_not f_not F_not S_not v_notvm gld_notvm glu_notvm hp_notvm hm_notvm f_notvm F_notvm S_notvm



/*References

Jenkins, S.P., 2020. Comparing distributions of ordinal data. Stata J. 20, 505–531. https://doi.org/10.1177/1536867X20953565
