cd "C:\Users\gwill\Dropbox\Research\Dissertation\Data Analysis - Territory Onset\data\"

**** Format ICOW data
use ICOWprovyr101, clear
sort dyad year
by dyad: egen minclaimyear = min(year)
collapse (max) icowsal sal* tc* minclaimyear, by(dyad year)
gen terriss = 1
save icow_modified.dta, replace

********************************************************************************
* Merge data
******************************************************************************** 
use terrdatav2.dta, clear

* Merge ICOW
merge 1:1 dyad year using icow_modified
	* not in master - no icow claim - keep
	* not in using - ???
drop _merge
replace terriss = 0 if terriss == .

* Merge rivalry
merge 1:1 dyad year using kgd_rivalry_dummy
drop _merge
drop start end

********************************************************************************
**** Create duration variables
********************************************************************************

xtset dyad year
tsfill
sort dyad year

* Create claim onset (fail) var
by dyad: gen fail = terriss == 1 & l.terriss != 1
order dyad year terriss fail


* Create duration variable
tsspell fail
order _*, before(fail)
rename _seq start
*by dyad: gen restart = terriss == 0 & l.fail == 1 & terriss != 1
by dyad: replace start = l.start + 1 if fail == 1
gen end = start + 1
order start end, before(fail)
drop if terriss == 1 & fail == 0
drop _*
 
 
* Create failure vars for particular claim types
*gen fail_strat = fail * tcstratloc
*gen fail_resource = fail * tcresource
*gen fail_offshore = fail * tcoffshore
*gen fail_homechal = fail * tchomechal
*gen fail_hometgt = fail * tchometgt
*gen fail_pop = fail * tcpop
*gen fail_histchal = fail * tchistchal
*gen fail_histtgt = fail * tchisttgt
*gen fail_histany = fail_histchal == 1 | fail_histtgt == 1

*replace fail_resource = 0 if fail_resource == .
*replace fail_offshore = 0 if fail_offshore == .
*replace fail_homechal = 0 if fail_homechal == .
*replace fail_hometgt = 0 if fail_hometgt == .
*replace fail_pop = 0 if fail_pop == .
*replace fail_histchal = 0 if fail_histchal == .
*replace fail_histtgt = 0 if fail_histtgt == .
*replace fail_histany = 0 if fail_histany == .

**** New vars
gen leadtransany = leadertrans1 == 1 | leadertrans2 == 1
gen solschany = solschdum1 == 1 | solschdum2 == 1
**** Replace missing
replace terriss = 0 if terriss == .
*replace kgd_rivalry_year = 0 if kgd_rivalry_year == .
*replace leadtransany = . if year < 1919
*replace solschany = 0 if solschany == . & year >= 1918

**** New vars
gen dcaprat = d.caprat
gen ldcaprat = l.dcaprat
gen lautdy = l.autdy
gen lsamereg = l.samereg


*gen lautdy = lagpolity21 < -5 & lagpolity22 < -5
*gen lsamereg = lagdemdy == 1 | lautdy == 1
*egen maxdep = rowmax(depend*)
*egen mindep = rowmin(depend*)
gen lrival = l.kgd_rivalry_year
gen lcwany = lcw1 == 1 | lcw2 == 1
gen lpchcap = l.pchcaprat

**** Generate major power variables
gen majpower = (ccode1 == 2 | ccode2 == 2) & year >= 1898
replace majpower = 1 if (ccode1 == 200 | ccode2 == 200) & year >= 1816
replace majpower = 1 if (ccode1 == 220 | ccode2 == 220) & ((year >= 1816 & year <= 1940) | year >= 1945) 
replace majpower = 1 if (ccode1 == 255 | ccode2 == 255) & year >= 1816 & year <= 1918 
replace majpower = 1 if (ccode1 == 255 | ccode2 == 255) & year >= 1925 & year <= 1945
replace majpower = 1 if (ccode1 == 255 | ccode2 == 255) & year >= 1991
replace majpower = 1 if (ccode1 == 300 | ccode2 == 300) & year >= 1816 & year <= 1918
replace majpower = 1 if (ccode1 == 325 | ccode2 == 325) & year >= 1860 & year <= 1943
replace majpower = 1 if (ccode1 == 365 | ccode2 == 365) & year >= 1816 & year <= 1917
replace majpower = 1 if (ccode1 == 365 | ccode2 == 365) & year >= 1922
replace majpower = 1 if (ccode1 == 710 | ccode2 == 710) & year >= 1950
replace majpower = 1 if (ccode1 == 740 | ccode2 == 740) & year >= 1895 & year <= 1945
replace majpower = 1 if (ccode1 == 740 | ccode2 == 740) & year >= 1991

replace lrival = 0 if lrival == .
replace lcwany = 0 if lcwany == .
replace TEK = 0 if TEK == . & year > 1945
replace lagterrch = 0 if lagterrch == 0
**** Generate min dyad year
sort dyad year
by dyad: egen mindyadyear = min(year)


***** Descriptives claims that started after dyad start
* filter in viewer
*(minclaimyear != mindyadyear) & minclaimyear == year
gen yeardiff = minclaimyear - mindyadyear if minclaimyear != . & mindyadyear != .
su yeardiff

**** Cleanup
drop tc* sal* 
rename end stop
*rename lagdemdy ldemdy
order dyad year start stop fail
drop pop1 stateabb1 milex1 milper1 irst1 pec1 tpop1 upop1 cinc1
drop pop2 stateabb2 milex2 milper2 irst2 pec2 tpop2 upop2 cinc2
save eu3.dta, replace
