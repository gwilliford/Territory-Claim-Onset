cd "C:\Users\gwill\Dropbox\Research\Dissertation\Data Management"
use dyaddata, clear

**** Stset for MIDs
stset year, fail(bmidonset) id(dyad)
* stcox onemp twomp lnccdist
rename (_d _t _t0) (midfail midstop midstart)
save midduration.dta, replace

**** Stset for barbieri
import delimited "C:\Users\gwill\Dropbox\Research\Dissertation\Data Anaylisis - Barbieri Replication\Chapter3.txt", clear
*stset year, fail(mid) id(id)
stset peace, fail(mid) id(id)
gen peace0 = peace - 1
save trademids, replace
