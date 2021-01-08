cd "C:\Users\gwill\Dropbox\Research\Dissertation\Data Management\sourcedata"
use "C:\Users\gwill\Downloads\dataverse_files (17)\Dreyer_ISQ_Rep_Data.dta"
xtset rivdyad year
gen nonspat = pos_iden == 1 | pos_ideo == 1 | iden_ideo == 1 | pos_iden_ideo == 1 | only_pos == 1 | only_ideo == 1 | only_iden == 1
gen spataddon = nonspat == 0 & l.nonspat == 1
