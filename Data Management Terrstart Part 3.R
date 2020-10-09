setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset")
library(readstata13)
library(dplyr)

##### Load data -------------------------------------------------------------------------
# rm(list=setdiff(ls(), "eu3"))
eu3 = read.dta13("./data/eu3.dta")
eu3 = eu3 %>% mutate(
  lagsolsch = lag(solschany),
  lagleadch_chisols = lag(leadtransany)
)

eu3$solsch_tandlag = eu3$solschany == 1 | eu3$lagsolsch == 1 
eu3$tin = eu3$year > 1944
#eu3$twomaj = eu3$m

# Shock variables
eu3$independence = (eu3$year >= eu3$statebirthyear1 & eu3$year <= eu3$statebirthyear1 + 5) | (eu3$year >= eu3$statebirthyear2 & eu3$year <= eu3$statebirthyear2 + 5)
eu3$ww1 = eu3$year >= 1914 & eu3$year <= 1923
eu3$ww2 = eu3$year >= 1937 & eu3$year <= 1950
eu3$coldwar = eu3$year >= 1989 & eu3$year <= 1994
eu3$systchange = eu3$year >= 1859 & eu3$year <= 1977

#rm(list=setdiff(ls(), c("eu3", "d", "e", "f", "g", "h", "i", "j")))