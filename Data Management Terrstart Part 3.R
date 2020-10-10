setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset")
library(readstata13)
library(dplyr)

################################################################################
### Data management functions
################################################################################

undirdyads <- function(df, ccode1, ccode2) {
  attach(df)
  ccodes = cbind(ccode1, ccode2)
  cmin   = sprintf("%03d", matrixStats::rowMins(ccodes))
  cmax   = sprintf("%03d", matrixStats::rowMaxs(ccodes))
  dyad = as.numeric(paste(cmin, cmax, sep = ""))
  detach(df)
  return(dyad)
}

################################################################################
### Load data from stata
################################################################################
# rm(list=setdiff(ls(), "eu3"))
eu3 = read.dta13("./data/eu3.dta")
eu3 = eu3 %>% mutate(
  lagsolsch = lag(solschany),
  lagleadch_chisols = lag(leadtransany)
)

################################################################################
# Format colonial contiguity data
################################################################################

# colonycontig = read.dta13("./data/contcold.dta") oldversion
colonycontig = read.dta13("./data/contdird.dta")
colonycontig = select(colonycontig, dyad, year)
colonycontig$colonycontig = 1

eu3 = left_join(eu3, colonycontig)

################################################################################
# Format postcolonial history data
################################################################################

postcolonial = read.csv("./data/coldata110.csv")
colnames(postcolonial)[1] = "colony"
postcolonial = postcolonial %>%
  select(colony, ColRuler, IndDate)
postcolonial$dyad = undirdyads(postcolonial, colony, ColRuler)
postcolonial$IndDate = as.character(postcolonial$IndDate)
postcolonial$colindyear = as.integer(substr(postcolonial$IndDate, 1, 4))
postcolonial = select(postcolonial, -IndDate, -colony, -ColRuler)
eu3 = left_join(eu3, postcolonial)

################################################################################
### Code new variables
################################################################################

# Postcolonial history
eu3$postcolonial = eu3$year >= eu3$colindyear 

# Colonial contiguity
eu3$colonycontig = replace(colonycontig, is.na(colonycontig), 0)

# International norms
eu3$tin = eu3$year > 1944

# Shock variables
eu3$independence = (eu3$year >= eu3$statebirthyear1 & eu3$year <= eu3$statebirthyear1 + 5) | (eu3$year >= eu3$statebirthyear2 & eu3$year <= eu3$statebirthyear2 + 5)
eu3$ww1 = eu3$year >= 1914 & eu3$year <= 1923
eu3$ww2 = eu3$year >= 1937 & eu3$year <= 1950
eu3$coldwar = eu3$year >= 1989 & eu3$year <= 1994
eu3$systchange = eu3$year >= 1859 & eu3$year <= 1977

#rm(list=setdiff(ls(), c("eu3", "d", "e", "f", "g", "h", "i", "j")))