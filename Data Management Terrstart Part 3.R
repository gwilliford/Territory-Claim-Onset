setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset")
library(readstata13)
library(dplyr)
library(haven)
library(matrixStats)

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
eu3 = read_dta("./data/eu3.dta")
# eu3 = eu3 %>% mutate(
#   lagsolsch = lag(solschany),
#   lagleadch_chisols = lag(leadtransany)
# )

################################################################################
# Format colonial contiguity data
################################################################################

# colonycontig = read.dta13("./data/contcold.dta") oldversion
colonycontig = read.dta13("./data/contdird.dta")
colonycontig = select(colonycontig, dyad, year)
colonycontig$colonycontig = 1

eu3 = left_join(eu3, colonycontig)
eu3$lnt = log(eu3$stop)

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
# Format COW cw data
################################################################################
# 
# # rowMins(x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...)
# 
# cowcw = read.csv("./data/INTRA-STATE_State_participants v5.1 CSV.csv")
# cowcw$StartYr1 = replace(cowcw$StartYr1, cowcw$StartYr1 == -9, NA)
# cowcw$StartYr2 = replace(cowcw$StartYr2, cowcw$StartYr2 == -9, NA)
# cowcw$StartYr3 = replace(cowcw$StartYr3, cowcw$StartYr3 == -9, NA)
# cowcw$StartYr4 = replace(cowcw$StartYr4, cowcw$StartYr4 == -9, NA)
# cowcw$StartYr1 = replace(cowcw$StartYr1, cowcw$StartYr1== -8, NA)
# cowcw$StartYr2 = replace(cowcw$StartYr2, cowcw$StartYr2 == -8, NA)
# cowcw$StartYr3 = replace(cowcw$StartYr3, cowcw$StartYr3 == -8, NA)
# cowcw$StartYr4 = replace(cowcw$StartYr4, cowcw$StartYr4 == -8, NA)
# # 
# # # Code ongoing as 2014
# cowcw$EndYr1 = replace(cowcw$EndYr1, cowcw$EndYr1 == -7, 2014)
# cowcw$EndYr2 = replace(cowcw$EndYr2, cowcw$EndYr2 == -7, 2014)
# cowcw$EndYr3 = replace(cowcw$EndYr3, cowcw$EndYr3 == -7, 2014)
# cowcw$EndYr4 = replace(cowcw$EndYr4, cowcw$EndYr4 == -7, 2014)
# # Code missing if cw 2, 3, 4 do not occur
# cowcw$EndYr1 = replace(cowcw$EndYr1, cowcw$EndYr1 == -8, NA)
# cowcw$EndYr2 = replace(cowcw$EndYr2, cowcw$EndYr2 == -8, NA)
# cowcw$EndYr3 = replace(cowcw$EndYr3, cowcw$EndYr3 == -8, NA)
# cowcw$EndYr4 = replace(cowcw$EndYr4, cowcw$EndYr4 == -8, NA)
# # Code missing for missing values
# cowcw$EndYr1 = replace(cowcw$EndYr1, cowcw$EndYr1 == -9, NA)
# cowcw$EndYr2 = replace(cowcw$EndYr2, cowcw$EndYr2 == -9, NA)
# cowcw$EndYr3 = replace(cowcw$EndYr3, cowcw$EndYr3 == -9, NA)
# cowcw$EndYr4 = replace(cowcw$EndYr4, cowcw$EndYr4 == -9, NA)
# 
# cowcw = cowcw %>% 
#   rowwise() %>% 
#   mutate(cwbegyear = min(StartYr1, StartYr2, StartYr3, StartYr4), 
#          cwendyear = Mean = mean(c(A, B, C)))
# )

# cowcw$cwbegyear = rowMins(as.matrix(cbind(cowcw$EndYr1, cowcw$EndYr2, cowcw$EndYr3, cowcw$EndYr4)), na.rm = T)
# cowcw$cwendyear = rowMaxs(as.matrix(cbind(cowcw$EndYr1, cowcw$EndYr2, cowcw$EndYr3, cowcw$EndYr4)), na.rm = T)
# 
# subm = select(cowcw, EndYr1, EndYr2, EndYr3, EndYr4, EndYr1, EndYr2, EndYr3, EndYr4)
# subm$StartYr1 = replace(subm$StartYr1, subm$StartYr1 == -9, NA)
# subm$StartYr2 = replace(subm$StartYr2, subm$StartYr2 == -9, NA)
# subm$StartYr3 = replace(subm$StartYr3, subm$StartYr3 == -9, NA)
# subm$StartYr4 = replace(subm$StartYr4, subm$StartYr4 == -9, NA)
# subm$StartYr1 = replace(subm$StartYr1, subm$StartYr1 == -8, NA)
# subm$StartYr2 = replace(subm$StartYr2, subm$StartYr2 == -8, NA)
# subm$StartYr3 = replace(subm$StartYr3, subm$StartYr3 == -8, NA)
# subm$StartYr4 = replace(subm$StartYr4, subm$StartYr4 == -8, NA)
# 
# # Code ongoing as 2014
# subm$EndYr1 = replace(subm$EndYr1, subm$EndYr1 == -7, 2014)
# subm$EndYr2 = replace(subm$EndYr2, subm$EndYr2 == -7, 2014)
# subm$EndYr3 = replace(subm$EndYr3, subm$EndYr3 == -7, 2014)
# subm$EndYr4 = replace(subm$EndYr4, subm$EndYr4 == -7, 2014)
# # Code missing if cw 2, 3, 4 do not occur
# subm$EndYr1 = replace(subm$EndYr1, subm$EndYr1 == -8, NA)
# subm$EndYr2 = replace(subm$EndYr2, subm$EndYr2 == -8, NA)
# subm$EndYr3 = replace(subm$EndYr3, subm$EndYr3 == -8, NA)
# subm$EndYr4 = replace(subm$EndYr4, subm$EndYr4 == -8, NA)
# # Code missing for missing values
# subm$EndYr1 = replace(subm$EndYr1, subm$EndYr1 == -9, NA)
# subm$EndYr2 = replace(subm$EndYr2, subm$EndYr2 == -9, NA)
# subm$EndYr3 = replace(subm$EndYr3, subm$EndYr3 == -9, NA)
# subm$EndYr4 = replace(subm$EndYr4, subm$EndYr4 == -9, NA)

# cowcw$EndYr1 = replace(cowcw$EndYr1, cowcw$EndYr1 == -7, NA)
# cowcw$EndYr2 = replace(cowcw$EndYr1, cowcw$EndYr2 == -7, NA)
# cowcw$EndYr3 = replace(cowcw$EndYr1, cowcw$EndYr3 == -7, NA)
# cowcw$EndYr4 = replace(cowcw$EndYr1, cowcw$EndYr4 == -7, NA)
# cowcw$EndYr1 = replace(cowcw$EndYr1, cowcw$EndYr1 == -7, NA)
# cowcw$EndYr2 = replace(cowcw$EndYr1, cowcw$EndYr2 == -7, NA)
# cowcw$EndYr3 = replace(cowcw$EndYr1, cowcw$EndYr3 == -7, NA)
# cowcw$EndYr4 = replace(cowcw$EndYr1, cowcw$EndYr4 == -7, NA)

# cowcw = cowcw %>%
#   filter(ccodeA > 0)
#   mutate(
#   ) %>%
#   select("ccodeA" = "CcodeA", cwbegyear, cwendyear)
#   
# cowcw$exp = cowcw$cwbegyear - cowcw$cwendyear + 1

### Code new variables
################################################################################


# Mp status
eu3$onemp = replace(eu3$onemp, is.na(eu3$onemp), 0)
eu3$twomp = replace(eu3$twomp, is.na(eu3$twomp), 0)

# TEK
eu3$tek = eu3$TEK

# Regime change
eu3$regtransdy = eu3$regtrans1 == 1 | eu3$regtrans2 == 1
eu3$regtransdy = replace(eu3$regtransdy, is.na(eu3$regtransdy), 0)
eu3$regtransdy = replace(eu3$regtransdy, eu3$year < 1918, NA)
eu3$regtransdylnt = eu3$regtransdy * eu3$lnt
eu3 = eu3 %>% mutate(lregtransdy = lag(regtransdy))

# Postcolonial history
eu3$postcolonial = eu3$year >= eu3$colindyear 
eu3$postcolonial = replace(eu3$postcolonial, is.na(eu3$postcolonial), 0)

# Colonial contiguity
eu3$colonycontig = replace(eu3$colonycontig, is.na(eu3$colonycontig), 0)
eu3$colonycontiglnt = eu3$colonycontig * eu3$lnt

# International norms
  # 1958 convention on the high seas
  # LOST comes into force in 1994
  # LOST convention concluded in 1982
  # Lost convention began 1973
eu3$convhighseas = eu3$year >= 1958
eu3$lostconvbeg = eu3$year >= 1973
eu3$lostconvend = eu3$year >= 1982
eu3$lostpassed = eu3$year >= 1994
eu3$decolonization = eu3$year >= 1960
eu3$tin = eu3$year > 1945

# MIDs
eu3 = eu3 %>%
  group_by(dyad) %>%
  mutate(
    runsummid = cumsum(ndymid)
  )

# Shock variables
# eu3$independence = (eu3$year >= eu3$statebirthyear1 & eu3$year <= eu3$statebirthyear1 + 5) | (eu3$year >=                                                                                             eu3$statebirthyear2 & eu3$year <= eu3$statebirthyear2 + 5)
eu3$independence = eu3$statebirthyear1 == eu3$year | eu3$statebirthyear2 == eu3$year
eu3$independencelnt = eu3$independence * eu3$lnt
eu3$ww1 = eu3$year >= 1914 & eu3$year <= 1923
eu3$ww2 = eu3$year >= 1937 & eu3$year <= 1950
eu3$coldwar = eu3$year >= 1989 & eu3$year <= 1994
eu3$systchange = eu3$year >= 1859 & eu3$year <= 1977

# Civil war in year
eu3$cwany = eu3$cw1 == 1 | eu3$cw2 == 1
eu3$cwany = replace(eu3$cwany, is.na(eu3$cwany) & eu3$year > 1945, 0)
eu3$cwany = replace(eu3$cwany, eu3$year < 1946, NA)

eu3 = eu3 %>% mutate(
  lregtrans = lag(regtransdy),
  lindependence = lag(independence)
)
  #cwshock = cwany == T | lag(cwany) == T | lag(cwany, 2) == T | lag(cwany, 3) == T | lag(cwany, 4) == T,
  #regtransshock = regtrans == T | lag(regtrans) == T | lag(regtrans, 2) == T | lag(regtrans, 3) == T | lag(regtrans, 4) == T,

# eu3$cwshock = replace(eu3$cwshock, eu3$year < 1946, NA)
# eu3$cwshock = replace(eu3$cwshock, eu3$year >= 1946 & is.na(eu3$cwshock), 0)

# Polrel
eu3$polrel = eu3$majpower == 1 | eu3$contdir == 1

# Logtime interactions
eu3$lnccdistlnt = eu3$lnccdist * eu3$lnt
eu3$demdylnt = eu3$demdy * eu3$lnt
eu3$lagterrchlnt = eu3$lagterrch * eu3$lnt
eu3$bdymidlnt = eu3$bdymid * eu3$lnt
eu3$ww2lnt = eu3$ww2 * eu3$lnt
eu3$onemplnt = eu3$onemp * eu3$lnt

# Recode missing based on dates
eu3$TEK = ifelse(eu3$year < 1960, NA, eu3$TEK)
eu3$TEK = ifelse(eu3$year >= 1960, 0, eu3$TEK)
eu3$leadchdy0 = replace(eu3$leadchdy0, eu3$year < 1875, NA)
eu3$leadchdy1 = replace(eu3$leadchdy0, eu3$year < 1875, NA)
eu3$leadchdy2 = replace(eu3$leadchdy0, eu3$year < 1875, NA)
eu3$leadchdy012 = replace(eu3$leadchdy0, eu3$year < 1875, NA)
eu3$solschdy0 = replace(eu3$solschdy0, eu3$year < 1918, NA)
eu3$solschdy1 = replace(eu3$solschdy0, eu3$year < 1918, NA)
eu3$solschdy2 = replace(eu3$solschdy0, eu3$year < 1918, NA)
eu3$solschdy012 = replace(eu3$solschdy0, eu3$year < 1918, NA)

eu3 = eu3 %>% 
  arrange(dyad, year) %>%
  group_by(dyad) %>% 
  mutate(
    leadchdy1 = lag(leadchdy0), 
    solschdy1 = lag(solschdy0)
  )
