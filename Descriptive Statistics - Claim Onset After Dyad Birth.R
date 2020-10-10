################################################################################
### Identify claims that begin after dyad birth
### Identify claims that begin after leader and coalition changes
################################################################################

library(dplyr)
library(matrixStats)
library(readstata13)

################################################################################
### Create state birth variables
################################################################################

states <- read.csv("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/system2016.csv")
states <- states %>% 
  select(ccode, year) %>% 
  group_by(ccode) %>% 
  summarize(statebirthyear = min(year))
stateschal = states %>% rename("chal" = "ccode", "chalbirthyear" = "statebirthyear")
statestgt = states %>% rename("tgt" = "ccode", "tgtbirthyear" = "statebirthyear")

################################################################################
### Load ICOW claims dataset
################################################################################

icow_claims = read.csv("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/ICOW Territory Provisional Data 1.01/ICOWprov101.csv")
icow_claims = left_join(icow_claims, stateschal) 
icow_claims = left_join(icow_claims, statestgt) 

################################################################################
### Create dyad startyear
################################################################################

icow_claims$begclaim = as.character(icow_claims$begclaim)
icow_claims$claimstartyear = as.integer(substr(icow_claims$begclaim, 1, 4))
icow_claims$claimstartmonth = as.integer(substr(icow_claims$begclaim, 5, 6))

################################################################################
### Identify claims that begin after dyad birth
################################################################################

icow_claims$chalbirthdum = icow_claims$chalbirthyear == icow_claims$claimstartyear
icow_claims$tgtbirthdum = icow_claims$tgtbirthyear == icow_claims$claimstartyear
icow_claims$dyadbirth = icow_claims$chalbirthdum == 1 | icow_claims$tgtbirthyear == 1
icow_claims$dyadbirthyear = rowMins(cbind(icow_claims$chalbirthyear, icow_claims$tgtbirthyear))

################################################################################
### Get salience variables from claimyear data
################################################################################

icow_claimyear = read.csv("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/ICOW Territory Provisional Data 1.01/ICOWprovyr101.csv")
icow_claimsum = icow_claimyear %>%
  group_by(claimdy) %>% 
  summarize(
    claim = max(claim), 
    idenchal = max(tcidenchal),
    identgt = max(tcidentgt),
    idenany = rowMaxs(cbind(idenchal, identgt)),
    histchal = max(tchistchal),
    histtgt = max(tchisttgt),
    histany = rowMaxs(cbind(histchal, histtgt)),
    homechal = max(tchomechal),
    hometgt = max(tchometgt),
    homeany = rowMaxs(cbind(homechal, hometgt)),
    res = max(tcresource) > 0,
    pop = max(tcpop),
    stratloc = max(tcstratloc),
    offshore = max(tcoffshore) > 0,
    tcentirecl = max(tcentirecl) > 0
  )
icow_claims   = full_join(icow_claims, icow_claimsum)

################################################################################
### Import chisols and archigos data
################################################################################
chisols = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/CHISOLSstyr4_0.dta")
chisols = chisols %>% select(ccode, year, "leadtrsols" = "leadertrans", solschange, solschdum,
,,   regtrans, demtrans, auttrans, autend)
chisols1 = chisols %>% rename("chal" = "ccode", "solschchal" = "solschdum", "regtranschal" = "regtrans")
chisols2 = chisols %>% rename("tgt" = "ccode", "solschtgt" = "solschdum", "regtranstgt" = "regtrans")

archigos = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/archdum1.dta")
archigos1 = rename(archigos, "chal" = "ccode1", "leadchchal" = "leadch1")
archigos2 = rename(archigos, "tgt" = "ccode1", "leadchtgt" = "leadch1")

archigosfull = read.dta13("./data/Archigos_v.2.9_leader-year.dta")
archigosfull = archigosfull[, c("leadid", "ccode", "ccname", "idacr", "leader", "startdate", "enddate", "bornin", "died","eindate", "eoutdate", "startobs", "endobs", "year", "entry", "exit", "exit_tv", "fail", "age0", "age", "exitcode")]
archigosfull = archigosfull %>% 
  group_by(leadid) %>% 
  summarize(
    minyear = min(eindate),
    ccode = max(ccode),
    transdum = 1
    )
archigosfull$transdate = format.Date(archigosfull$minyear, "%Y-%m")
archigosfull$year      = as.numeric(format.Date(archigosfull$minyear, "%Y"))
archigosfull$month     = as.numeric(format.Date(archigosfull$minyear, "%m"))
archigosfull = select(archigosfull, -minyear, -leadid)
# archigosfull = archigosfull %>%
#   group_by(ccode, year) %>%
#   summarize(
#     earliesttrans = min(eindate)
#   ) 
# archigosfull = archigosfull %>% 
#   filter(year == lubridate::year(earliesttrans))
af1 = archigosfull %>% rename("chal" = "ccode", "claimstartyear" = "year", 
                              "transdumchal" = "transdum", "transdatechal" = "transdate", "transmonthchal" = "month")
af2 = archigosfull %>% rename("tgt" = "ccode", "claimstartyear" = "year",
                              "transdumtgt" = "transdum", "transdatetgt" = "transdate", "transmonthtgt" = "month")

# icow_claims = left_join(icow_claims, chisols1)
# icow_claims = left_join(icow_claims, chisols2)
# icow_claims = left_join(icow_claims, archigos1)
# icow_claims = left_join(icow_claims, archigos2)

# Merge archigosfull to icow claims data
iclead = left_join(icow_claims, af1)
iclead = left_join(iclead, af2)
iclead$transdumchal = ifelse(is.na(iclead$transdumchal), 0, 1)
iclead$transdumtgt = ifelse(is.na(iclead$transdumtgt), 0, 1)
iclead$transinyear = as.numeric(iclead$transdumchal == 1 | iclead$transdumtgt == 1)
# iclead$transmonthchal = ifelse(is.na(iclead$transmonthchal), 0, iclead$transmonthchal)
# iclead$transmonthtgt = ifelse(is.na(iclead$transmonthtgt), 0, iclead$transmonthtgt)


# trans before/after claim
icleadtrans = iclead[iclead$transinyear == 1, ]
icleadtrans$onemissing = ifelse(is.na(icleadtrans$transmonthchal) | is.na(icleadtrans$transmonthtgt), T, F)
icleadtrans$bothbefore = icleadtrans$claimstartmonth > icleadtrans$transmonthchal | icleadtrans$claimstartmonth > icleadtrans$transmonthtgt
icleadtrans$eitherbefore = icleadtrans$claimstartmonth > icleadtrans$transmonthchal | icleadtrans$claimstartmonth > icleadtrans$transmonthtgt
icleadtrans$eithersame = icleadtrans$claimstartmonth == icleadtrans$transmonthchal | icleadtrans$claimstartmonth == icleadtrans$transmonthtgt 
icleadtrans$transba = ifelse(icleadtrans$eitherbefore == T, "before", NA)
icleadtrans$transba = ifelse(icleadtrans$bothafter == T, "after", icleadtrans$transba)
icleadtrans$transba = replace(icleadtrans$transba, is.na(icleadtrans$transba) & icleadtrans$eithersame == T, "same")
icleadtrans$transba = replace(icleadtrans$transba, is.na(icleadtrans$transba), "after")

icleadtrans$transba2 = icleadtrans$transba
icleadtrans$transba2 = replace(icleadtrans$transba2, icleadtrans$transba == "same", "befsame")
icleadtrans$transba2 = replace(icleadtrans$transba2, icleadtrans$transba == "before", "befsame")
table(icleadtrans$transba)
table(icleadtrans$transba2)
# icleadtrans$transba = as.factor(icleadtrans$transba)

# #icleadtrans$eitherbefore = ifelse((icleadtrans$claimstartmonth > icleadtrans$transmonthchal) & icleadtrans$onemissing == T, T, icleadtrans$eitherbefore)
# icleadtrans$eitherbefore = ifelse((icleadtrans$claimstartmonth > icleadtrans$transmonthtgt) & is.na(icleadtrans$transmonthchal), T, icleadtrans$eitherbefore)
# summary(icleadtrans$eitherbefore)
# 
# icleadtrans$eitherbefore = ifelse(is.na(icleadtrans$eitherbefore) & icleadtrans$claimstartmonth > icleadtrans$transmonthchal &
#                                     is.na(icleadtrans$transmonthtgt), TRUE, icleadtrans$eitherbefore)
# icleadtrans$eitherbefore = ifelse(is.na(icleadtrans$eitherbefore) & icleadtrans$claimstartmonth > icleadtrans$transmonthtgt & 
#                                     is.na(icleadtrans$transmonthchal)
# 
# 
# icleadtrans$bothafter = icleadtrans$claimstartmonth < icleadtrans$transmonthchal & icleadtrans$claimstartmonth < icleadtrans$transmonthtgt
# icleadtrans$eitherafter = icleadtrans$claimstartmonth < icleadtrans$transmonthchal | icleadtrans$claimstartmonth < icleadtrans$transmonthtgt

View(icleadtrans[is.na(icleadtrans$transba), ])


icleadtrans$transba = ifelse(icleadtrans$onemissing == T & icleadtrans$eitherafter == T, "after", icleadtrans$transba)
icleadtrans$transba = ifelse(icleadtrans$eitherafter == T & icleadtrans$eithersame == T, "sametime", icleadtrans$transba)
summary(as.factor(icleadtrans$transba))

if (iclead$bothbefore == T) {
  iclead$transba == "before"
}
if (iclead$eitherafter == T) iclead$transba == "after"
  iclead$transba = "after" 
  iclead$transba = "before"



View(iclead[iclead$transinyear == 1,])
iclead = 

iclead$claim_in_leadch_year = iclead$earliesttranschalyear == iclead$claimstartyear | iclead$earliesttranstgtyear == iclead$claimstartyear

################################################################################
### Identify claims that begin in year of leader transition
################################################################################

leaderchangeclaims = icow_claims %>% filter(leadchchal == 1 | leadchtgt == 1)


solschangeclaims = icow_claims %>% filter(solschchal == 1 | solschtgt == 1)


################################################################################
### Identify claims that occur outside dyad birth
################################################################################

postbirth     = icow_claims %>% filter(dyadbirth == F)
postbirth$gap = postbirth$claimstartyear - postbirth$dyadbirthyear
postbirth     = postbirth %>% relocate(claim, name, dyad, begclaim, endclaim, chal,
, ,          tgt, dyadbirthyear, chalbirthyear, tgtbirthyear, gap,
, ,          idenchal, identgt, idenany, histchal, histtgt, histany,
, ,          homechal, hometgt, homeany, res, pop, stratloc, offshore,
, ,          tcentirecl)

postbirthiden = filter(postbirth, idenchal == T)
View(postbirthiden)

postbirthiden = filter(postbirth, idenchal == T, region != 2)
View(postbirthiden)

View(icow_claims[dyad == "520530",])
View(icow_claims[chal == 811 | tgt == 811,])
View(icow_claims[chal == 750 | tgt == 750 & postbirth == T,])
View(icow_claims[dyad == 145150,])

View(icow_claims[dyad == 471475,])
View(icow_claims[dyad == 800811,])


library(writexl)
write_xlsx(postbirth, "C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/icowtypes.xlsx")
)
