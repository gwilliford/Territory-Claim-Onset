################################################################################
################################################################################
### Identify claims that begin after dyad birth
################################################################################
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
                             regtrans, demtrans, auttrans, autend)
chisols1 = chisols %>% rename("chal" = "ccode", "solschchal" = "solschdum", "regtranschal" = "regtrans")
chisols2 = chisols %>% rename("tgt" = "ccode", "solschtgt" = "solschdum", "regtranstgt" = "regtrans")

archigos = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/archdum1.dta")
archigos1 = rename(archigos, "chal" = "ccode1", "leadchchal" = "leadch1")
archigos2 = rename(archigos, "tgt" = "ccode1", "leadchtgt" = "leadch1")

# icow_claims = full_join(icow_claims, chisols1)
# icow_claims = full_join(icow_claims, chisols1)
# icow_claims = full_join(icow_claims, chisols1)
# icow_claims = full_join(icow_claims, chisols1)

postbirth     = icow_claims %>% filter(dyadbirth == F)
postbirth$gap = postbirth$claimstartyear - postbirth$dyadbirthyear
postbirth     = postbirth %>% relocate(claim, name, dyad, begclaim, endclaim, chal,
                                   tgt, dyadbirthyear, chalbirthyear, tgtbirthyear, gap,
                                   idenchal, identgt, idenany, histchal, histtgt, histany,
                                   homechal, hometgt, homeany, res, pop, stratloc, offshore,
                                   tcentirecl)




################################################################################
### Create subsets
################################################################################
postbirthiden = filter(postbirth, idenchal == T)
View(postbirthiden)

postbirthiden = filter(postbirth, idenchal == T, region != 2)
View(postbirthiden)

> View(icow_claims[dyad == "520530",])
> View(icow_claims[chal == 811 | tgt == 811,])
View(icow_claims[chal == 750 | tgt == 750 & postbirth == T,])
View(icow_claims[dyad == 145150,])

View(icow_claims[dyad == 471475,])
View(icow_claims[dyad == 800811,])


library(writexl)
write_xlsx(postbirth, "C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/icowtypes.xlsx")
)