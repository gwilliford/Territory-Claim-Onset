# Estimating Unrealized Gains from Trade
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis")
library(readr)
library(readxl)
library(haven)
library(readstata13)
library(dplyr)
library(matrixStats)
library(lme4)
library(MuMIn)
library(optimx)
library(DescTools)

### Import Monadic Data
madd = read_csv("./data/madd.csv")
dcap = read_dta("./data/NMC_5_0.dta")
dpol = read_excel("./data/p4v2016.xls")
dpol = select(dpol, ccode, year, polity2)
dw   = read_dta("./data/bdm2s2_nation_year_data_may2002.dta") %>%
  select(ccode, year, W, S, GovCrises, strikes)
chisols <- read_dta('./data/CHISOLSstyr4_0.dta')
select(dpol, ccode, year, polity2)
chisols <- dplyr::select(chisols, ccode, year, totalldrtrans, leadertrans, solschange, solschdum, solschange30, solsch30dum, solsminchange, solsminchdum)

### Create common identifiers for dyadic data
undirdyads <- function(df, ccode1, ccode2) {
  attach(df)
  ccodes = cbind(ccode1, ccode2)
  cmin   = sprintf("%03d", matrixStats::rowMins(ccodes))
  cmax   = sprintf("%03d", matrixStats::rowMaxs(ccodes))
  dyad = as.numeric(paste(cmin, cmax, sep = ""))
  detach(df)
  return(dyad)
}

### Monadic trade
mtrade <- read_csv('./data/National_COW_4.0.csv')
mtrade$importsmil <- mtrade$imports * 1000000
mtrade$exportsmil <- mtrade$exports * 1000000
mtrade$aggtrade <- mtrade$imports + mtrade$exports
mtrade$aggtrademil <- mtrade$importsmil + mtrade$exportsmil
mtrade <- mtrade %>%
  select(ccode, year, imports, exports, importsmil, exportsmil, aggtrade, aggtrademil)

### Leader support data
ead <- read_dta("./data/EAD+2.0+Annual+0101019.dta")

### ACD data
acd <- read_excel("./data/ucdpprioacd.xlsm")
acd$ccode <- as.numeric(acd$gwno_a)
acd$cw <- 1
acd <- acd %>%
  group_by(ccode, year) %>%
  summarize(cw = max(cw))

### Merge Monadic Data
dmon = full_join(madd, dcap)
dmon = full_join(dmon, dpol)
dmon = full_join(dmon, chisols)
dmon = full_join(dmon, dw)
dmon = full_join(dmon, ead)
dmon = full_join(dmon, mtrade)
dmon = full_join(dmon, acd)

### Monadic Lags
dmon <- dmon %>%
  arrange(ccode, year) %>%
  mutate(
    
    # Trade vars
    ln_aggtrade = ifelse(is.na(aggtrade), 0, log(aggtrade)),
    ln_aggtrademil = ifelse(is.na(aggtrademil), 0, log(aggtrademil)),
    lag_ln_aggtrade = lag(ln_aggtrade),
    lag_ln_aggtrademil = lag(ln_aggtrademil),
    
    # GDP vars
    lag_gdp = lag(gdp), 
    lag_gdpcap = lag(gdpcap),
    ln_gdp = ifelse(gdp == 0, 0, log(gdp)),
    lag_ln_gdp = lag(ln_gdp),
    ln_gdpcap = ifelse(gdpcap == 0, 0, log(gdpcap)),
    lag_ln_gdpcap = lag(ln_gdpcap),
    pch_gdp    = (gdp - lag_gdp)/lag_gdp * 100,
    pch_gdpcap = (gdpcap - lag_gdpcap)/lag_gdpcap * 100,
    pch_ln_gdp    = (ln_gdp - lag_ln_gdp)/lag_ln_gdp * 100,
    pch_ln_gdpcap = (ln_gdpcap - lag_ln_gdpcap)/lag_ln_gdpcap * 100,
    
    # Control vars
    lcinc = lag(cinc) * 10,
    lag_pop = lag(pop),
    ln_pop = log(pop),
    lag_ln_pop = lag(ln_pop),
    
    # Institutional vars
    lpol = lag(polity2), 
    lagW = lag(W),
    lagS = lag(S),
    lcw = lag(cw)
  )

# Check for duplicates
sum(duplicated(dmon[, c("ccode", "year")]))
dmon[duplicated(dmon[, c("ccode", "year")]),]

# Create separate versions of monadic data
dmon1 = dmon %>% select(-"version") %>% setNames(paste0(names(.), "1")) %>% rename(year = year1)
dmon2 = dmon %>% select(-"version") %>% setNames(paste0(names(.), "2")) %>% rename(year = year2)
#write.csv(dmon1, "./data/monad1.csv")
write_dta(dmon1, "./data/monad1.dta", version = 13)
write_dta(dmon2, "./data/monad2.dta", version = 13)
