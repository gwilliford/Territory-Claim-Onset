#### Merge monadic data with eugene files

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

dmon1 = read.dta13("./data/monad1.dta")
dmon2 = read.dta13("./data/monad2.dta")

##### Format files for tergen analysis
eugene = read.dta13("./data/DirectedDyadEugeneMaster2017_01_29.dta")
eugene = eugene %>% filter(dyadA == 1)
eugene$ccode1 = as.numeric(eugene$ccode1)
eugene$ccode2 = as.numeric(eugene$ccode2)
eugene = eugene %>% select(-polity21, -gdpcap1, -polity22, -gdpcap2)
eu2 = left_join(eugene, dmon1)
eu2 = left_join(eu2, dmon2)

# Code dyad dummies
eu2$solschany = ifelse(eu2$solschdum2 == 1 | eu2$solschdum1 == 1, 1, 0)
eu2$leadtransany = ifelse(eu2$leadertrans1 == 1 | eu2$leadertrans2 == 1, 1, 0)

# Var creation
eu2$capmax = rowMaxs(as.matrix(eu2[, c("cap_1", "cap_2")]))
eu2$capmin = rowMins(as.matrix(eu2[, c("cap_1", "cap_2")]))
eu2$caprat = eu2$capmin / (eu2$capmin + eu2$capmax)
eu2$contdir = ifelse(eu2$contig == 1, 1, 0)

# Lags
eu2 = eu2 %>%
  mutate(
    lcaprat = lag(caprat),
    pchcaprat = lag(caprat) / caprat,
    lally = lag(alliance)
  )

#### Kick out to stata for duration generation - datas management - eu3.do
save.dta13(eu2, "C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/eu2.dta")
