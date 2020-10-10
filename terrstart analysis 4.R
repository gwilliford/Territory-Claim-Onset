setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset")
library(readstata13)
library(dplyr)
library(tvcure)
library(compiler)
options(scipen = 999)
cl <- makeCluster(4, "SOCK")
registerDoSNOW(cl)
cmpfun(tvcure)
enableJIT(3)

##### Leadership change models -------------------------------------------------
l0 = tvcure(Surv(start, stop, fail) ~
              leadchdy0 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l0)

l1 = tvcure(Surv(start, stop, fail) ~
              leadchdy1 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l1)

l2 = tvcure(Surv(start, stop, fail) ~ 
              leadchdy2 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l2)

l012 = tvcure(Surv(start, stop, fail) ~ 
                leadchdy012 + lcwany + independence,
              cureform = ~ lagterrch + lnccdist + 
                majpower + defense + demdy + trival,
              data = eu3,
              brglm = F, var = T, nboot = 30); summary(l012)

##### solschange models --------------------------------------------------------

s0 = tvcure(Surv(start, stop, fail) ~ 
              leadchdy0 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l0)

s1 = tvcure(Surv(start, stop, fail) ~ 
              leadchdy1 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l1)

s2 = tvcure(Surv(start, stop, fail) ~ 
              leadchdy2 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l2)

s012 = tvcure(Surv(start, stop, fail) ~ 
                leadchdy012 + lcwany + independence,
              cureform = ~ lagterrch + lnccdist + 
                majpower + defense + demdy + trival,
              data = eu3,
              brglm = F, var = T, nboot = 30); summary(l012)

##### International models -----------------------------------------------------

i0 = tvcure(Surv(start, stop, fail) ~ 
              pchcaprat + bdymid + systchange + ww1 + ww2 + coldwar,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l0)

i1 = tvcure(Surv(start, stop, fail) ~ 
              lagpchcap + lbdymid + systchange + ww1 + ww2 + coldwar,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l1)

##### TODO
# Interaction effects for leadership x solschange
# TEK Models for leadership change and solschange
# TEK Models for international
