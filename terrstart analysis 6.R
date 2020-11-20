setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset")
library(readstata13)
library(dplyr)
library(tvcure)
library(compiler)
library(beepr)
options(scipen = 999)
cl <- makeCluster(4, "SOCK")
registerDoSNOW(cl)
cmpfun(tvcure)
enableJIT(3)

##### International models -------------------------------------------------
i0_cure = tvcure(Surv(start, stop, fail) ~
                   pchcap + bdymid + systchange + ww1 + ww2 + coldwar,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = dyaddata,
                 brglm = F, var = T, nboot = 30); summary(i0_cure)

i1_cure = tvcure(Surv(start, stop, fail) ~
                   lpchcap + lbdymid + systchange + ww1 + ww2 + coldwar,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = dyaddata,
                 brglm = F, var = T, nboot = 30); summary(i1_cure)

##### Leadership change models -------------------------------------------------
l0_cure = tvcure(Surv(start, stop, fail) ~
                   leadch0 + independence + regtransdy,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = dyaddata,
                 brglm = F, var = T, nboot = 30); summary(l0_cure)

l1_cure = tvcure(Surv(start, stop, fail) ~
                   leadch1 + lindependence + lregtransdy,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = dyaddata,
                 brglm = F, var = T, nboot = 30); summary(l1_cure)

##### Solschange models -------------------------------------------------
s0_cure = tvcure(Surv(start, stop, fail) ~
                   solsch0 + independence + regtransdy,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = dyaddata,
                 brglm = F, var = T, nboot = 30); summary(s0_cure)

s1_cure = tvcure(Surv(start, stop, fail) ~
                   solsch1 + lindependence + lregtransdy,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = dyaddata,
                 brglm = F, var = T, nboot = 30); summary(s1_cure)
                   solsch0 + independence + independencelnt + regtransdy + regtransdylnt, 
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = dyaddata,
                 brglm = F, var = T, nboot = 50); summary(s0_cure1)

##### PRD models -------------------------------------------------
s0_cox_prd = coxph(Surv(start, stop, fail) ~
                     solsch0 + independence + regtransdy + 
                     lnccdist + lagterrch + postcolonial + colonycontig + 
                     onemp + twomp + defense + demdy + trival,
                   data = dyaddata, subset = dyaddata$polrel == 1); summary(s0_cox_prd)
s1_cox_prd = coxph(Surv(start, stop, fail) ~
                     solsch1 + lindependence + lregtransdy + 
                     lnccdist + lagterrch + postcolonial + colonycontig + 
                     onemp + twomp + defense + demdy + trival,
                   data = dyaddata, subset = dyaddata$polrel == 1); summary(s1_cox_prd)
l0_cox_prd = coxph(Surv(start, stop, fail) ~
                     leadch0 + independence + independencelnt + regtransdy + regtransdylnt + 
                     lnccdist + lagterrch + postcolonial + colonycontig + 
                     onemp + twomp + defense + demdy,
                   data = dyaddata, subset = dyaddata$polrel == 1); summary(l0_cox_prd)
l1_cox_prd = coxph(Surv(start, stop, fail) ~
                     leadch1 + lindependence + independencelnt + lregtransdy + regtransdylnt + 
                     lnccdist + lagterrch + postcolonial + colonycontig + 
                     onemp + twomp + defense + demdy,
                   data = dyaddata, subset = dyaddata$polrel == 1); summary(l1_cox_prd)
s0_cox_riv = coxph(Surv(start, stop, fail) ~
                     solsch0 + independence + regtransdy + 
                     lnccdist + lagterrch + postcolonial + colonycontig + 
                     onemp + twomp + defense + demdy,
                   data = dyaddata, subset = dyaddata$trival == 1); summary(s0_cox_riv)
s1_cox_riv = coxph(Surv(start, stop, fail) ~
                     solsch1 + lindependence + lregtransdy +
                     lnccdist + lagterrch + postcolonial + colonycontig + 
                     onemp + twomp + defense + demdy,
                   data = dyaddata, subset = dyaddata$trival == 1); summary(s1_cox_riv)
l0_cox_riv = coxph(Surv(start, stop, fail) ~
                     leadch0 + independence + independencelnt + regtransdy + regtransdylnt + 
                     lnccdist + lagterrch + postcolonial + colonycontig + 
                     onemp + twomp + defense + demdy + trival,
                   data = dyaddata, subset = dyaddata$trival == 1); summary(l0_cox_riv)
l1_cox_riv = coxph(Surv(start, stop, fail) ~
                     leadch1 + lindependence + independencelnt + lregtransdy + regtransdylnt + 
                     lnccdist + lagterrch + postcolonial + colonycontig + 
                     onemp + twomp + defense + demdy + trival,
                   data = dyaddata, subset = dyaddata$trival == 1); summary(l1_cox_riv)

##### Civwar models -------------------------------------------------
l0_cure_cw = tvcure(Surv(start, stop, fail) ~
                      leadch0 + independence + independencelnt + regtransdy + regtransdylnt + cwany +
                    cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                      onemp + twomp + defense + demdy + trival,
                    data = dyaddata, 
                    brglm = F, var = T, nboot = 30); summary(l0_cure_cw)

s0_cure_cw = tvcure(Surv(start, stop, fail) ~
                      solsch0 + independence + independencelnt + regtransdy + regtransdylnt + cwany,
                    cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                      onemp + twomp + defense + demdy + trival,
                    data = dyaddata,
                    brglm = F, var = T, nboot = 30); summary(s0_cure_cw)
