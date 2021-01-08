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
                data = terrstart,
                brglm = F, var = T, nboot = 30); summary(i0_cure)

i1_cure = tvcure(Surv(start, stop, fail) ~
                  lagpchcap + lbdymid + systchange + ww1 + ww2 + ww2lnt + coldwar,
                cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                  onemp + twomp + defense + demdy + trival,
                data = eu3,
                brglm = F, var = T, nboot = 30); summary(i0_cure)

##### Leadership change models -------------------------------------------------
l0_cure = tvcure(Surv(start, stop, fail) ~
                   leadchdy0 + independence + independencelnt + regtransdy + regtransdylnt,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(l0_cure)
l1_cure = tvcure(Surv(start, stop, fail) ~
                   leadchdy1 + independence + independencelnt + regtransdy + regtransdylnt,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(l1_cure)

##### Solschange models -------------------------------------------------
s0_cure = tvcure(Surv(start, stop, fail) ~
                   solschdy0 + independence + independencelnt + regtransdy + regtransdylnt,
                   cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(s0_cure)
s1_cure = tvcure(Surv(start, stop, fail) ~
                   solschdy1 + independence + independencelnt + regtransdy + regtransdylnt,
                   cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(s1_cure)				 
s0_cox_prd = coxph(Surv(start, stop, fail) ~
                   solschdy0 + independence + independencelnt + regtransdy + regtransdylnt + 
				   lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3); summary(s0_cox_prd)
s1_cox_prd = coxph(Surv(start, stop, fail) ~
                   solschdy1 + independence + independencelnt + regtransdy + regtransdylnt + 
				   lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3); summary(s1_cox_prd)



##### Civwar models -------------------------------------------------
l0_cure_cw = tvcure(Surv(start, stop, fail) ~
                    leadchdy0 + independence + independencelnt + regtransdy + regtransdylnt + cwany +
                    pchcaprat + bdymid + systchange + ww1 + ww2 + coldwar,
                  cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                    onemp + twomp + defense + demdy + trival,
                  data = eu3,
                  brglm = F, var = T, nboot = 30); summary(l0_cure_cw)

s0_cure_cw = tvcure(Surv(start, stop, fail) ~
                    solschdy0 + independence + independencelnt + regtransdy + regtransdylnt + cwany,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(s0_cure_cw)
