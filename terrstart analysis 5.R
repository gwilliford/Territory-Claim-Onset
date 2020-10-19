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
i0_cox = coxph(Surv(start, stop, fail) ~ 
                 pchcaprat + bdymid + systchange + ww1 + ww2 + coldwar + 
                 lnccdist + lagterrch + postcolonial + colonycontig + 
                  onemp + twomp + defense + demdy + trival,
                data = eu3); summary(i0_cox)
cox.zph(i0_cox)

i0_cox_lnt = coxph(Surv(start, stop, fail) ~ 
                 pchcaprat + bdymid + bdymidlnt + systchange + ww1 + ww2 + ww2lnt + coldwar + 
                 lnccdist + lnccdistlnt + lagterrch + lagterrchlnt + postcolonial + colonycontig + 
                 onemp + onemplnt + twomp + defense + demdy + trival,
               data = eu3); summary(i0_cox)
cox.zph(i0_cox_lnt)


i0_cure = tvcure(Surv(start, stop, fail) ~
                  pchcaprat + bdymid + bdymidlnt + systchange + ww1 + ww2 + ww2lnt + coldwar,
                cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                  onemp + twomp + defense + demdy + trival,
                data = eu3,
                brglm = F, var = T, nboot = 30); summary(i0_cure)


##### Leadership change models -------------------------------------------------

### Initial models
l0_cox = coxph(Surv(start, stop, fail) ~
                 leadchdy0 + independence + independencelnt + regtransdy + regtransdylnt + 
                 lnccdist + lagterrch + lagterrchlnt + 
                 postcolonial + colonycontig + colonycontiglnt +
                 onemp + twomp + defense + demdy + demdylnt + trival,
               data = eu3); summary(l0_cox); cox.zph(l0_cox); beep(8)

l0_cure = tvcure(Surv(start, stop, fail) ~
                   leadchdy0 + independence + independencelnt + regtransdy + regtransdylnt,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(l0_cure); beep(8)

l0_cure_full = tvcure(Surv(start, stop, fail) ~
                   leadchdy0 + independence + independencelnt + regtransdy + regtransdylnt +
                   pchcaprat + bdymid + systchange + ww1 + ww2 + coldwar,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(l0_cure); beep(8)


### Add civil war variable
# l0_cox_cw = coxph(Surv(start, stop, fail) ~
#                    leadchdy0 + independence + lcwany + 
#                   lnccdist + lagterrch + postcolonial + colonycontig + 
#                    onemp + twomp + defense + demdy + trival,
#                  data = eu3); summary(l0_cox_cw)
# cox.zph(l0_cox)

l0_cure_cw = tvcure(Surv(start, stop, fail) ~
                    leadchdy0 + independence + independencelnt + lcwany +
                    pchcaprat + bdymid + systchange + ww1 + ww2 + coldwar,
                  cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                    onemp + twomp + defense + demdy + trival,
                  data = eu3,
                  brglm = F, var = T, nboot = 30); summary(l0_cure_cw)

##### Solschange models -------------------------------------------------
# s0_cox = coxph(Surv(start, stop, fail) ~
#                    solschdy0 + independence +
#                   lnccdist + lagterrch + postcolonial + colonycontig + 
#                    onemp + twomp + defense + demdy + trival,
#                  data = eu3); summary(s0_cox)
# cox.zph(s0_cox)

s0_cure = tvcure(Surv(start, stop, fail) ~
                   solschdy0 + independence + independencelnt,
                   cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(s0_cure)

s0_cure = tvcure(Surv(start, stop, fail) ~
                   solschdy0 + independence + independencelnt,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(s0_cure)
s0_cure_tek = tvcure(Surv(start, stop, fail) ~
                   solschdy0 + independence + independencelnt + regtransdy,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival + tek,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(s0_cure)

### Add regtransdy variable
### Add civil war variable
s0_cox_cw = coxph(Surv(start, stop, fail) ~
                 solschdy0 + independence + lcwany +
                 lnccdist + lagterrch + postcolonial + colonycontig + 
                 onemp + twomp + defense + demdy + trival,
               data = eu3); summary(s0_cox)
cox.zph(s0_cox_cw)


s0_cure_cw = tvcure(Surv(start, stop, fail) ~
                   solschdy0 + independence + independencelnt + lcwany +
                   pchcaprat + bdymid + systchange + ww1 + ww2 + coldwar,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(s0_cure_cw)

eu3$polrel = as.numeric(eu3$polrel)
eusub = eu3 %>% filter(polrel == 1)
s0_cox_prd = coxph(Surv(start, stop, fail) ~
                     solschdy0 + lnccdist + independence + independencelnt + regtransdy + 
                     lagterrch + postcolonial + colonycontig + 
                     onemp + twomp + defense + demdy + trival,
                   data = eusub); summary(s0_cox_prd)
l0_cox_prd = coxph(Surv(start, stop, fail) ~
                     leadchdy0 + independence + independencelnt + regtransdy + 
                     lnccdist + lagterrch + postcolonial + colonycontig + 
                     onemp + twomp + defense + demdy + trival,
                   data = eusub); summary(l0_cox_prd)

eu3$independence = ifelse(eu3$statebirthyear1 == 1 | eu3$state\
                        )

# Add international shocks
# Results go away wrt contigous states (Mostly?)


##### TODO
# PH tests
# Interaction effects for leadership x solschange
# TEK Models for leadership change and solschange
# TEK Models for international
# Cubic splines
# Dpol
# Tek var is fucked up