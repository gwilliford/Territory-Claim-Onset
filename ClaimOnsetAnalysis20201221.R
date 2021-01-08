setwd("C:/Users/gwill/Dropbox/Research/Dissertation/chapter4")
library(readr)
library(dplyr)
library(tvcure)
library(compiler)
library(beepr)
options(scipen = 999)
cl <- makeCluster(4, "SOCK")
registerDoSNOW(cl)
cmpfun(tvcure)
enableJIT(3)

terrstart = read_csv("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/terrstart.csv")
terrstart$lnt = log(terrstart$stop)
terrstart$polrel = ifelse(terrstart$conttype < 6 | terrstart$onemp == 1 | terrstart$twomp == 1, 1, 0)

################################################################################
# Solschange models
################################################################################

sols_cox = coxph(Surv(start, stop, fail) ~
             solsch1 + lindependence + lregtrans +
               lnccdist + lagterrch + lagterrch:lnt + postcolonial + colonycontig +
             onemp + twomp + defense + demdy + demdy:lnt + trival,
           data = terrstart); sols_cox
cox.zph(sols_cox)

sols_cox = coxph(Surv(start, stop, fail) ~
                   solsch1 + lindependence + lregtrans + lcwany +
                   lnccdist + lagterrch + lagterrch:lnt + postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + demdy:lnt + trival,
                 data = terrstart, subset = terrstart$polrel == 1); sols_cox
cox.zph(sols_cox)


sols_cox = coxph(Surv(start, stop, fail) ~
                   solsch1 + lindependence + lregtrans +
                   lnccdist + lagterrch + lagterrch:lnt + postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + demdy:lnt,
                 data = terrstart, subset = terrstart$trival == 1); sols_cox
cox.zph(sols_cox)

s1_cure = tvcure(Surv(start, stop, fail) ~
                   solsch1 + lindependence + lregtrans,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + trival,
                 data = terrstart,
                 brglm = T, var = F)
saveRDS(s1_cure, "./models/s1_cure.RDS")

s1_cure = tvcure(Surv(start, stop, fail) ~
                   solsch1 + solsch1:lnt + lindependence + lregtrans + lcwany,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + trival,
                 data = terrstart,
                 brglm = T, var = T); summary(s1_cure)
s1_cure_sch = sch(s1_cure)
plotsch(s1_cure_sch, "solsch1")
plotsch(s1_cure_sch, "lindependenceTRUE")
plotsch(s1_cure_sch, "lregtrans")
plotsch(s1_cure_sch, "lcwany")

################################################################################
# International models 
################################################################################
set.seed(2973201)
icb_cure = tvcure(Surv(start, stop, fail) ~
                lpchcap + licb + systchange + ww1 + ww2 + coldwar,
              cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig +
                onemp + twomp + defense + demdy + trival,
              data = terrstart,
              brglm = F, var = T); summary(micb)

################################################################################
# MID Models
################################################################################
set.seed(294387)
mid_cure = tvcure(Surv(start, stop, fail) ~
                   lpchcap + lbdymid + systchange + ww1 + ww2 + coldwar,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + trival,
                 data = terrstart,
                 brglm = F, var = T); summary(i1_cure)


































##### PRD models -------------------------------------------------
s0_cox_prd = coxph(Surv(start, stop, fail) ~
                     solsch0 + independence + regtransdy +
                     lnccdist + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy + trival,
                   data = terrstart, subset = terrstart$polrel == 1); summary(s0_cox_prd)
s1_cox_prd = coxph(Surv(start, stop, fail) ~
                     solsch1 + lindependence + lregtransdy +
                     lnccdist + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy + trival,
                   data = terrstart, subset = terrstart$polrel == 1); summary(s1_cox_prd)
l0_cox_prd = coxph(Surv(start, stop, fail) ~
                     leadch0 + independence + independencelnt + regtransdy + regtransdylnt +
                     lnccdist + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy,
                   data = terrstart, subset = terrstart$polrel == 1); summary(l0_cox_prd)
l1_cox_prd = coxph(Surv(start, stop, fail) ~
                     leadch1 + lindependence + independencelnt + lregtransdy + regtransdylnt +
                     lnccdist + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy,
                   data = terrstart, subset = terrstart$polrel == 1); summary(l1_cox_prd)
s0_cox_riv = coxph(Surv(start, stop, fail) ~
                     solsch0 + independence + regtransdy +
                     lnccdist + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy,
                   data = terrstart, subset = terrstart$trival == 1); summary(s0_cox_riv)
s1_cox_riv = coxph(Surv(start, stop, fail) ~
                     solsch1 + lindependence + lregtransdy +
                     lnccdist + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy,
                   data = terrstart, subset = terrstart$trival == 1); summary(s1_cox_riv)
l0_cox_riv = coxph(Surv(start, stop, fail) ~
                     leadch0 + independence + independencelnt + regtransdy + regtransdylnt +
                     lnccdist + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy + trival,
                   data = terrstart, subset = terrstart$trival == 1); summary(l0_cox_riv)
l1_cox_riv = coxph(Surv(start, stop, fail) ~
                     leadch1 + lindependence + independencelnt + lregtransdy + regtransdylnt +
                     lnccdist + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy + trival,
                   data = terrstart, subset = terrstart$trival == 1); summary(l1_cox_riv)

##### Civwar models -------------------------------------------------
l0_cure_cw = tvcure(Surv(start, stop, fail) ~
                      leadch0 + independence + independencelnt + regtransdy + regtransdylnt + cwany +
                    cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig +
                      onemp + twomp + defense + demdy + trival,
                    data = terrstart,
                    brglm = F, var = T, nboot = 30); summary(l0_cure_cw)

s0_cure_cw = tvcure(Surv(start, stop, fail) ~
                      solsch0 + independence + independencelnt + regtransdy + regtransdylnt + cwany,
                    cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig +
                      onemp + twomp + defense + demdy + trival,
                    data = terrstart,
                    brglm = F, var = T, nboot = 30); summary(s0_cure_cw)
