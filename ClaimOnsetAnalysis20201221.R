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
terrstart$systchangelnt = terrstart$systchange * terrstart$lnt
terrstart$licblnt = terrstart$licb * terrstart$lnt
terrstart$polrel = ifelse(terrstart$conttype < 6 | terrstart$onemp == 1 | terrstart$twomp == 1, 1, 0)
terrstart$cont5 = ifelse(terrstart$conttype < 6, 1, 0)
terrstart$endcoldwar = ifelse(terrstart$year == 1989, 1, 0)

################################################################################
# KM Plot
################################################################################

plot(Surv(start, stop, fail) ~ 1, data = terrstart)

################################################################################
# Leadership change models
################################################################################
# Cure PH Test
lead_cure_test = tvcure(Surv(start, stop, fail) ~
                          leadchange + lindependence + lregtrans,
                        cureform = ~ contdir + lagterrch + postcolonial + colonycontig +
                          onemp + twomp + defense + demdy + trival,
                        data = terrstart,
                        brglm = T, var = F)
lead_cure_test_sch = sch(lead_cure_test)
plotsch(lead_cure_test_sch, "solsch1")
plotsch(lead_cure_test_sch, "lindependence")
plotsch(lead_cure_test_sch, "lregtrans")

# Full model
set.seed(9871643)
sols_cure = tvcure(Surv(start, stop, fail) ~
                     solsch1 + lindependence + lregtrans,
                   cureform = ~ contdir + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy + trival,
                   data = terrstart,
                   brglm = T, var = T)


################################################################################
# Solschange models
################################################################################

# Cox models
sols_cox = coxph(Surv(start, stop, fail) ~
                   solschany + lindependence + lregtrans + lcowcwany +
                   contdir + lagterrch + postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + demdy:lnt + trival,
                 data = terrstart); sols_cox
cox.zph(sols_cox)

sols_cox_prd = coxph(Surv(start, stop, fail) ~
                   solschany + lindependence + lregtrans + lcowcwany +
                   contdir + lagterrch + postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + demdy:lnt + trival,
                 data = terrstart, subset = terrstart$polrel == 1); sols_cox_prd
cox.zph(sols_cox_prd)


tvtable(sols_cox, sols_cox_prd, sols_cure_any)



sols_cure_sch = sch(sols_cure_any)
plotsch(sols_cure_sch, "solschany1")
plotsch(sols_cure_sch, "lindependence")
plotsch(sols_cure_sch, "lregtrans")
plotsch(sols_cure_sch, "lcowcwany")

# Cure model -------------------------------------------------------------------

# PH Diagnostics 
sols_cure_test = tvcure(Surv(start, stop, fail) ~
                          solsch1 + lindependence + lregtrans + lcowcwany,
                        cureform = ~ contdir + lagterrch + postcolonial + colonycontig +
                          onemp + twomp + defense + demdy + trival,
                        data = terrstart,
                        brglm = T, var = F)
sols_cure_test_sch = sch(sols_cure_test)
plotsch(sols_cure_test_sch, "solsch1")
plotsch(sols_cure_test_sch, "lindependenceTRUE")
plotsch(sols_cure_test_sch, "lregtrans")
plotsch(sols_cure_test_sch, "lcowcwany")

# Full model
set.seed(9871643)
sols_cure_full = tvcure(Surv(start, stop, fail) ~
                          solsch1 + lindependence + lregtrans,
                        cureform = ~ contdir + lagterrch +
                          postcolonial + colonycontig +
                          onemp + twomp + defense + demdy + trival,
                        data = terrstart,
                        brglm = T, var = T)
saveRDS(sols_cure_full, "./models/sols_cure_full.RDS")
sols_cure_sch = sch(sols_cure_full)
plotsch(sols_cure_sch, "solsch1")
plotsch(sols_cure_sch, "lindependenceTRUE")
plotsch(sols_cure_sch, "lregtrans")
plotsch(sols_cure_sch, "lcowcwany")

# Survival plots
sols_nx = apply(sols_cure_cw$X, 2, median)
sols_nx = rbind(sols_nx, sols_nx)
sols_nx[2, "solsch1"] = 1

sols_nz = apply(sols_cure_cw$Z, 2, median)
sols_nz = rbind(sols_nz, sols_nz)
sols_nz[2, "(Intercept)"] = sols_cure_cw$gamma["(Intercept)"]

sols_pred = tvcure::predict_tvcure_noci(sols_cure_cw, insamp = F, newX = sols_nx, newZ = sols_nz)
survplot(sols_pred, "suncure")
survplot(sols_pred, "spop")

# plot(-log(sols_pred$suncure[, 1]) ~ sols_pred$failtime, type = "l")
# lines(-log(sols_pred$suncure[, 2]) ~ sols_pred$failtime, lty = 2)

set.seed(9871643)
sols_cure_cw = tvcure(Surv(start, stop, fail) ~
                          solsch1 + lindependence + lregtrans + lcowcwany,
                        cureform = ~ contdir + lagterrch +
                          postcolonial + colonycontig +
                          onemp + twomp + defense + demdy + trival,
                        data = terrstart,
                        brglm = T, var = T)
saveRDS(sols_cure_cw, "./models/sols_cure_cw.RDS")
sols_cure_cw_sch = sch(sols_cure_cw)
plotsch(sols_cure_cw_sch, "solsch1")
plotsch(sols_cure_cw_sch, "lindependenceTRUE")
plotsch(sols_cure_cw_sch, "lregtrans")
plotsch(sols_cure_cw_sch, "lcowcwany")

a = coxph(Surv(start, stop, fail) ~
         solschany1 + lindependence + lregtrans + lcowcwany + contdir + lagterrch +
         postcolonial + colonycontig +
         onemp + twomp + defense + demdy + trival,
       data = terrstart)
b = coxph(Surv(start, stop, fail) ~
            solschany1 + lindependence + lregtrans + lcowcwany + lagterrch +
            postcolonial + colonycontig +
            onemp + twomp + defense + demdy + trival,
          data = terrstart, subset = terrstart$contdir == 1)
d = coxph(Surv(start, stop, fail) ~
            solschany1 + lindependence + lregtrans + lcowcwany + contdir + lagterrch +
            postcolonial + colonycontig +
            onemp + twomp + defense + demdy,
          data = terrstart, subset = terrstart$trival == 1)

sols_cure_any = tvcure(Surv(start, stop, fail) ~
                        solschany1 + lindependence + lregtrans + lcowcwany,
                      cureform = ~ contdir + lagterrch +
                        postcolonial + colonycontig +
                        onemp + twomp + defense + demdy + trival,
                      data = terrstart,
                      brglm = T, var = T)
saveRDS(sols_cure_any, "./models/sols_cure_any.RDS")


sols_cure_min = tvcure(Surv(start, stop, fail) ~
                         solschmin1 + lindependence + lregtrans + lcowcwany,
                       cureform = ~ contdir + lagterrch +
                         postcolonial + colonycontig +
                         onemp + twomp + defense + demdy + trival,
                       data = terrstart,
                       brglm = T, var = T)
saveRDS(sols_cure_min, "./models/sols_cure_min.RDS")

################################################################################
# International models 
################################################################################

# PH Test
set.seed(2973201)
icb_cure_test = tvcure(Surv(start, stop, fail) ~
                licb + lrivstart + lpchcap + ww2 + endcoldwar,
              cureform = ~ contdir + lagterrch + postcolonial + colonycontig +
                onemp + twomp + defense + demdy + trival,
              data = terrstart,
              brglm = T, var = F)
icb_cure_test_sch = sch(icb_cure_test)
plotsch(icb_cure_test_sch, "licb")
plotsch(icb_cure_test_sch, "lpchcap")
plotsch(icb_cure_test_sch, "systchangeTRUE")
plotsch(icb_cure_test_sch, "ww1TRUE")
plotsch(icb_cure_test_sch, "ww2TRUE")
plotsch(icb_cure_test_sch, "coldwarTRUE")

# Full model
icb_cox = coxph(Surv(start, stop, fail) ~
                   licb + lpchcap + ww2 + endcoldwar + contdir + lagterrch +
                   postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + trival,
                 data = terrstart)

set.seed(2973201)
icb_cure = tvcure(Surv(start, stop, fail) ~
                    licb + lpchcap + ww2 + endcoldwar,
                  cureform = ~ contdir + lagterrch +
                    postcolonial + colonycontig +
                    onemp + twomp + defense + demdy + trival,
                  data = terrstart,
                  brglm = T, var = T)
saveRDS(icb_cure, "./models/icb_cure.RDS")

mid_cure = tvcure(Surv(start, stop, fail) ~
                    lbdymid + lpchcap,
                  cureform = ~ contdir + lagterrch +
                    postcolonial + colonycontig +
                    onemp + twomp + defense + demdy + trival,
                  data = terrstart,
                  brglm = T, var = T)
saveRDS(mid_cure, "./models/mid_cure.RDS")




# Survival plots
icb_nx = apply(icb_cure_nomid$X, 2, median)
icb_nx = rbind(icb_nx, icb_nx)
icb_nx[2, "licb"] = 1

icb_nz = apply(icb_cure_nomid$Z, 2, median)
icb_nz = rbind(icb_nz, icb_nz)
icb_nz[2, "(Intercept)"] = icb_cure_nomid$gamma["(Intercept)"]

icb_pred = tvcure::predict_tvcure_noci(icb_cure_nomid, insamp = F, newX = icb_nx, newZ = icb_nz)
survplot(icb_pred, "suncure") # looks better than spop


icb_cure_sch = sch(icb_cure_nomid)
plotsch(icb_cure_sch, "licb")
plotsch(icb_cure_sch, "lpchcap")
# plotsch(icb_cure_sch, "systchangeTRUE")
plotsch(icb_cure_sch, "ww1TRUE")
plotsch(icb_cure_sch, "ww2TRUE")
plotsch(icb_cure_sch, "coldwarTRUE")




set.seed(2973201)
icb_cure_cont = tvcure(Surv(start, stop, fail) ~
                          licb + lpchcap + ww2 + endcoldwar,
                        cureform = ~ contdir + lagterrch +
                          postcolonial + colonycontig +
                          onemp + twomp + defense + demdy + trival,
                        data = terrstart,
                        brglm = T, var = T)
saveRDS(icb_cure_nomid, "./models/icb_cure.RDS")


icb_cont = tvcure(Surv(start, stop, fail) ~
                         lbdymid + lpchcap + ww2 + endcoldwar,
                       cureform = ~ onemp + twomp + defense + demdy + trival,
                       data = terrstart, subset = terrstart$contdir == 1,
                       brglm = T, var = T)
saveRDS(icb_cure_nomid, "./models/icb_cure.RDS")

sols_cont = tvcure(Surv(start, stop, fail) ~
                    solsch1 + lindependence + lregtrans + lcowcwany,
                  cureform = ~ lagterrch +
                    postcolonial + 
                    onemp + twomp + defense + demdy + trival,
                  data = terrstart, subset = terrstart$contdir == 1,
                  brglm = T, var = T)

prediction4(sols_cont, "contdir", c(0, 1))



























##### PRD models -------------------------------------------------
s0_cox_prd = coxph(Surv(start, stop, fail) ~
                     solsch0 + independence + regtransdy +
                     contdir + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy + trival,
                   data = terrstart, subset = terrstart$polrel == 1); summary(s0_cox_prd)
s1_cox_prd = coxph(Surv(start, stop, fail) ~
                     solsch1 + lindependence + lregtransdy +
                     contdir + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy + trival,
                   data = terrstart, subset = terrstart$polrel == 1); summary(s1_cox_prd)
l0_cox_prd = coxph(Surv(start, stop, fail) ~
                     leadch0 + independence + independencelnt + regtransdy + regtransdylnt +
                     contdir + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy,
                   data = terrstart, subset = terrstart$polrel == 1); summary(l0_cox_prd)
l1_cox_prd = coxph(Surv(start, stop, fail) ~
                     leadch1 + lindependence + independencelnt + lregtransdy + regtransdylnt +
                     contdir + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy,
                   data = terrstart, subset = terrstart$polrel == 1); summary(l1_cox_prd)
s0_cox_riv = coxph(Surv(start, stop, fail) ~
                     solsch0 + independence + regtransdy +
                     contdir + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy,
                   data = terrstart, subset = terrstart$trival == 1); summary(s0_cox_riv)
s1_cox_riv = coxph(Surv(start, stop, fail) ~
                     solsch1 + lindependence + lregtransdy +
                     contdir + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy,
                   data = terrstart, subset = terrstart$trival == 1); summary(s1_cox_riv)
l0_cox_riv = coxph(Surv(start, stop, fail) ~
                     leadch0 + independence + independencelnt + regtransdy + regtransdylnt +
                     contdir + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy + trival,
                   data = terrstart, subset = terrstart$trival == 1); summary(l0_cox_riv)
l1_cox_riv = coxph(Surv(start, stop, fail) ~
                     leadch1 + lindependence + independencelnt + lregtransdy + regtransdylnt +
                     contdir + lagterrch + postcolonial + colonycontig +
                     onemp + twomp + defense + demdy + trival,
                   data = terrstart, subset = terrstart$trival == 1); summary(l1_cox_riv)

##### Civwar models -------------------------------------------------
l0_cure_cw = tvcure(Surv(start, stop, fail) ~
                      leadch0 + independence + independencelnt + regtransdy + regtransdylnt + cwany +
                    cureform = ~ contdir + lagterrch + postcolonial + colonycontig +
                      onemp + twomp + defense + demdy + trival,
                    data = terrstart,
                    brglm = F, var = T, nboot = 30); summary(l0_cure_cw)

s0_cure_cw = tvcure(Surv(start, stop, fail) ~
                      solsch0 + independence + independencelnt + regtransdy + regtransdylnt + cwany,
                    cureform = ~ contdir + lagterrch + postcolonial + colonycontig +
                      onemp + twomp + defense + demdy + trival,
                    data = terrstart,
                    brglm = F, var = T, nboot = 30); summary(s0_cure_cw)
