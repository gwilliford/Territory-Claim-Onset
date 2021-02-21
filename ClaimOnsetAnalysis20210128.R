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

################################################################################
# Data management
################################################################################
terrstart = read_csv("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/terrstart.csv")
terrstart$lnt = log(terrstart$stop)
terrstart$solschanylnt = terrstart$solschany * terrstart$lnt
terrstart$licblnt = terrstart$licb * terrstart$lnt
terrstart$polrel = ifelse(terrstart$conttype < 6 | terrstart$onemp == 1 | terrstart$twomp == 1, 1, 0)
terrstart$cont5 = ifelse(terrstart$conttype < 6, 1, 0)
terrstart$endcoldwar = ifelse(terrstart$year == 1989, 1, 0)
terrstart$colrace = as.numeric(terrstart$year >= 1884 & terrstart$year <= 1894)
terrstart$decol = as.numeric(terrstart$year >= 1956 & terrstart$year <= 1962)
terrstart$solschany = as.numeric(terrstart$year >= 1859 & terrstart$year <= 1877)
terrstart$endcoldwar = as.numeric(terrstart$year >= 1989 & terrstart$year <= 1992)
# terrstart = terrstart %>%
#   arrange(dyad, year) %>%
#   group_by(dyad) %>%
#   mutate(solschany5 = as.numeric(solschany == 1 |
#                                     lag(solschany, 1) == 1 |
#                                     lag(solschany, 2) == 1 |
#                                     lag(solschany, 3) == 1 |
#                                     lag(solschany, 4) == 1 |
#                                     lag(solschany, 5) == 1), 
#          ww15 = as.numeric(ww1 == 1 |
#                                     lag(ww1, 1) == 1 |
#                                     lag(ww1, 2) == 1 |
#                                     lag(ww1, 3) == 1 |
#                                     lag(ww1, 4) == 1 |
#                                     lag(ww1, 5) == 1), 
#          ww25 = as.numeric(ww2 == 1 |
#                                     lag(ww2, 1) == 1 |
#                                     lag(ww2, 2) == 1 |
#                                     lag(ww2, 3) == 1 |
#                                     lag(ww2, 4) == 1 |
#                                     lag(ww2, 5) == 1), 
#          endcoldwar5 = as.numeric(endcoldwar == 1 |
#                                     lag(endcoldwar, 1) == 1 |
#                                     lag(endcoldwar, 2) == 1 |
#                                     lag(endcoldwar, 3) == 1 |
#                                     lag(endcoldwar, 4) == 1 |
#                                     lag(endcoldwar, 5) == 1))
terrstart = terrstart %>%
    arrange(dyad, year) %>%
    group_by(dyad) %>%
    mutate(solschany5 = as.numeric(solschany == 1 |
                                      lag(solschany, 1) == 1 |
                                      lag(solschany, 2) == 1 |
                                      lag(solschany, 3) == 1 |
                                      lag(solschany, 4) == 1 |
                                      lag(solschany, 5) == 1))

################################################################################
# KM Plot
################################################################################

plot(Surv(start, stop, fail) ~ 1, data = terrstart)

################################################################################
# Descriptive stats
################################################################################

table(terrstart$solschany, terrstart$fail)
216/(459335+216)
0.000470024
76/(140553+76)
0.0005404291

################################################################################
# Solschange models
################################################################################

# Cox models
sols_cox = coxph(Surv(start, stop, fail) ~
                   solschany1 + lindependence + lregtrans + lcowcwany +
                   contdir + lagterrch + postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + demdy:lnt + trival,
                 data = terrstart); sols_cox
cox.zph(sols_cox)

sols_cox_prd = coxph(Surv(start, stop, fail) ~
                   solschany1 + lindependence + lregtrans + lcowcwany +
                   contdir + lagterrch + postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + demdy:lnt + trival,
                 data = terrstart, subset = terrstart$polrel == 1); sols_cox_prd
cox.zph(sols_cox_prd)

ssols_cure_any = tvcure(Surv(start, stop, fail) ~
                         solschany1 + lindependence + lregtrans + lcowcwany,
                       cureform = ~ contdir + lagterrch +
                         postcolonial + colonycontig +
                         onemp + twomp + defense + demdy + trival,
                       data = terrstart,
                       brglm = T, var = T)
saveRDS(sols_cure_any, "./models/sols_cure_any.RDS")

sols_cure_any_lnt = tvcure(Surv(start, stop, fail) ~
                         solschany1 + solschany1:lnt + lindependence + lregtrans + lcowcwany,
                       cureform = ~ contdir + lagterrch +
                         postcolonial + colonycontig +
                         onemp + twomp + defense + demdy + trival,
                       data = terrstart,
                       brglm = T, var = T)
saveRDS(sols_cure_any_lnt, "./models/sols_cure_any_lnt.RDS")


sols_cure_reg = tvcure(Surv(start, stop, fail) ~
                         solsch1 + lindependence + lregtrans + lregtrans:lnt + lcowcwany,
                       cureform = ~ contdir + lagterrch +
                         postcolonial + colonycontig +
                         onemp + twomp + defense + demdy + trival,
                       data = terrstart,
                       brglm = T, var = T)
saveRDS(sols_cure_reg, "./models/sols_cure_reg.RDS")

tvtable(sols_cox, sols_cox_prd, sols_cure_any)
  
sols_cure_sch = sch(sols_cure_any)
plotsch(sols_cure_sch, "solschany1")
plotsch(sols_cure_sch, "lindependence")
plotsch(sols_cure_sch, "lregtrans")
plotsch(sols_cure_sch, "lcowcwany")


sols_cure_final = tvcure(Surv(start, stop, fail) ~
                         solsch1 + lindependence + lregtrans + lregtrans:lnt + lcowcwany + ww2 + endcoldwar,
                       cureform = ~ contdir + lagterrch +
                         postcolonial + colonycontig +
                         onemp + twomp + defense + demdy + trival,
                       data = terrstart,
                       brglm = T, var = T)
saveRDS(sols_cure_final, "./models/sols_cure_final.RDS")
sols_cure_final_sch = sch(sols_cure_final)
plotsch(sols_cure_final_sch, "solsch1")
plotsch(sols_cure_final_sch, "lindependence")
plotsch(sols_cure_final_sch, "ww2")
plotsch(sols_cure_final_sch, "endcoldwar")


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



sols_cure_min = tvcure(Surv(start, stop, fail) ~
                         solschmin1 + lindependence + lregtrans + lcowcwany,
                       cureform = ~ contdir + lagterrch +
                         postcolonial + colonycontig +
                         onemp + twomp + defense + demdy + trival,
                       data = terrstart,
                       brglm = T, var = T)
saveRDS(sols_cure_min, "./models/sols_cure_min.RDS")

################################################################################
# ICB models 
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
plotsch(icb_cure_test_sch, "solschanyTRUE")
plotsch(icb_cure_test_sch, "ww1TRUE")
plotsch(icb_cure_test_sch, "ww2TRUE")
plotsch(icb_cure_test_sch, "coldwarTRUE")

# Full model
icb_cox = coxph(Surv(start, stop, fail) ~
                   licb + lpchcap + ww2 + endcoldwar + lagterrch +
                   postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + trival,
                 data = terrstart)

icb_cox_prd = coxph(Surv(start, stop, fail) ~
                      licb + lpchcap + ww2 + endcoldwar + lagterrch +
                      postcolonial + colonycontig +
                      onemp + twomp + defense + demdy + trival,
                    data = terrstart, subset = terrstart$polrel == 1)

set.seed(2973201)
icb_cure = tvcure(Surv(start, stop, fail) ~
                    licb + lpchcap + ww2 + endcoldwar,
                  cureform = ~ contdir + lagterrch +
                    postcolonial + colonycontig +
                    onemp + twomp + defense + demdy + trival,
                  data = terrstart,
                  brglm = T, var = T)
saveRDS(icb_cure, "./models/icb_cure.RDS")

test = tvcure(Surv(start, stop, fail) ~ solschany + ww1 + ww2 + endcoldwar + colrace + decol,
              cureform = ~ demdy, data = terrstart, var = F, brglm = T)

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
# plotsch(icb_cure_sch, "solschanyTRUE")
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


################################################################################
# MID Models
################################################################################

mid_cox = coxph(Surv(start, stop, fail) ~
                  lbdymid + lpchcap + solschany + ww1 + ww2 + endcoldwar +
                  contdir + lagterrch +
                  postcolonial + colonycontig +
                  onemp + twomp + defense + demdy + trival,
                data = terrstart); mid_cox

mid_cox_prd = coxph(Surv(start, stop, fail) ~
                  lbdymid + lpchcap + solschany + ww1 + ww2 + endcoldwar +
                  contdir + lagterrch +
                  postcolonial + colonycontig +
                  onemp + twomp + defense + demdy + trival,
                data = terrstart, subset = terrstart$polrel == 1); mid_cox_prd

mid_cure = tvcure(Surv(start, stop, fail) ~
                    lbdymid + lpchcap,
                  cureform = ~ contdir + lagterrch +
                    postcolonial + colonycontig +
                    onemp + twomp + defense + demdy + trival,
                  data = terrstart,
                  brglm = T, var = T)
saveRDS(mid_cure, "./models/mid_cure.RDS")
mid_cure_sch = sch(mid_cure)
plotsch(mid_cure_sch, "lbdymid")


################################################################################
# Midonset models
################################################################################

midonset_cure = tvcure(Surv(start, stop, fail) ~
                    lbmidonset + lpchcap,
                  cureform = ~ contdir + lagterrch +
                    postcolonial + colonycontig +
                    onemp + twomp + defense + demdy + trival,
                  data = terrstart,
                  brglm = T, var = T, nboot = 30)
saveRDS(midonset_cure, "./models/midonset_cure.RDS")

set.seed(68198411)
fatonset_cure = tvcure(Surv(start, stop, fail) ~
                         lbfatonset + lpchcap,
                       cureform = ~ contdir + lagterrch +
                         postcolonial + colonycontig +
                         onemp + twomp + defense + demdy + trival,
                       data = terrstart,
                       brglm = T, var = T)
saveRDS(fatonset_cure, "./models/fatonset_cure.RDS")

set.seed(2928432)
fatonset_cure2 = tvcure(Surv(start, stop, fail) ~
                         lbfatonset + lpchcap + systchange + ww1 + ww2 + endcoldwar,
                       cureform = ~ contdir + lagterrch +
                         postcolonial + colonycontig +
                         onemp + twomp + defense + demdy + trival,
                       data = terrstart,
                       brglm = T, var = T)
saveRDS(fatonset_cure2, "./models/fatonset_cure2.RDS")
fatonset_cure2_sch = sch(fatonset_cure2)
plotsch(fatonset_cure2_sch, "lbfatonset")
plotsch(fatonset_cure2_sch, "lpchcap")
plotsch(fatonset_cure2_sch, "systchange")
plotsch(fatonset_cure2_sch, "ww1")
plotsch(fatonset_cure2_sch, "ww2")
plotsch(fatonset_cure2_sch, "endcoldwar")

set.seed(2349023)
fatonset_cure3 = tvcure(Surv(start, stop, fail) ~
                          lbfatonset + lpchcap,
                        cureform = ~ contdir + lagterrch +
                          postcolonial + colonycontig +
                          onemp + twomp + defense + demdy + trival + 
                          solschany + ww1 + ww2 + endcoldwar,
                        data = terrstart,
                        brglm = T, var = F)
saveRDS(fatonset_cure3, "./models/fatonset_cure3.RDS")
fatonset_cure3_sch = sch(fatonset_cure3)
plotsch(fatonset_cure3_sch, "lbfatonset")
plotsch(fatonset_cure3_sch, "lpchcap")

fatonset_cox = coxph(Surv(start, stop, fail) ~
                          lbfatonset + lpchcap + contdir + lagterrch +
                          postcolonial + colonycontig +
                          onemp + twomp + defense + demdy + trival + 
                          solschany + ww1 + ww2 + endcoldwar,
                        data = terrstart); summary(fatonset_cox)

fatonset_cox_prd = coxph(Surv(start, stop, fail) ~
                       lbfatonset + lpchcap + contdir + lagterrch +
                       postcolonial + colonycontig +
                       onemp + twomp + defense + demdy + trival + 
                       solschany + ww1 + ww2 + endcoldwar,
                     data = terrstart, subset = terrstart$polrel == 1); summary(fatonset_cox_prd)

waronset_cure = tvcure(Surv(start, stop, fail) ~
                         lbwaronset + lpchcap,
                       cureform = ~ contdir + lagterrch +
                         postcolonial + colonycontig +
                         onemp + twomp + defense + demdy + trival,
                       data = terrstart,
                       brglm = T, var = F)
saveRDS(waronset_cure, "./models/waronset_cure.RDS")
waronset_cure_sch = sch(waronset_cure)
plotsch(waronset_cure_sch, "lbwaronset")




set.seed(99815681)
icb_cure2 = tvcure(Surv(start, stop, fail) ~
                          licb + lpchcap + ww2 + endcoldwar,
                        cureform = ~ contdir + lagterrch +
                          postcolonial + colonycontig +
                          onemp + twomp + defense + demdy + trival,
                        data = terrstart,
                        brglm = T, var = T)
saveRDS(icb_cure2, "./models/icb_cure2.RDS")

################################################################################
# Fatal MID models
################################################################################

fat_cox = coxph(Surv(start, stop, fail) ~
                   lbdyfat + lpchcap + solschany + ww1 + ww2 + endcoldwar +
                   contdir + lagterrch +
                   postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + trival,
                 data = terrstart); fat_cox

fat_cox_prd = coxph(Surv(start, stop, fail) ~
                   lbdyfat + lpchcap + solschany + ww1 + ww2 + endcoldwar +
                   contdir + lagterrch +
                   postcolonial + colonycontig +
                   onemp + twomp + defense + demdy + trival,
                 data = terrstart, subset = terrstart$polrel == 1); fat_cox_prd

set.seed(239389239)
fat_cure = tvcure(Surv(start, stop, fail) ~
                    lbdyfat + lpchcap,
                  cureform = ~ contdir + lagterrch +
                    postcolonial + colonycontig +
                    onemp + twomp + defense + demdy + trival,
                  data = terrstart,
                  brglm = T, var = T)
saveRDS(fat_cure, "./models/fat_cure.RDS")
fat_cure_sch = sch(fat_cure)
plotsch(fat_cure_sch, "lbdyfat")

################################################################################
# War models
################################################################################

war_cox = coxph(Surv(start, stop, fail) ~
                  lbdywar + lpchcap + solschany + ww1 + ww2 + endcoldwar + 
                  contdir + lagterrch +
                  postcolonial + colonycontig +
                  onemp + twomp + defense + demdy + trival,
                data = terrstart); war_cox

war_cox_prd = coxph(Surv(start, stop, fail) ~
                  lbdywar + lpchcap + solschany + ww1 + ww2 + endcoldwar + 
                  contdir + lagterrch +
                  postcolonial + colonycontig +
                  onemp + twomp + defense + demdy + trival,
                data = terrstart, subset = terrstart$polrel == 1); war_cox_prd

set.seed(36543895)
war_cure = tvcure(Surv(start, stop, fail) ~
                    lbdywar + lpchcap + solschany + ww1 + ww2 + ,
                  cureform = ~ contdir + lagterrch +
                    postcolonial + colonycontig +
                    onemp + twomp + defense + demdy + trival,
                  data = terrstart,
                  brglm = T, var = T)
saveRDS(war_cure, "./models/war_cure.RDS")
war_cure_sch = sch(war_cure)
plotsch(war_cure_sch, "lbdywar")
plotsch(war_cure_sch, "lpchcap")

set.seed(8815941898)
war_cure_lnt = tvcure(Surv(start, stop, fail) ~
                    lbdywar + lbdywar:lnt + lpchcap + solschany + ww1 + ww2 + endcoldwar,
                  cureform = ~ contdir + lagterrch +
                    postcolonial + colonycontig +
                    onemp + twomp + defense + demdy + trival,
                  data = terrstart,
                  brglm = T, var = T)
saveRDS(war_cure_lnt, "./models/war_cure_lnt.RDS")
swar_cure_lnt_sch = sch(war_cure_lnt)
plotsch(war_cure_lnt_sch, "lbdywar")
plotsch(war_cure_lnt_sch, "lpchcap")


war_cure_shock = tvcure(Surv(start, stop, fail) ~
                    lbdywar + lpchcap + solschany + ww1 + ww2 + endcoldwar,
                  cureform = ~ contdir + lagterrch +
                    postcolonial + colonycontig +
                    onemp + twomp + defense + demdy + trival,
                  data = terrstart,
                  brglm = T, var = T)
saveRDS(war_cure_shock, "./models/war_cure_shock.RDS")
war_cure_shock_sch = sch(war_cure_shock)
plotsch(war_cure_shock_sch, "lbdywar")
plotsch(war_cure_shock_sch, "lpchcap")


################################################################################
# Tables
################################################################################
tvtable(sols_cox, sols_cox_prd, sols_cure_any)
tvtable(icb_cox, icb_cox_prd, icb_cure)
tvtable(mid_cox, mid_cox_prd, mid_cure)
tvtable(fat_cox, fat_cox_prd, fat_cure)
tvtable(war_cox, war_cox_prd, war_cure)







war_cure2 = tvcure(Surv(start, stop, fail) ~
                    lbdywar + lpchcap + lindependence + lcowcwany,
                  cureform = ~ contdir + lagterrch +
                    postcolonial + colonycontig +
                    onemp + twomp + defense + demdy + trival + 
                    solschany + ww1 + ww2 + endcoldwar,
                  data = terrstart,
                  brglm = T, var = T)
saveRDS(war_cure2, "./models/war_cure2.RDS")






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
