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

##### Leadership change cox models -------------------------------------------------

l0_cox = coxph(Surv(start, stop, fail) ~ leadchdy0 + lcwany + independence + independencelnt + 
                 lagterrch + lagterrchlnt + lnccdist + lnccdistlnt + postcolonial + colonycontig + 
                 majpower + defense + demdy + trival + tin,
               data = eu3); summary(l0_cox)
l1_cox = coxph(Surv(start, stop, fail) ~ leadchdy1 + lcwany + independence + independencelnt + 
                 lagterrch + lagterrchlnt + lnccdist + lnccdistlnt + 
                 majpower + defense + demdy + trival,
               data = eu3); summary(l1_cox)
l2_cox = coxph(Surv(start, stop, fail) ~ leadchdy2 + lcwany + independence + independencelnt + 
                 lagterrch + lagterrchlnt + lnccdist + lnccdistlnt +
                 majpower + defense + demdy + trival,
               data = eu3); summary(l2_cox)
l012_cox = coxph(Surv(start, stop, fail) ~ leadchdy012 + lcwany + independence + independencelnt + 
                   lagterrch + lagterrchlnt + lnccdist + lnccdistlnt + 
                   majpower + defense + demdy + trival, 
                 data = eu3); summary(l012_cox)
cox.zph(lc0)
cox.zph(lc1)
cox.zph(lc2)
cox.zph(lc012)

##### Leadership chane cure models -------------------------------------------------

l0_cure = tvcure(Surv(start, stop, fail) ~
              leadchdy0 + lcwany + independence,
            cureform = ~ lnccdist + onemp + twomp + lagterrch + 
              defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l0_cure)
# Add regtransvar, postcolonial, colonycontig
l0_cureb = tvcure(Surv(start, stop, fail) ~
                   leadchdy0 + independence + regtransshock,
                 cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                   onemp + twomp + defense + demdy + trival,
                 data = eu3,
                 brglm = F, var = T, nboot = 30); summary(l0_cureb)
# Add runsummid, igo, tin
l0_curec = tvcure(Surv(start, stop, fail) ~
                    leadchdy0 + independence + regtransshock,
                  cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                    onemp + twomp + defense + demdy + trival + runsummid + 
                    igosum + tin,
                  data = eu3,
                  brglm = F, var = T, nboot = 30); summary(l0_curec)
# Chane domestic prox vars
l0_cured = tvcure(Surv(start, stop, fail) ~
                    leadchdy0 + independence + regtransshock,
                  cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                    onemp + twomp + defense + demdy + trival + runsummid,
                  data = eu3,
                  brglm = F, var = T, nboot = 30); summary(l0_cured)
  # cwany, regrans, and indepdencend (no shock) cause problems
# Add tek
l0_curef = tvcure(Surv(start, stop, fail) ~
                    leadchdy0 + independence,
                  cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + tek +
                    onemp + twomp + defense + demdy + trival + runsummid,
                  data = eu3,
                  brglm = F, var = T, nboot = 30); summary(l0_curef)
# Contiguous dyads only
l0_cureg = tvcure(Surv(start, stop, fail) ~
                    leadchdy0 + independence + independencelnt + cwany + regtrans,
                  cureform = ~ lagterrch + 
                    onemp + twomp + defense + demdy + trival,
                  data = eu3, subset = eu3$contdir == 1,
                  brglm = F, var = T, nboot = 30); summary(l0_cureg)
# Contigous dyads w/o distance
l0_cureh = tvcure(Surv(start, stop, fail) ~
                    leadchdy0 + independence + regtransshock,
                  cureform = ~ lagterrch + postcolonial + colonycontig + tek +
                    onemp + twomp + defense + demdy + trival + runsummid,
                  data = eu3, subset = eu3$contdir == 1,
                  brglm = F, var = T, nboot = 30); summary(l0_cureh)


l0_curej = tvcure(Surv(start, stop, fail) ~
                    leadchdy0 + independence + independencelnt +,
                  cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + tek + 
                    onemp + twomp + defense + demdy + trival,
                  data = eu3,
                  brglm = F, var = T, nboot = 30); summary(l0_curej)

l0_curek = tvcure(Surv(start, stop, fail) ~
                    leadchdy0 + independence + independencelnt + 
                    pchcaprat + bdymid + systchange + ww1 + ww2 + coldwar,
                  cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                    onemp + twomp + defense + demdy + trival,
                  data = eu3,
                  brglm = F, var = T, nboot = 30); summary(l0_curek)

l0_curel = tvcure(Surv(start, stop, fail) ~
                    leadchdy0 + independence + independencelnt + lcwany +
                    pchcaprat + bdymid + systchange + ww1 + ww2 + coldwar,
                  cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                    onemp + twomp + defense + demdy + trival,
                  data = eu3,
                  brglm = F, var = T, nboot = 30); summary(l0_curel)
l0_curem = tvcure(Surv(start, stop, fail) ~
                    pchcaprat + bdymid + systchange + ww1 + ww2 + coldwar,
                  cureform = ~ lnccdist + lagterrch + postcolonial + colonycontig + 
                    onemp + twomp + defense + demdy + trival,
                  data = eu3,
                  brglm = F, var = T, nboot = 30); summary(l0_curem)

# Add international shocks
# No tek, igosum, tin, regtrans (miscoded), runsummid
# Others not included: postcolonialism, LOST
# Results go away wrt contigous states (Mostly?)


# international stakes
l1_cure = tvcure(Surv(start, stop, fail) ~
              leadchdy1 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l1_cure)

l2_cure = tvcure(Surv(start, stop, fail) ~ 
              leadchdy2 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l2_cure)

l012_cure = tvcure(Surv(start, stop, fail) ~ 
              leadchdy012 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l012_cure)

hist(eu3$ccdistance)
##### Leadership change cox models with PRD -------------------------------------------------

l0_cox_pr = coxph(Surv(start, stop, fail) ~ leadchdy0 + lcwany + independence + lagterrch + postcolonial + colonycontig + lnccdist + 
                onemp + twomp + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(l0_cox_pr)
l1_cox_pr = coxph(Surv(start, stop, fail) ~ leadchdy1 + lcwany + independence + lagterrch + lnccdist + 
                majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(l1_cox_pr)
l2_cox_pr = coxph(Surv(start, stop, fail) ~ leadchdy2 + lcwany + independence + lagterrch + lnccdist + 
                majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(l2_cox_pr)
l012_cox_pr = coxph(Surv(start, stop, fail) ~ leadchdy012 + lcwany + independence + lagterrch + lnccdist + 
                  majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(l012_cox_pr)

##### Leadership change models w/ph -------------------------------------------------

l0zph = tvcure(Surv(start, stop, fail) ~
              leadchdy0 + lcwany + independence + independencelnt,
            cureform = ~ lagterrch + lagterrchlnt + lnccdist + lnccdistlnt + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l0zph)
l0zphb = tvcure(Surv(start, stop, fail) ~
                 leadchdy0 + lcwany + independence + independencelnt,
               cureform = ~ lagterrch + lnccdist + 
                 majpower + defense + demdy + trival,
               data = eu3,
               brglm = F, var = T, nboot = 30); summary(l0zphb)


l1zph = tvcure(Surv(start, stop, fail) ~
              leadchdy1 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l1zph)

l2zph = tvcure(Surv(start, stop, fail) ~ 
              leadchdy2 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l2zph)

l012zph = tvcure(Surv(start, stop, fail) ~ 
                leadchdy012 + lcwany + independence,
              cureform = ~ lagterrch + lnccdist + 
                majpower + defense + demdy + trival,
              data = eu3,
              brglm = F, var = T, nboot = 30); summary(l012zph)

##### Leadership change models with tek-------------------------------------------------
### check and make sure that this is only picking up years after 1960 - 1 only after 1960, 0s back to 46
l0tek = tvcure(Surv(start, stop, fail) ~
              leadchdy0 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + TEK +
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l0tek)

l1tek = tvcure(Surv(start, stop, fail) ~
              leadchdy1 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + TEK +
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l1tek)

l2tek = tvcure(Surv(start, stop, fail) ~ 
              leadchdy2 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + TEK +
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(l2tek)

l012tek = tvcure(Surv(start, stop, fail) ~ 
                leadchdy012 + lcwany + independence,
              cureform = ~ lagterrch + lnccdist + TEK +
                majpower + defense + demdy + trival,
              data = eu3,
              brglm = F, var = T, nboot = 30); summary(l012tek)



##### solschange cure models --------------------------------------------------------

s0 = tvcure(Surv(start, stop, fail) ~ 
              solschdy0 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(s0)

s1 = tvcure(Surv(start, stop, fail) ~ 
              solschdy1 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(s1)

s2 = tvcure(Surv(start, stop, fail) ~ 
              solschdy2 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(s2)

s012 = tvcure(Surv(start, stop, fail) ~ 
                solschdy012 + lcwany + independence,
              cureform = ~ lagterrch + lnccdist + 
                majpower + defense + demdy + trival,
              data = eu3,
              brglm = F, var = T, nboot = 30); summary(s012)

##### solschange cure models with prds --------------------------------------------------------

s0pr = tvcure(Surv(start, stop, fail) ~ 
              solschdy0 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3, subset = eu3$polrel == T,
            brglm = F, var = T, nboot = 30); summary(s0)

s1pr = tvcure(Surv(start, stop, fail) ~ 
              solschdy1 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3, subset = eu3$polrel == T,
            brglm = F, var = T, nboot = 30); summary(s1)

s2pr = tvcure(Surv(start, stop, fail) ~ 
              solschdy2 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3, subset = eu3$polrel == T,
            brglm = F, var = T, nboot = 30); summary(s2)

s012pr = tvcure(Surv(start, stop, fail) ~ 
                solschdy012 + lcwany + independence,
              cureform = ~ lagterrch + lnccdist + 
                majpower + defense + demdy + trival,
              data = eu3, subset = eu3$polrel == T,
              brglm = F, var = T, nboot = 30); summary(s012)

##### Solschange cox models -------------------------------------------------

sc0 = coxph(Surv(start, stop, fail) ~ solschdy0 + lcwany + independence + independencelnt +
              lagterrch + lagterrchlnt + lnccdist + lnccdistlnt +
              onemp + twomp + defense + demdy + trival, data = eu3); summary(sc0)
sc1 = coxph(Surv(start, stop, fail) ~ solschdy1 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3); summary(sc1)
sc2 = coxph(Surv(start, stop, fail) ~ solschdy2 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3); summary(sc2)
sc012 = coxph(Surv(start, stop, fail) ~ solschdy012 + lcwany + independence + lagterrch + lnccdist + 
                majpower + defense + demdy + trival, data = eu3); summary(sc012)
cox.zph(sc0)

##### Solschange cox models with prds ------------------------------------------

sc0pr = coxph(Surv(start, stop, fail) ~ solschdy0 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(sc0pr)
sc1pr = coxph(Surv(start, stop, fail) ~ solschdy1 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(sc1pr)
sc2pr = coxph(Surv(start, stop, fail) ~ solschdy2 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(sc2pr)
sc012pr = coxph(Surv(start, stop, fail) ~ solschdy012 + lcwany + independence + lagterrch + lnccdist + 
                majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(sc012pr)

##### Interaction models -------------------------------------------------------

int0 = tvcure(Surv(start, stop, fail) ~ 
              leadchdy0 * solschdy0 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(int0)

int1 = tvcure(Surv(start, stop, fail) ~ 
              leadchdy1 * solschdy1 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(int1)

int2 = tvcure(Surv(start, stop, fail) ~ 
              leadchdy2 * solschdy2 + lcwany + independence,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(int2)

int012 = tvcure(Surv(start, stop, fail) ~ 
                leadchdy012 * solschdy012 + lcwany + independence,
              cureform = ~ lagterrch + lnccdist + 
                majpower + defense + demdy + trival,
              data = eu3,
              brglm = F, var = T, nboot = 30); summary(int012)

##### International models -----------------------------------------------------

i0 = tvcure(Surv(start, stop, fail) ~ 
              pchcaprat + bdymid + systchange + ww1 + ww2 + coldwar,
            cureform = ~ lagterrch + lnccdist + 
              onemp + twomp + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(i0)

i1 = tvcure(Surv(start, stop, fail) ~ 
              lpchcap + lbdymid + systchange + ww1 + ww2 + coldwar,
            cureform = ~ lagterrch + lnccdist + 
              onemp + twomp + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(i1)

##### TODO
# PH tests
# Interaction effects for leadership x solschange
# TEK Models for leadership change and solschange
# TEK Models for international
# Cubic splines
# Dpol