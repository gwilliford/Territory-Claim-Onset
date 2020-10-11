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


eu3$polrel = eu3$majpower == 1 | eu3$contdir == 1
eu3$lnt = log(eu3$stop)
eu3$independencelnt = eu3$independence * eu3$lnt
eu3$lnccdistlnt = eu3$lnccdist * eu3$lnt
eu3$demdylnt = eu3$demdy * eu3$lnt
eu3$lagterrchlnt = eu3$lagterrch * eu3$lnt
eu3$TEK = ifelse(eu3$year < 1960, NA, eu3$TEK)

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

##### Leadership change cox models -------------------------------------------------

lc0 = coxph(Surv(start, stop, fail) ~ leadchdy0 + lcwany + independence + independencelnt + 
              lagterrch + lagterrchlnt + lnccdist + lnccdistlnt +
               majpower + defense + demdy + trival,
            data = eu3); summary(lc0)
lc1 = coxph(Surv(start, stop, fail) ~ leadchdy1 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3); summary(lc1)
lc2 = coxph(Surv(start, stop, fail) ~ leadchdy2 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3); summary(lc2)
lc012 = coxph(Surv(start, stop, fail) ~ leadchdy012 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3); summary(lc012)
cox.zph(lc0)
cox.zph(lc1)
cox.zph(lc2)
cox.zph(lc012)

##### Leadership change cox models -------------------------------------------------

lc0pr = coxph(Surv(start, stop, fail) ~ leadchdy0 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(lc0pr)
lc1pr = coxph(Surv(start, stop, fail) ~ leadchdy1 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(lc1pr)
lc2pr = coxph(Surv(start, stop, fail) ~ leadchdy2 + lcwany + independence + lagterrch + lnccdist + 
              majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(lc2pr)
lc012pr = coxph(Surv(start, stop, fail) ~ leadchdy012 + lcwany + independence + lagterrch + lnccdist + 
                majpower + defense + demdy + trival, data = eu3, subset = eu3$polrel == T); summary(lc012pr)

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
              majpower + defense + demdy + trival, data = eu3); summary(sc0)
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
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(i0)

i1 = tvcure(Surv(start, stop, fail) ~ 
              lpchcap + lbdymid + systchange + ww1 + ww2 + coldwar,
            cureform = ~ lagterrch + lnccdist + 
              majpower + defense + demdy + trival,
            data = eu3,
            brglm = F, var = T, nboot = 30); summary(i1)

##### TODO
# PH tests
# Interaction effects for leadership x solschange
# TEK Models for leadership change and solschange
# TEK Models for international
