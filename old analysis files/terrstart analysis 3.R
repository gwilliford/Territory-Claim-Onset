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

##### Direct contiguity leaderchange cox models
  # Model 1 returns a significant result
c1 = coxph(Surv(start, stop, fail) ~ leadchdy0,
           data = eu3, subset = eu3$year > 1917 & eu3$conttype == 1); summary(c1)
c2 = coxph(Surv(start, stop, fail) ~ leadchdy1,
           data = eu3, subset = eu3$year > 1917 & eu3$conttype == 1); summary(c2)
c3 = coxph(Surv(start, stop, fail) ~ leadchdy2,
           data = eu3, subset = eu3$year > 1917 & eu3$conttype == 1); summary(c3)
c4 = coxph(Surv(start, stop, fail) ~ leadchdy012,
           data = eu3, subset = eu3$year > 1917 & eu3$conttype == 1); summary(c4)

##### Direct contiguity leaderchange cure models w/o brglm
t1 = tvcure(Surv(start, stop, fail) ~ lpchcap + leadchdy0, 
            cureform = ~ majpower + defense + ldemdy + igosum + lrival,
            data = eu3, subset = eu3$contdir == 1,
            brglm = F, var = T, nboot = 100); summary(t1)
t2 = tvcure(Surv(start, stop, fail) ~ lpchcap + leadchdy1, 
            cureform = ~ majpower + lnccdist + defense + ldemdy + igosum + lrival,
            data = eu3, subset = eu3$contdir == 1,
            brglm = F, var = T, nboot = 100); summary(t2)
t3 = tvcure(Surv(start, stop, fail) ~ lpchcap + leadchdy2, 
            cureform = ~ majpower + lnccdist + defense + ldemdy + igosum + lrival,
            data = eu3, subset = eu3$contdir == 1,
            brglm = F, var = T, nboot = 100); summary(t3)
t4 = tvcure(Surv(start, stop, fail) ~ lpchcap + leadchdy012, 
            cureform = ~ majpower + lnccdist + defense + ldemdy + igosum + lrival,
            data = eu3, subset = eu3$contdir == 1,
            brglm = F, var = T, nboot = 100); summary(t4)


#####
test = eu3 %>%
  filter(contdir == 1) %>%
  select(contdir, start, stop, fail, majpower, defense, demdy, igosum, lrival, pchcaprat, leadchdy0)
cor(as.matrix(test), use = "complete.obs")
test2 = na.omit(test)
summary(test)


##### Direct contiguity leaderchange cure models with brglm+
t1f = tvcure(Surv(start, stop, fail) ~ # GOod
               pchcaprat + 
               leadchdy0, 
             cureform = ~ majpower + defense + demdy + trival,
             data = eu3, subset = eu3$contdir == 1,
             brglm = F, var = T, nboot = 100); summary(t1f)
t1f = tvcure(Surv(start, stop, fail) ~ pchcaprat + lbdymid + # add lbdymid Good
               leadchdy0, 
             cureform = ~ majpower + defense + demdy + trival,
             data = eu3, subset = eu3$contdir == 1,
             brglm = F, var = T, nboot = 100); summary(t1f)
t1f = tvcure(Surv(start, stop, fail) ~ # add lagterrch - good
               leadchdy0 + 
               pchcaprat + lbdymid,
             cureform = ~ lagterrch + 
               majpower + defense + demdy + trival,
             data = eu3, subset = eu3$contdir == 1,
             brglm = F, var = T, nboot = 100); summary(t1f)
t1f = tvcure(Surv(start, stop, fail) ~ # add international shocks bad, add TEK bad
               leadchdy0 + 
               pchcaprat + lbdymid,
            cureform = ~ lagterrch + TEK + 
              majpower + defense + demdy + trival,
            data = eu3, subset = eu3$contdir == 1,
            brglm = F, var = T, nboot = 100); summary(t1f)



##### Indirect contiguity experimentation

t1f = tvcure(Surv(start, stop, fail) ~ # Drop TEK, full sample, add distance, 30 br reps - great
               leadchdy0 + 
               pchcaprat + lbdymid,
             cureform = ~ lagterrch + 
               lnccdist + 
               majpower + defense + demdy + trival,
             data = eu3,
             brglm = F, var = T, nboot = 30); summary(t1f)
t1f = tvcure(Surv(start, stop, fail) ~ # Add indepdencece and civil war controls - good
               leadchdy0 + lcwany + independence +
               pchcaprat + lbdymid,
             cureform = ~ lagterrch + 
               lnccdist + 
               majpower + defense + demdy + trival,
             data = eu3,
             brglm = F, var = T, nboot = 30); summary(t1f)
t1f = tvcure(Surv(start, stop, fail) ~ # Add systemic controls - woof
               leadchdy0 + lcwany + independence +
               pchcaprat + lbdymid +
               systchange + ww1 + ww2 + coldwar,
             cureform = ~ lagterrch + 
               lnccdist + 
               majpower + defense + demdy + trival,
             data = eu3,
             brglm = F, var = T, nboot = 30); summary(t1f)


##### Direct contiguity solschange cox models
c5 = coxph(Surv(start, stop, fail) ~ solschdy0,
           data = eu3, subset = eu3$year > 1917 & eu3$conttype == 1); summary(c5)
c6 = coxph(Surv(start, stop, fail) ~ solschdy1,
           data = eu3, subset = eu3$year > 1917 & eu3$conttype == 1); summary(c6)
c7 = coxph(Surv(start, stop, fail) ~ solschdy2,
           data = eu3, subset = eu3$year > 1917 & eu3$conttype == 1); summary(c7)
c8 = coxph(Surv(start, stop, fail) ~ solschdy012,
           data = eu3, subset = eu3$year > 1917 & eu3$conttype == 1); summary(c8)


##### Direct contiguity leaderchange cure models w/o brglm
t5 = tvcure(Surv(start, stop, fail) ~ lpchcap + solschdy0, 
            cureform = ~ majpower + lnccdist + defense + ldemdy + igosum + lrival,
            data = eu3, subset = eu3$contdir == 1,
            brglm = F, var = T, nboot = 100); summary(t5)
t6 = tvcure(Surv(start, stop, fail) ~ lpchcap + solschdy1, 
            cureform = ~ majpower + lnccdist + defense + ldemdy + igosum + lrival,
            data = eu3, subset = eu3$contdir == 1,
            brglm = F, var = T, nboot = 100); summary(t6)
t7 = tvcure(Surv(start, stop, fail) ~ lpchcap + solschdy2, 
            cureform = ~ majpower + lnccdist + defense + ldemdy + igosum + lrival,
            data = eu3, subset = eu3$contdir == 1,
            brglm = F, var = T, nboot = 100); summary(t7)
t8 = tvcure(Surv(start, stop, fail) ~ lpchcap + solschdy012, 
            cureform = ~ majpower + lnccdist + defense + ldemdy + igosum + lrival,
            data = eu3, subset = eu3$contdir == 1,
            brglm = F, var = T, nboot = 100); summary(t8)




