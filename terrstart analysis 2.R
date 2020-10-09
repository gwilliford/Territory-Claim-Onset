setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset")
library(readstata13)
library(dplyr)
library(tvcure)
library(compiler)
options(scipen = 999)
cl <- makeCluster(3, "SOCK")
registerDoSNOW(cl)
cmpfun(tvcure)
enableJIT(3)


a = tvcure(Surv(start, stop, fail) ~ lpchcap + lagsolsch + lcwany, 
             cureform = ~ majpower + ldemdy + lrival + lagdee,
             data = eu3, subset = eu3$conttype < 6 & eu3$year > 1917,
             brglm = T, 
             var = T, nboot = 100); summary(a)
b = tvcure(Surv(start, stop, fail) ~ lpchcap + lagsolsch, 
           cureform = ~ majpower + lnccdist + ldemdy + lrival,
           data = eu3, subset = eu3$year > 1917,
           var = T, nboot = 100); summary(b)
c = tvcure(Surv(start, stop, fail) ~ lpchcap + lagsolsch + lcwany, 
           cureform = ~ majpower + lnccdist + 
             defense + 
             ldemdy + igosum + 
             lagterrch + lrival + TEK,
           data = eu3, subset = eu3$year > 1917,
           var = T, nboot = 100); summary(c)
start = Sys.time()
start
d = tvcure(Surv(start, stop, fail) ~ lpchcap + lagsolsch + lcwany, 
           cureform = ~ majpower + lnccdist + 
             defense + 
             ldemdy + igosum + 
             lagterrch + lrival + TEK,
           data = eu3, subset = eu3$year > 1917,
           brglm = F, var = T, nboot = 100); summary(d)
end = Sys.time()
end
dur = end - start
# first run took 4 mins to go from start to initial estimates - only compile tvcure
# second run - compile all functions - 22:34:10 to ~ 22:38:30 - 6 mins, but compiling first - did not finish em by 22:55:20
# third run - no compilation - 22:47:47 - 22:42:45 ---- 5 mins
# Fourth run - brglm F with compilation - compiled in 15 seconds - em converged by 22:59:35

##### Full(ish) model
stopCluster(cl)
cl <- makeCluster(4, "SOCK")
registerDoSNOW(cl)

start = Sys.time()
start
e = tvcure(Surv(start, stop, fail) ~ lpchcap + solschany +
           lcwany + independence +
           systchange + ww1 + ww2 + coldwar, 
           cureform = ~ lnccdist + #terrchangedum + #TEK + 
             majpower + defense + demdy + igosum + trival,
             #tin,
           data = eu3, subset = eu3$year > 1917,
           brglm = T, var = T, nboot = 30); summary(e)
end = Sys.time()
end
dur = end - start
dur
# 15 mins for non brglm
# 23 mins for brglm
# 30 mins for brglm w/ international shocks

start = Sys.time()
start
f = tvcure(Surv(start, stop, fail) ~ lpchcap + solsch_tandlag +
             lcwany + independence +
             systchange + ww1 + ww2 + coldwar, 
           cureform = ~ lnccdist + #terrchangedum + #TEK + 
             majpower + defense + demdy + igosum + trival,
           #tin,
           data = eu3, subset = eu3$year > 1917,
           brglm = T, var = T, nboot = 30); summary(f)
end = Sys.time()
end
dur = end - start
dur

start = Sys.time()
start
g = tvcure(Surv(start, stop, fail) ~ lpchcap + leadtransany +
             lcwany + independence +
             systchange + ww1 + ww2 + coldwar, 
           cureform = ~ lnccdist + terrchangedum + #TEK + 
             majpower + defense + demdy + igosum + trival,
           #tin,
           data = eu3, subset = eu3$year > 1917,
           brglm = T, var = T, nboot = 30); summary(g)
end = Sys.time()
end
dur = end - start
dur
# w/out terrchange dum - 45 min
#### terrchange dum needs to be recoded
#### Cor matrix of shock variables

start = Sys.time()
start
h = tvcure(Surv(start, stop, fail) ~ lpchcap + as.numeric(solsch_tandlag) +
             lcwany + independence,# +
             #systchange + ww1 + ww2 + coldwar, 
           cureform = ~ lnccdist + #terrchangedum + #TEK + 
             majpower + defense + demdy + igosum + trival,
           #tin,
           data = eu3, subset = eu3$year > 1917 & eu3$conttype == 1,
           brglm = T, var = T, nboot = 30); summary(h)
end = Sys.time()
end
dur = end - start
dur

start = Sys.time()
start
i = tvcure(Surv(start, stop, fail) ~ lpchcap + lagleadch_chisols,
            # lcwany + independence,# +
           #systchange + ww1 + ww2 + coldwar, 
           cureform = ~ lnccdist + #terrchangedum + #TEK + 
             majpower + defense + demdy + igosum + trival,
           #tin,
           data = eu3, subset = eu3$conttype == 1,
           brglm = F, var = T, nboot = 30); summary(i)
end = Sys.time()
end
dur = end - start
dur


c = tvcure(Surv(start, stop, fail) ~ lpchcap + lagsolsch + lcwany, 
           cureform = ~ majpower + lnccdist + 
             defense + 
             ldemdy + igosum + lrival
             lagterrch + TEK,
           data = eu3, subset = eu3$year > 1917 & eu3$contdir == 1,
           var = T, nboot = 100); summary(c)
start = Sys.time()
start
d = tvcure(Surv(start, stop, fail) ~ lpchcap + lagsolsch + lcwany, 
           cureform = ~ majpower + lnccdist + 
             defense + 
             ldemdy + igosum + 
             lagterrch + lrival + TEK,
           data = eu3, subset = eu3$year > 1917,
           brglm = F, var = T, nboot = 100); summary(d)
end = Sys.time()
end
dur = end - start

j = tvcure(Surv(start, stop, fail) ~ lpchcap + solschany + solsch_tandlag + lcwany, 
           cureform = ~ majpower + lnccdist + 
             defense + 
             ldemdy + igosum + 
             lagterrch + lrival + TEK,
           data = eu3, subset = eu3$year > 1917,
           brglm = F, var = T, nboot = 100); summary(j)

sum(eu3$fail, na.rm = T)
table(eu3$leadtransany, eu3$fail)
table(eu3$solschany, eu3$fail)
table(eu3$lagsolsch, eu3$fail)

k = tvcure(Surv(start, stop, fail) ~ lpchcap + solschany, 
               cureform = ~ majpower + lnccdist + 
                 defense + 
                 ldemdy + igosum + lrival,
               data = eu3, subset = eu3$year > 1917,
               brglm = F, var = T, nboot = 100); summary(j)


rm(list=setdiff(ls(), c("eu3", "d", "e", "f", "g", "h", "i", "j")))
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset")
write_rds(d, "./mods/d.RDS")
write_rds(e, "./mods/e.RDS")
write_rds(f, "./mods/f.RDS")
write_rds(g, "./mods/g.RDS")
write_rds(h, "./mods/h.RDS")
write_rds(i, "./mods/i.RDS")
write_rds(j, "./mods/j.RDS")
write_rds(k, "./mods/k.RDS")

k = tvcure(Surv(start, stop, fail) ~ lpchcap + solschany, 
           cureform = ~ majpower + defense + ldemdy + igosum + lrival,
           data = eu3, subset = eu3$year > 1917 & eu3$conttype == 1,
           brglm = F, var = T, nboot = 100); summary(k)

+     lagterrch = lag(terrchangedum),
+     
  +     lagleadchdy1 = lag(leadchdy),
+     lagleadchdy2 = lag(leadchdy, 2),
+     lagsolschdy1 = lag(solschdy),
+     lagsolschdy2 = lag(solschdy, 2),
+     leadcht12 = leadchdy == T | lagleadchdy1 == T | lagleadchdy2 == T,
+     solscht12 = solschdy == T | lagsolschdy1 == T | lagsolschdy2 == T
