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