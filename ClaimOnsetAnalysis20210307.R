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

sols_cox_cw = coxph(Surv(start, stop, fail) ~
                        solsch1 + lindependence + lregtrans + lcowcwany + contdir + lagterrch +
                        postcolonial + colonycontig +
                        onemp + twomp + defense + demdy + trival,
                      data = terrstart)



