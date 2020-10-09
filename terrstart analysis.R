setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset")
library(readstata13)
library(dplyr)
library(tvcure)
options(scipen = 999)
cl <- makeCluster(3, "SOCK")
registerDoSNOW(cl)

##### using terrgen data -------------------------------------------------------------------------

terrstart = read.dta13("terrstart.dta")

a = tvcure(Surv(start, stop, fail) ~ dcaprat, 
       cureform = ~ contdir + majpower + lcaprat + ldemdy + maxdep + lally,
       data = terrstart,
       brglm = T, 
       var = T, nboot = 100); summary(a)
b = tvcure(Surv(start, stop, fail) ~ dcaprat, 
           cureform = ~ contdir + majpower + lcaprat + ldemdy + maxdep + lally + kgd_rivalry_year,
           data = terrstart,
           brglm = T, 
           var = T, nboot = 100); summary(b)
c = tvcure(Surv(start, stop, fail) ~ dcaprat, 
           cureform = ~ contdir + majpower + lcaprat + ldemdy + maxdep +
             lally + lrival,
           data = terrstart,
           brglm = T, 
           var = T, nboot = 100); summary(c)
d = coxph(Surv(start, stop, fail) ~ dcaprat + contdir + majpower + lcaprat + ldemdy + maxdep + lally + lrival, data = terrstart); summary(d)


##### using eu data -------------------------------------------------------------------------
eu3 = read.dta13("./data/eu3.dta")
eu3 = eu3 %>% mutate(
  lagsolsch = lag(solschany),
  lagleadch_chisols = lag(leadtransany)
)

e = tvcure(Surv(start, stop, fail) ~ pchcaprat + leadtransany + solschany, 
           cureform = ~ contig + lcaprat + ldemdy + lally, # majpower, interdependence, rival
           data = eu3,
           brglm = T, 
           var = T, nboot = 100); summary(e)
f = tvcure(Surv(start, stop, fail) ~ pchcaprat + leadtransany + solschany, 
           cureform = ~ contig + lcaprat + ldemdy + lally + lrival,
           data = eu3,
           brglm = T, 
           var = T, nboot = 100); summary(f)
g = tvcure(Surv(start, stop, fail) ~ lpchcap + leadtransany + solschany + lcwany, 
           cureform = ~ contig + lcaprat + ldemdy + lally + lrival,
           data = eu3,
           brglm = T, 
           var = T, nboot = 100); summary(g)
h = tvcure(Surv(start, stop, fail) ~ lpchcap + leadtransany + solschany + lcwany, 
           cureform = ~ lcaprat + ldemdy + lally + lrival,
           data = eu3, subset = eu3$contdir == 1,
           brglm = T, 
           var = T, nboot = 100); summary(h)
i = tvcure(Surv(start, stop, fail) ~ lpchcap + leadtransany + lcwany, 
           cureform = ~ lcaprat + ldemdy + lally + lrival,
           data = eu3, subset = eu3$contdir == 1,
           brglm = T, 
           var = T, nboot = 100); summary(i)
j = tvcure(Surv(start, stop, fail) ~ lpchcap + lagsolsch + lcwany, 
               cureform = ~ lcaprat + majpower + ldemdy + lally + lrival,
               data = eu3, subset = eu3$contdir == 1,
               brglm = T, 
               var = T, nboot = 100); summary(j)
k = tvcure(Surv(start, stop, fail) ~ lpchcap + lagsolsch + lcwany, 
           cureform = ~ contig + lcaprat + majpower + ldemdy + lally + lrival,
           data = eu3, subset = eu3$contdir == 1,
           brglm = T, 
           var = T, nboot = 100); summary(k)
l = tvcure(Surv(start, stop, fail) ~ lpchcap + lagleadch_chisols + lcwany, 
           cureform = ~ contig + lcaprat + majpower + ldemdy + lally + lrival,
           data = eu3, 
           brglm = T, 
           var = T, nboot = 100); summary(l)
m = tvcure(Surv(start, stop, fail) ~ lpchcap + lcaprat + lagleadch_chisols + lcwany, 
           cureform = ~ contig + majpower + lsamereg + lally + lrival,
           data = eu3, 
           brglm = T, 
           var = T, nboot = 100); summary(m)
n = tvcure(Surv(start, stop, fail) ~ lpchcap + lagleadch_chisols + lcwany, 
           cureform = ~ majpower + lsamereg + lally + lrival,
           data = eu3, subset = eu3$contdir == 1, 
           brglm = T, 
           var = T, nboot = 100); summary(n)
o = tvcure(Surv(start, stop, fail) ~ lpchcap + lagleadch_chisols + lcwany, 
           cureform = ~ contig + majpower + lsamereg + lally + lrival,
           data = eu3, 
           brglm = T, 
           var = T, nboot = 100); summary(o)
p = tvcure(Surv(start, stop, fail) ~ lpchcap + lagleadch_chisols + lcwany, 
           cureform = ~ contig + majpower + lsamereg + lally + lrival + lagdee2,
           data = eu3, 
           brglm = T, 
           var = T, nboot = 100); summary(p)


##### using newgene data -------------------------------------------------------------------------
rm(list=setdiff(ls(), "eu3"))
eu3 = read.dta13("./data/eu3.dta")
eu3 = eu3 %>% mutate(
  lagsolsch = lag(solschany),
  lagleadch_chisols = lag(leadtransany)
)

# Norms variables
eu3$tin = eu3$year > 1944

# International shock variables
eu3$ww1 = eu3$year >= 1914 & eu3$year <= 1923
eu3$ww2 = eu3$year >= 1937 & eu3$year <= 1950
eu3$coldwar = eu3$year >= 1989 & eu3$year <= 1994
eu3$systchange = eu3$year >= 1859 & eu3$year <= 1977
# eu3 = eu3 %>%
#   group_by(dyad) %>%
#   mutate(
#     mindyadyear = min(year)
#   )

# Domestic shock variables
eu3$postindep = 


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
d = tvcure(Surv(start, stop, fail) ~ lpchcap + lagsolsch + lcwany, 
           cureform = ~ majpower + lnccdist + 
             defense + 
             ldemdy + igosum + 
             lagterrch + lrival + TEK,
           data = eu3, subset = eu3$year > 1917,
           brglm = T, var = T, nboot = 100); summary(d)
end = Sys.time()
dur = end - start
