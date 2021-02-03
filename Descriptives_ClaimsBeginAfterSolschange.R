library(haven)
library(dplyr)
library(matrixStats)

# coalition changes
chisols = read_dta('C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/sourcedata/CHISOLSstyr4_0.dta')
chisols$solschany = ifelse(chisols$solschdum == 1 | chisols$solsminchdum == 1, 1, 0)
chisols$solschmomin = with(chisols, rowMins(cbind(solschmo1, solschmo2, solschmo3,
                                            solschmo4, solschmo5, solschmo6,
                                            solschmo7, solsminchmo1, solsminchmo2, 
                                            solsminchmo3, solsminchmo4, solsminchmo5, 
                                            solsminchmo6, solsminchmo7), na.rm = T))
chisols = chisols %>%
  arrange(ccode, year) %>% 
  group_by(ccode) %>%
  mutate(lsolschany = lag(solschany), 
         lsolschmomin = lag(solschmomin))
chisols1 = chisols %>% select("chal" = "ccode", "begyear" = "year", 
                              'solschanychal' = 'solschany',
                              'lsolschanychal' = 'lsolschany', 
                              'solschmominchal' = 'solschmomin',
                              'lsolschmominchal' = 'lsolschmomin')
chisols2 = chisols %>% select("tgt" = "ccode", "begyear" = "year", 
                              'solschanytgt' = 'solschany',
                              'lsolschanytgt' = 'lsolschany', 
                              'solschmomintgt' = 'solschmomin',
                              'lsolschmomintgt' = 'lsolschmomin')

icow = read_dta("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/sourcedata/ICOW Territory Provisional Data 1.01/ICOWprov101.dta") %>% 
  select(claimdy, chal, tgt, dyad, begclaim)
icow$begyear = as.numeric(substr(icow$begclaim, start = 1, stop = 4))
icow$begmon = as.numeric(substr(icow$begclaim, start = 5, stop = 6))


m = left_join(icow, chisols1)
m = left_join(m, chisols2)
m = m %>% filter(begyear > 1918)
m = m %>% filter(!is.na(claimdy))

m$transbefclaimchal = ifelse(m$solschanychal == 1 & m$begmon > m$solschmominchal, 1, 0)
m$transbefclaimchal = ifelse(m$lsolschanychal == 1 & m$begmon >= m$lsolschmominchal, 1, m$transbefclaimchal)
m$transbefclaimtgt = ifelse(m$solschanytgt == 1 & m$begmon > m$lsolschmomintgt, 1, 0)
m$transbefclaimtgt = ifelse(m$lsolschanytgt == 1 & m$begmon >= m$lsolschmomintgt, 1, m$transbefclaimtgt)
m$transbefclaim = ifelse(m$transbefclaimchal == 1 | m$transbefclaimtgt == 1, 1, 0)

# There are 68 claims that begin w/in 1 year of a coalition change
# table() - haven't defined 0s - need to see if 
# chisols = filter(chisols, solschany == 1)
# chisols = chisols %>% mutate(
#   solschmo1 = 
# )
# = dplyr::select(chisols, ccode, year, solschdum, solsminchdum, regtrans,
#                         demtrans, auttrans, autend)


icowcyr = read_dta("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/sourcedata/ICOW Territory Provisional Data 1.01/ICOWprovyr101.dta")

