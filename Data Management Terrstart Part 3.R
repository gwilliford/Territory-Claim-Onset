setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data")
library(desctools)

newgene <- datlag
# fullterr <- read_csv("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/ICOW Territory Provisional Data 1.01/ICOWprovyr101.csv") %>%
#   filter(chal != 2220 & tgt != 2200)
# fullterr = fullterr %>% 
#   group_by(claimdy) %>% 
#   mutate(
#     cltermyr  = max(year),
#     clterm    = if_else(cltermyr == year, 1, 0),
#   )
# terr <- fullterr %>%
#   arrange(dyad, year) %>%
#   group_by(dyad) %>%
#   mutate(
#     one = 1,
#     cltermyr  = max(year),
#     clterm    = if_else(cltermyr == year, 1, 0),
#     clstop    = cumsum(one),
#     clstart   = clstop - 1,
#   ) %>% 
#   ungroup(fullterr)

terr_claimdy <- read_csv("C:/Users/gwill/Dropbox/Research/Dissertation/Data Analysis - Territory Onset/data/ICOW Territory Provisional Data 1.01/ICOWprov101.csv") %>% 
  select(dyad, begclaim, endclaim)

# Code beginning and end years
terr_claimdy$begclaim = as.character(terr_claimdy$begclaim)
terr_claimdy$endclaim = as.character(terr_claimdy$endclaim)
terr_claimdy$byr = substr(terr_claim$begclaim, start = 1, stop = 4)
terr_claimdy$eyr = substr(terr_claim$endclaim, start = 1, stop = 4)

# 
  group_by(dyad, yeterr_claim = terr_claimdy %>% 
ar) %>% 
  summarize(
    
  )
