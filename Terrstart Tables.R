varlist = c("Leadership Change_{t}" = "leadch0", 
            "Leadership Change_{t-1}" = "leadch1", 
            "Leadership Change_{t-2}" = "leadch2",
            "Leadership Change_{t, t-1, t-2}" = "leadch012",
            "Coalition Change_{t}" = "solsch0", 
            "Coalition Change_{t-1}" = "solsch1", 
            "Coalition Change_{t-2}" = "solsch2",
            "Coalition Change_{t, t-1, t-2}" = "solsch012",
            "Civil War" = "lcwany", 
            "Independence" = "independence", 
            "Independence" = "independencelnt", 
            "Percent Change Capabilities_t" = "pchcaprat",
            "Percent Chagne Capabilities_{t-1} = lpchcap",
            "MID_{t}" = "bdymid",
            "MID_{t-1}" = "lbdymid",
            "System Change (1859-1877)" = "systchange", 
            "World War I" = "ww1",
            "World War II" = "ww2",
            "Coldwar Termination" = "coldwar",
            "Distance" = "lnccdist",
            "Previous Territorial Change" = "lagterrch", 
            "Former Colony" = "postcolonial",
            "Colonial Contiguity" = "colonycontig"
            "Transnational Ethnic Kin" = "TEK",
            "Major-Minor Dyad" = "onemp",
            "Major-Major Dyad" = "twomp",
            "Major Power" = "majpower", 
            "Defensive Alliance" = "defense", 
            "Joint Democracy" = "demdy", 
            "Rivalry" = "trival", 
            "Territorial Integrity Norm" = "tin")

tvtable(l0, varlist = varlist)

l0_tab <- tvtable(cox2, cure2, varlist = vl, modnames = F)
x1 <- tvtable_xtable(t1, digits = 2)
print(x1,
      booktabs = T,
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F,
      # add.to.row = addtorow,
      file = 'C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/tab/res.tex')

l0_tab_hr <- cbind(l0$, exp(l0$coefficients))