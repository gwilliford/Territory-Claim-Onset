sols_cure_cw = readRDS("./models/sols_cure_cw.RDS")

varlist = c("Coalition Change" = "solschany1",
            "Coalition Change" = "solsch1",
            "Civil War" = "lcowcwany",
            "Independence" = "lindependenceTRUE",
            "Independence" = "lindependence",
            "Independence $\\times \\ln(t)$" = "independencelnt",
            "Regime Transition" = "lregtrans",
            "Regime Transition $\\times \\ln(t)$" = "regtransdylnt",
            "Percent Change Capabilities" = "lpchcap",
            "MID Onset" = "lbmidonset",
            "Fatal MID Onset" = "lbfatonset",
            "System Change (1859-1877)" = "systchangeTRUE",
            "World War I" = "ww1TRUE",
            "World War II" = "ww2TRUE",
            "World War II $\\times \\ln(t)$" = "ww2lnt",
            "Coldwar Termination" = "coldwarTRUE",
            "Distance" = "lnccdist",
            "Contiguity" = "contdir",
            "Major-Minor Dyad" = "onemp",
            "Major-Major Dyad" = "twomp",
            "Previous Territorial Change" = "lagterrch",
            "Former Dependency" = "postcolonial",
            "Colonial Contiguity" = "colonycontig",
            "Transnational Ethnic Kin" = "TEK",
            "Defensive Alliance" = "defense",
            "Joint Democracy" = "demdy",
            "Rivalry" = "trival",
            "Territorial Integrity Norm" = "tin",
            "Intercept" = "Intercept")

itab = xtable(tvtable(sols_cure_cw, varlist = varlist, digits = 2))

print(itab,
      booktabs = F,
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F)

