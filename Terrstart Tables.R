varlist = c("Leadership Change$_{t}$" = "leadchdy0", 
            "Leadership Change$_{t-1}$" = "leadchdy1", 
            "Leadership Change$_{t-2}$" = "leadchdy2",
            "Leadership Change$_{t, t-1, t-2}$" = "leadchdy012",
            "Coalition Change$_{t}$" = "solschdy0", 
            "Coalition Change$_{t-1}$" = "solschdy1", 
            "Coalition Change$_{t-2}$" = "solschdy2",
            "Coalition Change$_{t, t-1, t-2}$" = "solschdy012",
            "Civil War" = "lcwany", 
            "Independence$_t$" = "independenceTRUE", 
            "Independence$_t$" = "independence", 
            "Independence$_{t-1}$" = "lindependenceTRUE", 
            "Independence$_{t-1}$" = "lindependence", 
            "Independence $\\times \\ln(t)$" = "independencelnt", 
            "Regime Transition$_t$" = "regtransdy",
            "Regime Transition$_{t-1}$" = "lregtransdy",
            "Regime Transition $\\times \\ln(t)$" = "regtransdylnt",
            "Percent Change Capabilities$_{t}$" = "pchcaprat",
            "Percent Change Capabilities$_{t-1}$" = "lpchcap",
            "MID$_{t}$" = "bdymid",
            "MID$_{t-1}$" = "lbdymid",
            "System Change (1859-1877)" = "systchangeTRUE", 
            "World War I" = "ww1TRUE",
            "World War II" = "ww2TRUE",
            "World War II $\\times \\ln(t)$" = "ww2lnt",
            "Coldwar Termination" = "coldwarTRUE",
            "Distance" = "lnccdist",
            "Previous Territorial Change" = "lagterrch", 
            "Former Colony" = "postcolonial",
            "Colonial Contiguity" = "colonycontig",
            "Transnational Ethnic Kin" = "TEK",
            "Major-Minor Dyad" = "onemp",
            "Major-Major Dyad" = "twomp",
            "Major Power" = "majpower", 
            "Defensive Alliance" = "defense", 
            "Joint Democracy" = "demdy", 
            "Rivalry" = "trival", 
            "Territorial Integrity Norm" = "tin", 
            "Intercept" = "Intercept")


# mnames = c("", "\multicolumn{2}{c}{Model 1}", "&", "\multicolumn{2}{c}{Model 2}", " &")
# mnames2 = c("", "Model 1", "Model 2")

itab = xtable(tvtable(i0_cure, i1_cure, varlist = varlist))
ltab = xtable(tvtable(l0_cure, l1_cure, varlist = varlist))
stab = xtable(tvtable(s0_cure, s1_cure, varlist = varlist))
#stab_prd = xtable(tvtable(s0_cox_prd, s1_cox_prd, s0_cox_riv, s1_cox_riv, varlist = varlist))
stab_prd = xtable(tvtable(s0_cox_prd, s0_cox_riv, varlist = varlist))
# colnames(itab) = c("", "\multicolumn{2}{c}{Model 1}", "&", "\multicolumn{2}{c}{Model 2}", " &")
# colnames(ltab) = mnames
# colnames(stab) = mnames
# colnames(stab_prd) = mnames2

print(itab,
      booktabs = F,
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F)

print(ltab,
      booktabs = Fm
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F)
print(stab,
      booktabs = F,
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F)
print(stab_prd,
      booktabs = F,
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F)




l0_tab = tvtable(l0, varlist = varlist)
l0_tab <- tvtable_xtable(l0_tab, digits = 2)
print(l0_tab,
      booktabs = F,
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F,
      # add.to.row = addtorow,
      file = 'C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter4/tab/leadertab.tex')
### fix 0 after nobs

hrtab = function(..., varlist = NULL, modnames = NULL, sigonly = T, siglevel = 0.05) {
  
  models <- list(...)
  len = length(models)
  `%notin%` <- Negate(`%in%`)
  class <- lapply(models, class)
  if (sum(class %notin% c("tvcure", "coxph")) > 0) stop("Models must be of class coxph or tvcure.")
  tabnames <- list()
  mn <- list()
  browser()
  
  # Create table
  for (i in 1:length(models)) {
    model <- models[[i]]
    tabnames[[i]] <- model$bnames
    tab = cbind(tabnames, exp(model$curemod$latency_fit$coefficients))
    if (sigonly == T) {
      bpval <- as.numeric(model$b_pvalue)
      tab[, 2] = replace(tab[, 2], bpval > siglevel, NA)
      # for (j in 1:length(bpval)) {
      #   if (bpval[j] > siglevel) replace(tab, tab[j, 2], NA)
      # }
    }
    assign(paste0("tab", i), tab)
  }  

  # order by varlist
  allnames <- unique(unlist(tabnames))
  if (is.null(varlist)) {
    varnames <- allnames
    varlabs <- allnames
  } else {
    index <- match(varlist, allnames)
    index2 <- !is.na(index)
    index3 <- names(varlist[index2])
    index <- index[!is.na(index)]
    vn <- allnames[index]
    # if (length(vn) == 1) c(vn, "")
    # vn <- as.vector(rbind(vn, ""))
    nout <- allnames[allnames %notin% vn]
    varnames <- c(vn, nout)
    varlabs <- varnames
    # dex <- as.vector(rbind(index3, ""))
    varlabs = index3
  }
  
  # Combine tables
  browser()
  tdf  <- matrix(varnames, ncol = 1)
  colnames(tdf)[1] <- "vn"
  for (i in 1:len) {
    tab <- eval(parse(text = paste0("tab", i)))
    colnames(tab)[1] = "vn"
    tab[, "vn"] = tabnames[[i]]
    tdf <- merge(tdf, tab, by = "vn", all.x = T, sort = F)
  }
  ind <- match(varnames, tdf[, 1])
  tdf <- tdf[ind, ]
  tdf[, 1] <- varlabs
  # colnames(tdf)[1] <- c(" ")
  #cn <- colnames(tdf)
  # ind <- startsWith(cn, "Hazard Ratio")
  # cn <- replace(cn, ind, "Hazard Ratio")
  # cn[1] <- ""
  colnames(tdf) <- NULL
  # for (j in seq(2, nrow(tdf), 2)) {
  #   tdf[j, 1] <- ""
  # }
  tdf <- as.matrix(tdf)
  
  # Create model names
  if (is.null(modnames)) {
    for (i in 1:len) {
      mn[[i]] = paste("\\multicolumn{1}{c}{Model ", i, "}", sep = "")
    }
  } else {
    mn[[i]] = modnames[i]
  }

  mn2 <- c(unlist(mn))
  # mn2 <- replace(mn2, mn2 == "blank", "")
  
  ftab <- rbind(mn2, tdf)
  rownames(ftab) <- NULL
  ftab
}

# This works (adding l012 breaks it)
# This doesn't
# Remove insignificant results

leadertab_res = tvtable(l0, l1, l2, l012, varlist = varlist)
leadertab_res <- tvtable_xtable(leadertab_res, digits = 2)
print(leadertab_res,
      booktabs = F,
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F,
      # add.to.row = addtorow,
      file = 'C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter4/tab/leadertab_res.tex')
### fix double modelnames
### fix 0 after nobs
### some stars aren't shoing up - only three star
leadertab_hr = hrtab(l0, l1, l2, varlist = varlist)

saveRDS(s0_cure, "./mods/s0_cure.RDS")
saveRDS(s1_cure, "./mods/s1_cure.RDS")
saveRDS(l0_cure, "./mods/l0_cure.RDS")
saveRDS(s1_cure, "./mods/l1_cure.RDS")
