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
