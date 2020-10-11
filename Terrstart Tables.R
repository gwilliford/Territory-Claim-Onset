varlist = c("Leadership Change_{t}" = "leadchdy0", 
            "Leadership Change_{t-1}" = "leadchdy1", 
            "Leadership Change_{t-2}" = "leadchdy2",
            "Leadership Change_{t, t-1, t-2}" = "leadchdy012",
            "Coalition Change_{t}" = "solschdy0", 
            "Coalition Change_{t-1}" = "solschdy1", 
            "Coalition Change_{t-2}" = "solschdy2",
            "Coalition Change_{t, t-1, t-2}" = "solschdy012",
            "Civil War" = "lcwany", 
            "Independence" = "independenceTRUE", 
            "Independence $\\times \\ln(t)$" = "independencelnt", 
            "Percent Change Capabilities_t" = "pchcaprat",
            "Percent Chagne Capabilities_{t-1}" = "lpchcap",
            "MID_{t}" = "bdymid",
            "MID_{t-1}" = "lbdymid",
            "System Change (1859-1877)" = "systchange", 
            "World War I" = "ww1",
            "World War II" = "ww2",
            "Coldwar Termination" = "coldwar",
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
            "Territorial Integrity Norm" = "tin")

l0_tab = tvtable(l0, varlist = varlist)
l0_tab <- tvtable_xtable(l0_tab, digits = 2)
print(l0_tab,
      booktabs = T,
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F,
      # add.to.row = addtorow,
      file = 'C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter4/tab/leadertab.tex')
### fix 0 after nobs

hrtab = function(..., varlist = NULL, modnames = NULL) {
  
  models <- list(...)
  len = length(models)
  `%notin%` <- Negate(`%in%`)
  class <- lapply(models, class)
  if (sum(class %notin% c("tvcure", "coxph")) > 0) stop("Models must be of class coxph or tvcure.")
  tabnames <- list()
  mn <- list()

  for (i in 1:length(models)) {
    model <- models[[i]]
    tabnames[[i]] <- model$bnames
    tab = cbind(tabnames, exp(model$curemod$latency_fit$coefficients))
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
    rownames(tab) = NULL
    colnames(tab)[1] = "vn"
    tdf <- merge(tdf, tab, by = "vn", all.x = T, sort = F)
  }
  ind <- match(varnames, tdf[, 1])
  tdf <- tdf[ind, ]
  tdf[, 1] <- varlabs
  colnames(tdf)[1] <- c(" ")
  cn <- colnames(tdf)
  ind <- startsWith(cn, "Hazard Ratio")
  cn <- replace(cn, ind, "Hazard Ratio")
  cn[1] <- ""
  colnames(tdf) <- NULL
  for (j in seq(2, nrow(tdf), 2)) {
    tdf[j, 1] <- ""
  }
  tdf <- as.matrix(tdf)
  
  
  # Create model names
  # if (is.null(modnames)) {
  #   if (nc == 1) {
  #     mn[[i]] <- paste("\\multicolumn{1}{c}{Model ", i, "}", sep = "")
  #   } else {
  #     mn[[i]] <- paste("\\multicolumn{2}{c}{Model ", i, "}", sep = "")
  #   }
  # } else {
  #   if (nc == 1) {
  #     mn[[i]] <- modnames[i]
  #   } else {
  #     mn[[i]] <- c(modnames[i], "blank")
  #   }
  # }
  if (is.null(modnames)) {
    for (i in 1:len) {
      mn[[i]] = paste("\\multicolumn{1}{c}{Model ", i, "}", sep = "")
    }
  } else {
    mn[[i]] = modnames[i]
  }
  browser()
  mn2 <- c(unlist(mn))
  # mn2 <- replace(mn2, mn2 == "blank", "")
  ftab <- rbind(mn2, tdf)
  rownames(ftab) <- NULL
  ftab
}
hrtab(l0, l1, varlist = varlist)


l0_tab <- tvtable(cox2, cure2, varlist = vl, modnames = F)


l0_tab_hr <- cbind(l0$, exp(l0$coefficients))