setwd("C:/Users/gwill/Dropbox/Research/Dissertation/chapter4")
library(tvcure)
sols_cure_any = readRDS("./models/sols_cure_any.RDS")

sols_cure_full = readRDS("./models/sols_cure_full.RDS")
sols_cure_cw = readRDS("./models/sols_cure_cw.RDS")
icb_cure = readRDS("./models/icb_cure.RDS")
mid_cure = readRDS("./models/mid_cure.RDS")
fat_cure = readRDS("./models/fat_cure.RDS")
war_cure = readRDS("./models/war_cure.RDS")
fatonset_cure2 = readRDS("./models/fatonset_cure2.RDS")

# log cumulative hazard / -log(log()) plots
plot(-log(sols_cure_cw$Survival), sols_cure_cw$Time)




# Attempt to create hazard estimate - didn't work
#' Function to estimate breslow survivor function from a fitted model
#' Useful for models besides coxph
#' Based on smcure package
#' TODO: Add sort functionality
breslow <- function(Time, Status, X, beta) {
  browser()
  death_point <- sort(unique(subset(Time, Status == 1)))
  coxexp <- exp(t(beta) %*% X)
  
  lambda <- numeric()
  event <- numeric()
  for (i in 1:length(death_point)) {
    event[i] <- sum(Status * as.numeric(Time == death_point[i]))
    temp <- sum(as.numeric(Time >= death_point[i]) * drop(coxexp))
    temp1 <- event[i]
    lambda[i] <- temp1/temp
  }
  HHazard <- numeric()
  for (i in 1:length(Time)) {
    HHazard[i] <- sum(as.numeric(Time[i] >= death_point) * lambda)
    # if (Time[i] > max(death_point))
    #   HHazard[i] <- Inf
    # if (Time[i] < min(death_point))
    #   HHazard[i] <- 0
  }
  survival <- exp(-HHazard)
  list(survival = survival, failtimes = death_point,
       haz = lambda, cumhaz = HHazard)
}

x1 = apply(icb_cure$X, 2, median)
x2 = x1
x2["lbdymid"] = 1
one = breslow(icb_cure$Time, icb_cure$Status, x1, icb_cure$beta)
two = breslow(icb_cure$Time, icb_cure$Status, x2, icb_cure$beta)

plot(one$cumhaz ~ one$failtimes)

plot(stepfun(one$failtimes, c(0, one$haz)))
plot(stepfun(two$failtimes, c(0, two$haz)))
