



# LICB
S3 = survfit(Surv(start, stop, fail) ~ 1, data = terrstart[terrstart$solsch1 == 1, ])
p3 = S3$surv[length(S3$surv)]
L3 = log(-log((S3$surv - (1 - p3))/p3))
A3 = (S3$surv - (1 - p3))/p3
S4 = survfit(Surv(start, stop, fail) ~ 1, data = terrstart[terrstart$solsch1 == 0, ])
p4 = S4$surv[length(S4$surv)]
L4 = log(-log((S4$surv - (1 - p4))/p4))
A4 = (S4$surv - (1 - p4))/p4

## LL plot
plot(L3 ~ S3$time, type = "l")
lines(L4 ~ S4$time)
## Unadjusted survplot
plot(S3$surv ~ S3$time, type = "l")
lines(S4$surv ~ S4$time)
## Adjusted survplot
plot(A3 ~ S3$time, type = "l")
lines(A4 ~ S4$time)

S3 = survfit(Surv(start, stop, fail) ~ 1, data = terrstart[terrstart$lindependence == 1, ])
p3 = S3$surv[length(S3$surv)]
L3 = log(-log((S3$surv - (1 - p3))/p3))
A3 = (S3$surv - (1 - p3))/p3
S4 = survfit(Surv(start, stop, fail) ~ 1, data = terrstart[terrstart$lindependence == 0, ])
p4 = S4$surv[length(S4$surv)]
L4 = log(-log((S4$surv - (1 - p4))/p4))
A4 = (S4$surv - (1 - p4))/p4

## LL plot
plot(L3 ~ S3$time, type = "l")
lines(L4 ~ S4$time)
## Unadjusted survplot
plot(S3$surv ~ S3$time, type = "l")
lines(S4$surv ~ S4$time)
## Adjusted survplot
plot(A3 ~ S3$time, type = "l")
lines(A4 ~ S4$time)

S3 = survfit(Surv(start, stop, fail) ~ 1, data = terrstart[terrstart$lcowcwany == 1, ])
p3 = S3$surv[length(S3$surv)]
L3 = log(-log((S3$surv - (1 - p3))/p3))
A3 = (S3$surv - (1 - p3))/p3
S4 = survfit(Surv(start, stop, fail) ~ 1, data = terrstart[terrstart$lcowcwany == 0, ])
p4 = S4$surv[length(S4$surv)]
L4 = log(-log((S4$surv - (1 - p4))/p4))
A4 = (S4$surv - (1 - p4))/p4

## LL plot
plot(L3 ~ S3$time, type = "l")
lines(L4 ~ S4$time)
## Unadjusted survplot
plot(S3$surv ~ S3$time, type = "l")
lines(S4$surv ~ S4$time)
## Adjusted survplot
plot(A3 ~ S3$time, type = "l")
lines(A4 ~ S4$time)



S3 = survfit(Surv(start, stop, fail) ~ 1, data = terrstart[terrstart$lregtrans == 1, ])
p3 = S3$surv[length(S3$surv)]
L3 = log(-log((S3$surv - (1 - p3))/p3))
A3 = (S3$surv - (1 - p3))/p3
S4 = survfit(Surv(start, stop, fail) ~ 1, data = terrstart[terrstart$lregtrans == 0, ])
p4 = S4$surv[length(S4$surv)]
L4 = log(-log((S4$surv - (1 - p4))/p4))
A4 = (S4$surv - (1 - p4))/p4

## LL plot
plot(L3 ~ S3$time, type = "l")
lines(L4 ~ S4$time)
## Unadjusted survplot
plot(S3$surv ~ S3$time, type = "l")
lines(S4$surv ~ S4$time)
## Adjusted survplot
plot(A3 ~ S3$time, type = "l")
lines(A4 ~ S4$time)


S3 = survfit(Surv(start, stop, fail) ~ 1, data = terrstart[terrstart$lbfatonset == 1, ])
p3 = S3$surv[length(S3$surv)]
L3 = log(-log((S3$surv - (1 - p3))/p3))
A3 = (S3$surv - (1 - p3))/p3
S4 = survfit(Surv(start, stop, fail) ~ 1, data = terrstart[terrstart$lbfatonset == 0, ])
p4 = S4$surv[length(S4$surv)]
L4 = log(-log((S4$surv - (1 - p4))/p4))
A4 = (S4$surv - (1 - p4))/p4

## LL plot
plot(L3 ~ S3$time, type = "l")
lines(L4 ~ S4$time)
## Unadjusted survplot
plot(S3$surv ~ S3$time, type = "l")
lines(S4$surv ~ S4$time, lty = 2)
## Adjusted survplot
plot(A3 ~ S3$time, type = "l")
lines(A4 ~ S4$time)

