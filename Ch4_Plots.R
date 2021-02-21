sols_surv = prediction4(sols_cure_full, "suncure", "solsch1", c(0, 1), CI = F)
sols_surv = sols_surv + xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = c(0, 1), name = "Coalition Change") +
  scale_color_discrete(labels = c(0, 1), name = "Coalition Change")
ggsave("./images/sols_surv.png", sols_surv, width = 5, height = 3, units = "in")
sols_surv = prediction4(sols_cure_full, "suncure", "solsch1", c(0, 1), CI = F, internals = T)


nx = apply(sols_cure_cw$X, 2, median)
nx = rbind(nx, nx)
nx[2, "solsch1"] = 1
nz = apply(sols_cure_cw$Z, 2, median)
nz = c(1, 1, 1, 1, 1, 0, 1, 0, 0, 1)
nz = rbind(nz, nz)
colnames(nz) = colnames(sols_cure_cw$Z)
sols_surv2 = prediction4(sols_cure_cw, "suncure", newX = nx, newZ = nz, CI = F, internals = T)


war_surv = prediction4(waronset_cure, "suncure", "lbwaronset", c(0, 1), CI = F)
war_surv = war_surv + xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = c(0, 1), name = "War Onset") +
  scale_color_discrete(labels = c(0, 1), name = "War Onset")
ggsave("./images/war_surv.png", war_surv, width = 5, height = 3, units = "in")


a = prediction4(sols_cure_full, "uncureprob", "contdir", c(0, 1), CI = F, internals = T)
# nx = apply(sols_cure_any$X, 2, median)
# nz = apply(sols_cure_any$Z, 2, median)
# nz[1] = sols_cure_any$gamma[1]
# nz = as.matrix(nz)
# a2 = predict_tvcure_noci(sols_cure_any, F, newX = nx, newZ = nz)
f = prediction4(sols_cure_full, "uncureprob", "onemp", c(0, 1), CI = F, internals = T)
g = prediction4(sols_cure_full, "uncureprob", "twomp", c(0, 1), CI = F, internals = T)
e = prediction4(sols_cure_full, "uncureprob", "colonycontig", c(0, 1), CI = F, internals = T)
i = prediction4(sols_cure_full, "uncureprob", "demdy", c(0, 1), CI = F, internals = T)
j = prediction4(sols_cure_full, "uncureprob", "trival", c(0, 1), CI = F, internals = T)

k = rbind(a$uncureprob, f$uncureprob, g$uncureprob, e$uncureprob, 
          i$uncureprob, j$uncureprob)
rownames(k) = c("Contiguity", "Major-Minor Dyad", "Major-Major Dyad",
                "Colonial Contiguity", "Joint Democracy", "Rivalry")
pi = round(((k[, 2] - k[, 1])/k[, 1]) * 100, 0)
k = cbind(k, pi)
colnames(k) = c("Variable = 0", "Variable = 1", "Percent Change")
xtable(k, digits = -2)


pd = round(((k[, 1] - k[, 2])/k[, 1]) * 100, 0)

increase, increase, increase, increase, increase, increase, decrease, decrease, increase

c(pi[1:6], pd[7:8], pi[9])

pchange = cbind(rbind("Contiguity", "Previous Territorial Change", 
            "Colonial History", "Colony Contiguity", "Major-Minor Dyad", 
            "Major-Major Dyad", "Alliance", "Joint Democracy", "Rivalry"),
      pi)
colnames(pchange) = c(" ", "Percent Change")
# no lagterrch, postcolonial, defense
# Increase: (New - Old) / old X 100
# Decrease: (Old - New) / old x 100
pchange[2, 2] = ""
pchange[3, 2] = ""
pchange[7, 2] = ""

pchange = pchange[-c(2, 3, 7), ]
library(xtable)
xtable(pchange, caption = "Percent Change in Predicted Probability of Territorial Claim Onset",
       label = "tab_pchange")


ptab = pi[c(1, 4:6, 8:9)]
names(ptab) = c("Contiguity", "Colonial Contiguity", "Major-Minor Dyad", 
"Major-Major Dyad", "Joint Democracy", "Rivalry")
ptab = as.matrix(ptab)
colnames(ptab) = "Percentage Change"
xtable(ptab, caption = "Percentage Change in Predicted Probability of Territorial Claim Onset",
       label = "tab_pchange", digits = 0)
