sols_surv = predict(sols_cure_full, "suncure", "solsch1", c(0, 1), CI = F)
sols_surv = sols_surv + xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = c(0, 1), name = "Coalition Change") +
  scale_color_discrete(labels = c(0, 1), name = "Coalition Change")
ggsave("./images/sols_surv.png", sols_surv, width = 5, height = 3, units = "in")
sols_surv = prediction4(sols_cure_full, "suncure", "solsch1", c(0, 1), CI = F, internals = T)


