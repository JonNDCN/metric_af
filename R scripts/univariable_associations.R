#### Perform boostrapped resampled univariable modelling

resamp_uni_predictor <-  resample_univariable(
  data = dat,
  predictor_var = "predictor",
  workers = 10,
  n_boots = 1000
)

## Plot

plot_resamp_uni_predictor <- func_plot_uni(
  resamp_uni_predictor$cis,
  predictor_var = "predictor",
  xlow = 4,
  xhigh = 20,
  ylim = c(0, 0.2)
) +
  scale_x_continuous(breaks = seq(0, 20, 2), name = "Predictor (units)")
