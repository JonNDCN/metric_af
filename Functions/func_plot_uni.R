#### Visualise the relationship between a predictor variable and the outcome

func_plot_uni <- function(data, predictor_var, xlow = 0.4, xhigh = 1.4, ylim = c(0,0.2)){
  data %>% group_by(!!sym(predictor_var)) %>% summarise(pred = mean(pred), lci = mean(lci), uci = mean(uci) ) %>% ggplot(data = ., aes(
    x = !!sym(predictor_var), y = pred, ymin = lci, ymax = uci)) +
    geom_line(size = 1) +
    coord_cartesian(xlim = c(xlow,xhigh), ylim = ylim, expand = F) +
    scale_y_continuous(name = "24-hour risk of NOAF (%)", breaks = seq(0,ylim[2],0.05),
                       labels = scales::label_percent(suffix = "")
                       ) +
    theme(legend.position = "none") +
    geom_ribbon(alpha = 0.2)
}
