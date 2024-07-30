#######################
#Function to resample from dat and get boostrap CIs for univariable assessments
#######################

if (!requireNamespace("rsample", quietly = TRUE)) {
  install.packages("rsample")
}
library(rsample)


resample_univariable <- function(data, outcome_var = "outcome", predictor_var, group_var = "stay_id", ns_degree = 3, knots = NULL,
                                 n_boots = 1000, workers = 15, pred_min = NULL, pred_max = NULL, enrich = FALSE) {
  library(future)
  # Set seed for reproducibility
  set.seed(42)
  
  if(enrich){
  data <- data %>% filter(!!sym(predictor_var)>0)
  }
  
  # Prepare patient-level data
  dat_patientlevel <- data %>%
    group_by(!!sym(group_var)) %>%
    slice_sample(n = 1) %>%
    ungroup()
  
  # Fit logistic regression model
  formula_str <- as.formula(paste(outcome_var, "== 'af' ~ ns(", predictor_var, ",", ns_degree, ",", "knots = ", 
                                  if(is.null(knots)) "NULL" else paste(knots, collapse = ", "),  ")"))
  model <- glm(formula = formula_str, family = binomial, data = dat_patientlevel)
  
  
  
  # Set prediction min and max based on parameters or dataset values
  if (is.null(pred_min)) pred_min <- min(dat_patientlevel[[predictor_var]], na.rm = TRUE)
  if (is.null(pred_max)) pred_max <- max(dat_patientlevel[[predictor_var]], na.rm = TRUE)
  
  # Create list of resamples
  patlevelresample <- function(d) {
    d <- d %>%
      group_by(!!sym(group_var)) %>%
      slice_sample(n = 1) %>%
      ungroup()
    d <- rsample::bootstraps(d, times = 1)
    d <- suppressWarnings(d$splits %>% as.data.frame(optional = FALSE))
    return(d)
  }
  
  future::plan(multisession, workers = workers)
  boots <- future.apply::future_replicate(n_boots, patlevelresample(data), simplify = FALSE)
  future::plan(sequential)
  
  boots <- map_dfr(boots, bind_rows, .id = "bootstrap") %>%
    nest(data = -bootstrap)
  
  # Fit model on bootstrap samples
  fit_model_on_boot <- function(tib) {
    glm(formula = formula_str, family = binomial, data = tib)
  }
  
  library(splines)
  future::plan(multisession, workers = 2)
  boots <- boots %>%
    mutate(model = future_map(data, fit_model_on_boot, .progress = TRUE, .options = furrr_options(packages = "splines")))
  future::plan(sequential)
  
  # Create prediction grid
  predictor_seq <- seq(pred_min, pred_max, length.out = 100)
  pred_grid <- expand.grid(predictor_var = seq(pred_min, pred_max, length.out = 100))
  pred_grid <- setNames(pred_grid, predictor_var)
  
  # Predictions
  predict_on_grid <- function(model) {
    predict(model, newdata = pred_grid, type = "response")
  }
  
  preds <- lapply(boots$model, predict_on_grid)
  preds <- do.call(rbind, lapply(seq_along(preds), function(i) cbind(bootstrap = i, pred_grid, pred = preds[[i]])))
  
  # Calculate CIs
  cis <- preds %>%
    group_by(!!sym(predictor_var)) %>%
    summarise(lci = quantile(pred, 0.025), uci = quantile(pred, 0.975)) %>%
    right_join(preds, by = predictor_var)
  
  # Optionally, save the results
  # saveRDS(cis, file = paste0("Data/", predictor_var, "_uni_resamples.RDS"))
  
  ## don't return boots as it's huge
  return(list(model = model, predictions = preds, cis = cis))
}

# Example of calling the function with custom prediction range
# results <- perform_analysis(dat, "outcome", "lab_magnesium", pred_min = 0.5, pred_max = 1.5)