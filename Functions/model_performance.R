func_model_performance <- function(model, data, testdata, cal_lim = 0.3, add_line = T, bootstrapping = FALSE, use_manual_probs = F, recalibrate_external = T,
                                 recal_intercept_only = F) {
    
    ##setup
    data <- data %>% ungroup()
    testdata <- testdata %>% ungroup()
    perfobj <- list()
    if(class(model)[[1]]=="rfe"){message("This function is testing the optimal model produced from recursive feature elimination (rfe)")}
    message("generating predictions in training data")
    
    ## Generate predictions in training data
    if(use_manual_probs == F){
    data <- data %>% mutate(probs = predict(model,type="prob", newdata = data)[["af"]]) }
    else {
      data <- generate_klein_probs(data)
    }
    
    message("generating calibration model")
    
    ## get 1 row-per patient data, random sample
    data <- data %>% group_by(stay_id) %>% slice_sample(n = 1) %>% ungroup()
    
    # Generate calibrated predictions in training data
    calib_mod <- glm(outcome=="af" ~ probs, family=binomial, data = data)
    data <- data %>% mutate(iso_probs =  predict(calib_mod, type = "response", newdata = data))
    
    if(model$fit$modelInfo$label=="glmnet" || model$fit$modelInfo$label=="glm" || use_manual_probs==T){
      data <- data %>% mutate(lp_probs = boot::logit(probs))
      calib_mod <- glm(outcome=="af" ~ lp_probs, family=binomial, data = data)
        if(recal_intercept_only==T){
          calib_mod <- glm(outcome=="af" ~ offset(lp_probs), family=binomial, data = data)
          }
      data <- data %>% mutate(iso_probs =  predict(calib_mod, type = "response", newdata = data))
    }
    
    
    message("generating predictions in test data")
    if(use_manual_probs==F){
      testdata <- testdata %>% mutate(probs = predict(model,type="prob", newdata = testdata)[["af"]])
    } else {
      testdata <- generate_klein_probs(testdata)
    }
    
    ## get 1 row-per patient data, random sample
    testdata <- testdata %>% group_by(stay_id) %>% slice_sample(n = 1) %>% ungroup()
    
    ## Generate calibrated predictions in test data using calib mod
    testdata <- testdata %>% mutate(lp_probs = boot::logit(probs))
    
    if(recalibrate_external==T){
    testdata <- testdata %>% mutate(iso_probs =  predict(calib_mod, newdata = testdata, type = "response"))
    } else {
      testdata <- testdata %>% mutate(iso_probs =  probs)
    }
    
    if(bootstrapping == T){
      message("Generating bootstrap resample of patient-level data")
      ptleveldata_boot <- rsample::bootstraps(data, times = 1)
      data <- suppressWarnings(ptleveldata_boot$splits %>% as.data.frame(optional = F))
    }
    
    

    
    if(bootstrapping == T){
      message("Generating bootstrap resample of patient-level data")
      test_ptleveldata_boot <- rsample::bootstraps(testdata, times = 1)
      testdata <- suppressWarnings(test_ptleveldata_boot$splits %>% as.data.frame(optional = F))
    }
    


  
  message("Calculating performance")
  ## calculate apparent performance in one-row-per-patient
  data <- data %>% mutate(lp = boot::logit(iso_probs))
  apparent_cstat_model <- roc(outcome=="af" ~ iso_probs, data = data, quiet = T)
  apparent_auc <-  apparent_cstat_model$auc
  apparent_ci_auc <- c(ci.auc(apparent_cstat_model)[[1]], ci.auc(apparent_cstat_model)[[3]])
  
  ## calculate test performance
  testdata <- testdata %>% mutate(lp = boot::logit(iso_probs))
  test_cstat_model <- roc(outcome=="af" ~ iso_probs, data = testdata, quiet = T)
  test_auc <-  test_cstat_model$auc
  test_ci_auc <- c(ci.auc(test_cstat_model)[[1]], ci.auc(test_cstat_model)[[3]])
  
  message("Generating decision curves")
  dc <- rmda::decision_curve(formula = outcome ~ iso_probs, data = 
                               testdata %>% mutate(outcome = as.numeric(outcome=="af")),
                             policy = "opt-in", confidence.intervals = "none",
                             family = "binomial", fitted.risk = TRUE, thresholds = 0:25/100, study.design = "cohort",
  )
  
  message("Assessing calibration")
  ## Apparent calibration
  cslope_model <- glm(outcome=="af" ~ lp ,family="binomial", data=data)
  slope <- summary( cslope_model)$coefficients[2,1]
  
  citl_model <- glm(outcome=="af" ~ offset(lp),family="binomial", data=data)
  citl <- summary( citl_model)$coefficients[1,1]
  
  ## Test calibration
  test_cslope_model <- glm(outcome=="af" ~ lp ,family="binomial", data=testdata)
  test_slope <- summary(test_cslope_model)$coefficients[2,1]
  
  test_citl_model <- glm(outcome=="af" ~ offset(lp),family="binomial", data=testdata)
  test_citl <- summary( test_citl_model)$coefficients[1,1]
  
  auc <- c(apparent = round(apparent_auc,3),
           test = round(test_auc,3))
  cslope <- c(apparent = round(slope,3), test = round(test_slope,3))
  citl <- c(apparent = round(citl,3), test = round(test_citl,3))
  
  stats <- rbind(auc,cslope,citl)
  perfobj <- list()
  perfobj[["stats"]] <- stats
  
  if(bootstrapping==F){
    
    message("Generating calibration data")
    ### Calibration plots
    datalist <- list(data, testdata)
    # create 10 risk groups
    cal <- lapply(datalist, function(x){
      gpdata <- x %>% ungroup() %>%
        mutate(groups = cut(iso_probs,breaks=quantile(iso_probs,
                            prob = seq(0,1,0.1)),
                            labels=c(1:10),include.lowest=TRUE))
      obs <- gpdata %>% group_by(groups) %>% summarise(mean = mean(as.numeric(outcome=="af"))) %>% pull(mean)
      exp <- gpdata %>% group_by(groups) %>% summarise(exp = mean(iso_probs))
      obsn <- table(gpdata$outcome,gpdata$groups)["not_af",] #counts of non-af by risk group
      
      lci = pmax(0,(obs - (1.96*(((obs*(1-obs))/obsn)^.5))))
      uci = pmin(1,(obs + (1.96*(((obs*(1-obs))/obsn)^.5))))
      
      cal_data <- data.frame(exp,obs,lci,uci)
      return(cal_data)
    }
    )
    names(cal) = c("apparent","test")
    
    message("Generating calibration plots")
    cal_plots <- mapply(function(x,y){
      ### Generate calibration plots
      obs_all <- predict(loess(as.numeric(outcome=="af")~iso_probs,span=1, data = x, control = loess.control(statistics = "none")))
      lines_data <- data.frame(iso_probs = x$iso_probs,obs_all)
      lines_dataa <- lines_data %>% arrange(iso_probs)
      
      gg_cal <- ggplot() + geom_pointrange(data = y, aes(x = exp, y = obs, ymin = lci, ymax = uci)) +
        geom_abline(slope = 1, linetype = "dashed") +
        coord_cartesian(xlim = c(0,cal_lim), ylim = c(0,cal_lim), expand = F) +
        theme(plot.margin = margin(r = 10, t = 10), aspect.ratio = 1) +
        scale_x_continuous(breaks = seq(0,cal_lim,0.1)) + scale_y_continuous(breaks = seq(0,cal_lim,0.1))
      
    if(add_line == T){
      ggcal <- gg_cal + geom_line(data = lines_dataa, mapping = aes(x = iso_probs, y = obs_all))
    }
      return(gg_cal)
    }, x = datalist, y = cal,
    SIMPLIFY = FALSE
    )
    
    names(cal_plots) = c("apparent","test")
    
    
    ### ROC curves
    roc_all <- lapply(datalist, function(x){
      roc_data <-  roc(outcome=="af" ~ iso_probs, data = x, quiet = T)
      roc_labels <- as.data.frame(coords(roc_data, x=seq(0.01,0.10,0.01),
                                         input="threshold")) %>% mutate(threshold = paste0(threshold*100,"%"))
      roc_best <- coords(roc_data, "best", best.method = "closest.topleft")
      gg_roc <- ggroc(roc_data, legacy.axes = TRUE) + 
        coord_cartesian(expand = F) + theme(plot.margin =  margin(r=20, t = 20), aspect.ratio = 1) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        geom_point(data = roc_labels, aes(x = 1-specificity, y = sensitivity)) +
        geom_text(data = roc_labels, aes(x = 1-specificity, y = sensitivity, label = threshold),
                  hjust = -0.6, vjust=1)
      return(list(roc = roc_data, roc_labels = roc_labels, roc_best = roc_best, roc_plot = gg_roc))
    }
    )
    names(roc_all) = c("apparent","test")
    
    roc_compare <- ggroc(list(Apparent = roc_all$apparent$roc, Test = roc_all$test$roc), legacy.axes = TRUE) + 
      coord_cartesian(expand = F) + theme(plot.margin =  margin(r=20, t = 20), aspect.ratio = 1) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") + scale_color_grey()
    
    perfobj[["caldata"]] <- cal
    perfobj[["calplot"]] <- cal_plots
    perfobj[["decision_curves"]] <- dc
    perfobj[["roc"]] <- roc_all
    perfobj[["roc_compare"]] <- roc_compare
    return(perfobj)} else {
      return(c(test_auc, test_slope, test_citl))
    }
}



# get confidence intervals for external performance statistics
func_model_performance_cis <- function(model, intdata, extdata, nboots = 1000, ncores = 10) {

library(future)
options(future.globals.maxSize= 1200 * 1024^2)
plan(multisession, workers = ncores)
set.seed(42)
cis <-suppressMessages(suppressWarnings(future.apply::future_replicate(n=nboots,
        jb_caret_performance(model, intdata, extdata, bootstrapping = T),
        simplify = T)
))

plan(sequential)

return(cis)

}


  



