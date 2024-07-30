### XGBoost model ###
library(caret)
library(xgboost)
library(doParallel)
 besttune_xgboost <- read_rds(file = "Models/besttune_xgboost.RDS")


## Feature selection ##
set.seed(42)
 cl <- makePSOCKcluster(14)
 registerDoParallel(cl)
   model <- rfe(
     form =   outcome ~
       age + gender + weight + day + ccf + copd + ihd + htn + # comorbidities
       vital_heart_rate + prev_hrmax + prev_hrmean + prev_hrmin + #vital signs
       vital_temperature + prev_tempmax + vital_spo2 + vital_mbp + prev_mbpmin +
       prev_mbpmax + atrial_ectopics +
       norad_rate + prev_noradmax + vasopressin_binary + beta_blocker + #interventions
       mechvent + peep + fio2 + prev_fio2max + cvc + rrt  +
       lab_potassium + prev_kmin + prev_kmean + lab_magnesium + lab_sodium + #investigations
       lab_ph + lab_urea + prev_ureamax + lab_creatinine +
       lab_lactate + lab_albumin + lab_hb +  lab_plt + lab_wbc + lab_log_nlr +
       ethnicity_reduced
     ,
     data = dat,
     sizes = seq(2, 24, 1),
     rfeControl = caret_rfe_ctrl_patientlevel,
     ## pass options to train()
     method = "xgbTree",
     preProc = c("center", "scale"),
     metric = "ROC",
     tuneGrid = besttune_xgboost,
     scale_pos_weight = pos.weight,
     trControl = rfe_inner
   )
 stopCluster(cl)


## Assess performance ##
set.seed(42)
model_performance <- func_model_performance(model, dat, hic)


# get confidence intervals for performance statistics
library(future)
options(future.globals.maxSize= 1200 * 1024^2)
plan(multisession, workers = 14)
set.seed(42)
model_cis <-future.apply::future_replicate(n=1000,
        jb_caret_performance(model, dat, hic, bootstrapping = T),
        simplify = T)

plan(sequential)

