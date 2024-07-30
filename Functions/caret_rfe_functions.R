

caret_rfe_functions <- list(summary = twoClassSummary,
                            fit = function (x, y, first, last, ...) {
                              train(x, y, ...)
                            },
                            pred = caretFuncs$pred,
                            rank = function(object, x, y) {
                              vimp <- varImp(object)$importance
                              vimp <- vimp[order(vimp$Overall,decreasing = TRUE),,drop = FALSE]
                              vimp$var <- rownames(vimp)                  
                              vimp
                            },
                            selectSize = function (x, metric = "ROC", tol = 1, maximize = TRUE) 
                            {
                              if (!maximize) {
                                best <- min(x[, metric])
                                perf <- (x[, metric] - best)/best * 100
                                flag <- perf <= tol
                              }
                              else {
                                best <- max(x[, metric])
                                perf <- (best - x[, metric])/best * 100
                                flag <- perf <= tol
                              }
                              min(x[flag, "Variables"])
                            },
                            selectVar = caretFuncs$selectVar)

### This one has a different rank function which works with the MLP
caret_rfe_functions2 <- list(summary = twoClassSummary,
                             fit = function (x, y, first, last, ...) {
                               train(x, y, ...)
                             },
                             pred = caretFuncs$pred,
                             rank = caretFuncs$rank,
                             selectSize = function (x, metric = "ROC", tol = 1, maximize = TRUE) 
                             {
                               if (!maximize) {
                                 best <- min(x[, metric])
                                 perf <- (x[, metric] - best)/best * 100
                                 flag <- perf <= tol
                               }
                               else {
                                 best <- max(x[, metric])
                                 perf <- (best - x[, metric])/best * 100
                                 flag <- perf <= tol
                               }
                               min(x[flag, "Variables"])
                             },
                             selectVar = caretFuncs$selectVar)


caret_rfe_ctrl <- rfeControl(
  functions = caret_rfe_functions,
  saveDetails = TRUE,
  index = unit_indices,
  indexOut = NULL,
  returnResamp = "all",
  allowParallel = T,
  verbose = TRUE
)

caret_rfe_ctrl_patientlevel <- rfeControl(
  functions = caret_rfe_functions,
  saveDetails = TRUE,
  index = datunitfolds,
  indexOut = datunitidfoldsout,
  returnResamp = "all",
  allowParallel = T,
  verbose = TRUE
)

caret_rfe_ctrl_patientlevel_rerank <- rfeControl(
  functions = caret_rfe_functions,
  saveDetails = TRUE,
  index = datunitfolds,
  indexOut = datunitidfoldsout,
  returnResamp = "all",
  allowParallel = T,
  verbose = TRUE,
  rerank = T
)

caret_rfe_ctrl_patientlevel2 <- rfeControl(
  functions = caret_rfe_functions2,
  saveDetails = TRUE,
  index = datunitfolds,
  indexOut = datunitidfoldsout,
  returnResamp = "all",
  allowParallel = T,
  verbose = TRUE
)

caret_rfe_ctrl_patientlevel2_rerank <- rfeControl(
  functions = caret_rfe_functions2,
  saveDetails = TRUE,
  index = datunitfolds,
  indexOut = datunitidfoldsout,
  returnResamp = "all",
  allowParallel = T,
  verbose = TRUE,
  rerank = T
)



