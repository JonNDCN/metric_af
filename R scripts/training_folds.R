## Development data training folds

### Manual folds based on admission unit ###
index1 <- which(dat$first_careunit!="Oxford AICU")
index2 <- which(dat$first_careunit!="Oxford Churchill")
index3 <- which(dat$first_careunit!="Reading ICU")
index4 <- which(dat$first_careunit!="Medical Intensive Care Unit (MICU)")
index5 <- which(dat$first_careunit!="Medical/Surgical Intensive Care Unit (MICU/SICU)")
index6 <- which(dat$first_careunit!="Surgical Intensive Care Unit (SICU)")
index7 <- which(dat$first_careunit!="Trauma SICU (TSICU)")
unit_indices <- list(index1, index2, index3, index4, index5, index6, index7)

datunitfolds <- c(unit_indices, unit_indices, unit_indices, unit_indices, unit_indices)




#### Manual opposing folds for rfe with one row per patient within each fold
set.seed(4)
datunitidfold1out <- lapply(unit_indices, function(x, data)
{data[-x,] %>% group_by(stay_id) %>% slice_sample(n=1) %>% pull(rn)},
data = dat
)
datunitidfold2out <- lapply(unit_indices, function(x, data)
{data[-x,] %>% group_by(stay_id) %>% slice_sample(n=1) %>% pull(rn)},
data = dat
)
datunitidfold3out <- lapply(unit_indices, function(x, data)
{data[-x,] %>% group_by(stay_id) %>% slice_sample(n=1) %>% pull(rn)},
data = dat
)
datunitidfold4out <- lapply(unit_indices, function(x, data)
{data[-x,] %>% group_by(stay_id) %>% slice_sample(n=1) %>% pull(rn)},
data = dat
)
datunitidfold5out <- lapply(unit_indices, function(x, data)
{data[-x,] %>% group_by(stay_id) %>% slice_sample(n=1) %>% pull(rn)},
data = dat
)

datunitidfoldsout <- c(datunitidfold1out, datunitidfold2out, datunitidfold3out, datunitidfold4out, datunitidfold5out)
