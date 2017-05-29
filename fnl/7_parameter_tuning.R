library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_scale_20170528_stacking.RData")
load(file = "./datathon2017/final_features.RData")

# Predictors and Response -------------------------------------------------
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
response = 'response'

# xgboost -----------------------------------------------------------------
setDT(fnl.dat)
training = fnl.dat[Patient_ID <= 279201]
rm(fnl.dat); gc()
library(xgboost)

set.seed(8)
ss = 0.5
idx = sample(1:nrow(training), ss * nrow(training))
trainBC = as.data.frame(training[-idx])
validationBC = as.data.frame(training[idx])
dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
watchlist <- list(train = dtrain, eval = dval)

gamma = c(0, 0.1, 0.01, 1, 0.5) # 0.971080, 0.971122, 0.971183, 0.971056, 0.971145
max_depth = c(6, 8, 10, 12) # 0.971245, 0.971023, 0.970911, 0.970796
min_child_weight = c(8, 12, 16, 20) # 0.971191, 0.971147, 0.971112, 0.971182
subsample = c(0.3, 0.6, 0.8) # 0.970668, 0.971061, 0.971148
colsample_bytree = c(0.15, 0.5, 0.75) # 0.971197, 0.971130, 0.971191
lambda = c(1e-5, 1e-2, 0.1, 1, 100) # 0.970804, 0.970827, 0.970754, 0.970932, 0.971044
alpha = c(1e-5, 1e-2, 0.1, 1, 100) # 0.971072, 0.971184, 0.971111, 0.971071, 0.971002
scale_pos_weight = c(1, 2, 3) # Neg / Pos (4) # 0.971055, 0.971101
max_delta_step = c(0, 1, 2, 4) # 0.971131, 0.971265, 0.971185, 0.971046

for(i in max_delta_step){
    print(i)
    gc()
    param <- list(
        max_depth = 6,
        eta = 0.1,
        nthread = 7,
        objective = "binary:logistic",
        eval_metric = "rmse",
        eval_metric = "auc",
        booster = "gbtree",
        gamma = 0.01,
        min_child_weight = 8,
        subsample = 0.8,
        colsample_bytree = 0.15,
        lambda = 100,
        alpha = 1e-2,
        scale_pos_weight = 4,
        # max_delta_step = 1,
        seed = 1989
    )
    xgbFit <- xgb.train(param,dtrain,nrounds = 2000,watchlist,print_every_n = 50,
                        early_stopping_rounds = 50,verbose = 1)
    var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
    write.csv(var.imp, file = paste0("./featureImportance_Fnl_", i, ".csv"), row.names = F)
}