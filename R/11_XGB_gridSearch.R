library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_scale_20170525_fix_all.RData")
load(file = "./modelData/feat_all_scale_20170525_fix_all_impute.RData")
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
convertNum = function(x){
    for(i in 1:ncol(x)){
        x[, i] = as.numeric(x[, i])
    }
    x
}

# Feature Selection -------------------------------------------------------
lst.files = list.files("./featImp", full.name = T)
impFeatures = rbindlist(lapply(lst.files, function(x) fread(x)))
predictors =unique(impFeatures$Feature)
# predictors =colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]
response = 'response'

# xgboost -----------------------------------------------------------------
setDT(fnl.dat)
training = fnl.dat[Patient_ID <= 279201]
rm(fnl.dat); gc()
library(xgboost)

trainPerc = 0.2
set.seed(5)
idx = sample(1:nrow(training), trainPerc * nrow(training))
trainBC = convertNum(as.data.frame(training[-idx])); gc()
validationBC = convertNum(as.data.frame(training[idx]))
rm(training); gc()

dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
watchlist <- list(train = dtrain, eval = dval)

# Grid Search -------------------------------------------------------------
xgb_grid = expand.grid(
    eta = c(0.01, 0.001, 0.0001),
    max_depth = c(4, 6, 8, 10),
    subsample = c(0.6, 0.7, 0.8, 0.9, 1),
    colsample_bytree = c(0.15, 0.3, 0.6, 0.9)
)
xgb_grid$score = NA
xgb_grid$rnd = NA

for(i in 1:nrow(xgb_grid)){
    set.seed(1)
    param <- list(
        max_depth = xgb_grid[i, 'max_depth'],
        eta = xgb_grid[i, 'eta'],
        nthread = 6,
        objective = "binary:logistic",
        eval_metric = "auc",
        # eval_metric = "rmse",
        booster = "gbtree",
        gamma = 0.01,
        min_child_weight = 1,
        subsample = xgb_grid[i, 'subsample'],
        colsample_bytree = xgb_grid[i, 'colsample_bytree']
    )
    
    xgbFit <- xgb.train(param,dtrain,nrounds = 10000,watchlist,print_every_n = 200,
                        early_stopping_rounds = 20,verbose = 0)
    
    xgb_grid[i, "score"] = as.numeric(xgbFit$best_score)
    xgb_grid[i, "rnd"] = as.numeric(xgbFit$best_iteration)
    
    print(as.numeric(xgbFit$best_score))
    var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
    write.csv(var.imp, file = paste0("./featureImportance_GS_", i, ".csv"))
    gc()
}