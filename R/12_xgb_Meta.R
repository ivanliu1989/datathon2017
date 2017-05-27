# Cross Validation --------------------------------------------------------
# trainIndex <- createFolds(trainBC$response, k = 5, list = FALSE)
# head(trainIndex)
library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_extra_imputed_cleaned_pca_0527.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all_extra_imputed_cleaned.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all_extra.RData")
setDT(fnl.dat)
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
predictors =colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]
response = 'response'

# xgboost -----------------------------------------------------------------
setDT(fnl.dat)
training = fnl.dat[Patient_ID <= 279201]
rm(fnl.dat); gc()
library(xgboost)

set.seed(1989)
idx = 1:139600
trainBC = as.data.frame(training[-idx])
validationBC = as.data.frame(training[idx])
dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
watchlist <- list(train = dtrain, eval = dval)
gc()
# Meta 1
param <- list(
    max_depth = 6,
    eta = 0.1,
    nthread = 7,
    objective = "binary:logistic",
    eval_metric = "auc",
    eval_metric = "rmse",
    booster = "gbtree",
    # booster = "gblinear",
    gamma = 1,
    min_child_weight = 20,
    subsample = 0.8,
    colsample_bytree = 0.7
)


# Part A ------------------------------------------------------------------
xgbFit1 <- xgb.train(param,dtrain,nrounds = 100,watchlist,print_every_n = 5, verbose = 1)
var.imp = xgb.importance(colnames(dtrain), model = xgbFit1)
write.csv(var.imp, file = paste0("./featureImportance_pca_meta1.csv"))
# save(xgbFit, file = paste0("./xgb_meta_1.RData"))
gc()
featStacking1 = predict(xgbFit1, dval)
featStacking1 = data.frame(Patient_ID = validationBC$Patient_ID, xgbTreePCA = featStacking1)

# Part B ------------------------------------------------------------------
watchlist <- list(train = dval, eval = dtrain)
xgbFit2 <- xgb.train(param,dval,nrounds = 100,watchlist,print_every_n = 5, verbose = 1)
var.imp = xgb.importance(colnames(dtrain), model = xgbFit2)
write.csv(var.imp, file = paste0("./featureImportance_pca_meta2.csv"))
gc()
featStacking2 = predict(xgbFit1, dtrain)
featStacking2 = data.frame(Patient_ID = trainBC$Patient_ID, xgbTreePCA = featStacking1)

xgbTreePCA = rbind(featStacking1, featStacking2)
save(xgbTreePCA, file = "./data/Metadata/xgbTreePCA.RData")















# Test --------------------------------------------------------------------
param <- list(
    max_depth = 6,
    eta = 0.1,
    nthread = 7,
    objective = "binary:logistic",
    eval_metric = "auc",
    eval_metric = "rmse",
    booster = "gbtree",
    # booster = "gblinear",
    gamma = 1,
    min_child_weight = 20,
    subsample = 0.8,
    colsample_bytree = 0.7
)
setDF(training)
dtrain <- xgb.DMatrix(data.matrix(training[, predictors]), label = training[, response])
xgbFit <- xgb.train(param,dtrain,nrounds = 100,print_every_n = 5, verbose = 1)

rm(training);
load(file = "./modelData/feat_all_extra_imputed_cleaned_pca_0527.RData")
predictors =colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]
response = 'response'
testing = fnl.dat[Patient_ID > 279201]
rm(fnl.dat); gc()
setDF(testing)
dtest <- xgb.DMatrix(data.matrix(testing[, predictors]), label = testing[, response])
featStackingTest = predict(xgbFit1, dtrain)
xgbTreePCATest = data.frame(Patient_ID = testing$Patient_ID, xgbTreePCA = featStackingTest)

