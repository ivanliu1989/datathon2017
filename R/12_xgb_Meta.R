library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
# load(file = "./modelData/feat_all_extra_imputed_cleaned_pca_0527.RData")
load(file = "./modelData/feat_all_scale_20170525_fix_all_extra_imputed_cleaned.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all_extra.RData")
setDT(fnl.dat)
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
predictors =colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]
response = 'response'

# xgboost -----------------------------------------------------------------
setDT(fnl.dat)
testing = fnl.dat[Patient_ID > 279201]
# training = fnl.dat[Patient_ID <= 279201]
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
xgbFit1 <- xgb.train(param,dtrain,nrounds = 60,watchlist,print_every_n = 5, verbose = 1)
var.imp = xgb.importance(colnames(dtrain), model = xgbFit1)
write.csv(var.imp, file = paste0("./featureImportance_xgbTreeImputed_meta1.csv"))
# save(xgbFit, file = paste0("./xgb_meta_1.RData"))
gc()
featStacking1 = predict(xgbFit1, dval)
featStacking1 = data.frame(Patient_ID = validationBC$Patient_ID, xgbTreeImputed = featStacking1)

# Part B ------------------------------------------------------------------
watchlist <- list(train = dval, eval = dtrain)
xgbFit2 <- xgb.train(param,dval,nrounds = 60,watchlist,print_every_n = 5, verbose = 1)
var.imp = xgb.importance(colnames(dtrain), model = xgbFit2)
write.csv(var.imp, file = paste0("./featureImportance_xgbTreeImputed_meta2.csv"))
gc()
featStacking2 = predict(xgbFit2, dtrain)
featStacking2 = data.frame(Patient_ID = trainBC$Patient_ID, xgbTreeImputed = featStacking2)

xgbTreeImputed = rbind(featStacking1, featStacking2)
save(xgbTreeImputed, file = "./modelData/Metadata/xgbTreeImputed.RData")
save(predictors, xgbFit1, xgbFit2, file = "./modelData/Metadata/xgbTreeImputed_XGBModel_Feat.RData")
gc()


# Test --------------------------------------------------------------------
rm(training); rm(trainBC); rm(validationBC); rm(dtrain); rm(dval); rm(watchlist); rm(var.imp); gc()
load(file = "./modelData/Metadata/xgbTreeImputed_XGBModel_Feat.RData")
load(file = "./modelData/Metadata/xgbTreeImputed.RData")
setDF(testing)
dtrain <- xgb.DMatrix(data.matrix(testing[, predictors]))
gc()
featStackingTest1 = predict(xgbFit1, dtrain)
featStackingTest2 = predict(xgbFit2, dtrain)
xgbTreeImputedTest = data.frame(Patient_ID = testing$Patient_ID, xgbTreeImputed = (featStackingTest1 + featStackingTest2)/2)
tail(xgbTreeImputedTest); head(xgbTreeImputedTest)

xgbTreeImputed = rbind(xgbTreeImputed, xgbTreeImputedTest)
save(xgbTreeImputed, file = "./modelData/Metadata/xgbTreeImputed.RData")




