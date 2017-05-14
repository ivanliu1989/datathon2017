# Cross Validation --------------------------------------------------------
trainIndex <- createFolds(trainBC$response, k = 5, list = FALSE)
head(trainIndex)

load(file = "./feat_all_scale_20170514.RData")


xgb.cv(param, dtrain, nround, nfold=5, metrics={'error'})