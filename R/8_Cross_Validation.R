# Cross Validation --------------------------------------------------------
# trainIndex <- createFolds(trainBC$response, k = 5, list = FALSE)
# head(trainIndex)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_scale_20170524_nvz.RData")
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
# save(fnl.dat, file = "./modelData/feat_all_scale_20170524_nvz.RData")

# PCA ---------------------------------------------------------------------
# library(caret)
# nzv <- nearZeroVar(as.data.frame(fnl.dat)[,!colnames(fnl.dat) %in% c('Patient_ID','response')], saveMetrics= TRUE)
# toDel = rownames(nzv[nzv$zeroVar,])
# fnl.dat[, (toDel):=NULL]
# for(i in 1:ncol(fnl.dat)){
#     print(nrow(unique(fnl.dat[,i, with = F])))
# }
# 
repNaN = function(x, rep = NA){
    x[,lapply(.SD,function(x){ifelse(is.nan(x),rep,x)})]    
}
repNA = function(x, rep = NA){
    x[,lapply(.SD,function(x){ifelse(is.na(x),rep,x)})]    
}
convertNum = function(x){
    for(i in 1:ncol(x)){
        x[, i] = as.numeric(x[, i])
    }
    x
}
fnl.dat = repNaN(fnl.dat, 0)
fnl.dat = repNA(fnl.dat, 0)
predictors =colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]
response = 'response'
fnl.dat = as.data.frame(fnl.dat)
fnl.dat = convertNum(fnl.dat)
gc()
pp_hpc <- preProcess(fnl.dat[, predictors], method = c("pca"))#
# pp_hpc <- prcomp(fnl.dat[, predictors], scale. = F, center = F)
# validationBC = convertNum(as.data.frame(training[idx]))
# testBC = trainBC[idx2,]
# trainBC = trainBC[-idx2,]
# 
# transformed.train <- predict(pp_hpc, newdata = trainBC[, predictors])
# transformed.test <- predict(pp_hpc, newdata = testBC[, predictors])
# transformed.val <- predict(pp_hpc, newdata = validationBC[, predictors])
# head(transformed.train)
# trainBC = data.table(Patient_ID = trainBC$Patient_ID, transformed.train, response = trainBC$response)
# testBC = data.table(Patient_ID = testBC$Patient_ID, transformed.test, response = testBC$response)
# validationBC = data.table(Patient_ID = validationBC$Patient_ID, transformed.val, response = validationBC$response)
# colnames(validationBC) <- c('Patient_ID', paste0('PC',1:638), 'response')



# xgboost -----------------------------------------------------------------
setDT(fnl.dat)
training = fnl.dat[Patient_ID <= 279201]
test = fnl.dat[Patient_ID > 279201]
rm(fnl.dat); gc()

library(xgboost)
idx = sample(1:nrow(training), 0.5 * nrow(training))
trainBC = convertNum(as.data.frame(training[-idx]))
validationBC = convertNum(as.data.frame(training[idx]))
# idx2 = sample(1:nrow(trainBC), 0.5 * nrow(trainBC))
# testBC = trainBC[idx2,]
# trainBC = trainBC[-idx2,]

predictors =colnames(trainBC)[!colnames(trainBC) %in% c('Patient_ID','response')]
# predictors =var.imp$Feature
response = 'response'
dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
# dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
watchlist <- list(train = dtrain, eval = dval)

param <- list(
    max_depth = 6,
    eta = 0.01,
    nthread = 6,
    objective = "binary:logistic", #"reg:linear",
    eval_metric = "auc",
    eval_metric = "rmse",
    # booster = "gbtree",
    booster = "gblinear",
    gamma = 0.01,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.5
)
xgbFit <- xgb.train(param,dtrain,nrounds = 10000,watchlist,print_every_n = 50,
                    early_stopping_rounds = 20,verbose = 1)

var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
# write.csv(var.imp, file = './feat_imp2.csv', row.names = F)
xgb.plot.importance(var.imp[1:20,])
val = predict(xgbFit, dtest)
val = predict(xgbFit, dtest, ntreelimit = F)
val.f = ifelse(val>= 0.5, 1, 0); table(val.f)
caret::confusionMatrix(val.f,testBC$response)
pROC::roc(testBC$response, val) 

# All Feature: 0.9748/0.9662/Specificity : 0.8966 | 0.15 | 975 features
# All Feature: 0.9745/0.9656/Specificity : 0.8960 | 0.05 | 1136 features

# All Feature: 0.975/0.9666/Specificity : 0.8956 | 0.15 | 992 features
# Top Feature: 0.975/0.9668/Specificity : 0.8960 | 0.15 | 992 features

# 20170524
# All 0.9727
# Top 0.9729

# xgbFit.full = list(xgbFit = xgbFit,
#                    predictors = predictors)
xgbFit.top = list(xgbFit = xgbFit,
                  predictors = predictors)
# save(xgbFit.full, xgbFit.top, file="./modelData/xgb20170514.RData")
# rm(dtrain); rm(dval); rm(dtest); rm(trainBC); rm(validationBC); rm(testBC); gc()
test = convertNum(as.data.frame(test))
dtesting <- xgb.DMatrix(data.matrix(test[, xgbFit.top$predictors]), label = test[, response])
pred.full = predict(xgbFit.top$xgbFit, dtesting)

# Submission
library(data.table)
submissions = fread("./datathon2017/data/diabetes_submission_example.csv")
submit_score = data.table(Patient_ID = test$Patient_ID, Diabetes = pred.full)

options(scipen = 3)
submissions = merge(submissions, submit_score, by = "Patient_ID", all.x = T)
submissions[, Diabetes.x := NULL]
setnames(submissions, c("Patient_ID", "Diabetes"))
submissions[is.na(Diabetes), Diabetes := 0.000001]
# submissions[is.na(Diabetes), Diabetes := 0.001]
submissions[, Patient_ID := as.character(Patient_ID)]
write.csv(submissions, file = "submission20170524_09559_09534_gblinear.csv", row.names = F)
# lstsub = fread("./submission/submission20170508.csv")

# a = merge(submissions, lstsub, by = 'Patient_ID')
# a[, lst := ifelse(Diabetes.y >= 0.5, 1,0)]
# a[, this := ifelse(Diabetes.x >= 0.5, 1,0)]
# table(a[Diabetes.y == 0.00001]$this == a[Diabetes.y == 0.00001]$lst)

submissions_no1 = fread("./submission/submission20170508.csv")
submissions_no2 = fread("./submission20170514_0975_09668.csv")
table(ifelse(submissions_no2$Diabetes >= 0.5, 1,0) == ifelse(submissions$Diabetes >= 0.5, 1, 0))
table(ifelse(submissions_no2$Diabetes >= 0.5, 1,0) == ifelse(submissions_no1$Diabetes >= 0.5, 1, 0))


# Cross validation with predictions for each folder -----------------------
gc()
training = convertNum(as.data.frame(training))
predictors =colnames(training)[!colnames(training) %in% c('Patient_ID','response')]
response = 'response'
training <- xgb.DMatrix(data.matrix(training[, predictors]), label = training[, response])
param <- list(
    max_depth = 6,
    eta = 0.01,
    nthread = 8,
    objective = "binary:logistic", #"reg:linear",
    eval_metric = "auc",
    eval_metric = "rmse",
    booster = "gbtree",
    gamma = 0.01,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.15
)
res <- xgb.cv(params = param, data = training, nfold = 5, nrounds = 10000,
              prediction = TRUE, showsd = TRUE, 
              stratified = TRUE, early_stopping_rounds = 10)
