# Cross Validation --------------------------------------------------------
# trainIndex <- createFolds(trainBC$response, k = 5, list = FALSE)
# head(trainIndex)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_scale_20170514.RData")
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]

# xgboost -----------------------------------------------------------------
training = fnl.dat[Patient_ID <= 279201]
# test = fnl.dat[Patient_ID > 279201]
rm(fnl.dat); gc()
convertNum = function(x){
    for(i in 1:ncol(x)){
        x[, i] = as.numeric(x[, i])
    }
    x
}
library(xgboost)
idx = sample(1:nrow(training), 0.2 * nrow(training))
trainBC = convertNum(as.data.frame(training[-idx]))
validationBC = convertNum(as.data.frame(training[idx]))
idx2 = sample(1:nrow(trainBC), 0.5 * nrow(trainBC))
testBC = trainBC[idx2,]
trainBC = trainBC[-idx2,]

predictors =colnames(trainBC)[!colnames(trainBC) %in% c('Patient_ID','response')]
predictors =var.imp$Feature
response = 'response'
dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
watchlist <- list(train = dtrain, eval = dval)


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
    colsample_bytree = 0.5
)
xgbFit <- xgb.train(param,dtrain,nrounds = 10000,watchlist,print_every_n = 50,
                    early_stopping_rounds = 20,verbose = 1)

var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
write.csv(var.imp, file = './feat_imp2.csv', row.names = F)
xgb.plot.importance(var.imp[1:20,])
val = predict(xgbFit, dtest)
val.f = ifelse(val>= 0.5, 1, 0); table(val.f)
caret::confusionMatrix(val.f,testBC$response)
pROC::roc(testBC$response, val) 

# All Feature: 0.9748/0.9662/Specificity : 0.8966 | 0.15 | 975 features
# All Feature: 0.9745/0.9656/Specificity : 0.8960 | 0.05 | 1136 features

# All Feature: 0.975/0.9666/Specificity : 0.8956 | 0.15 | 992 features
# Top Feature: 0.975/0.9668/Specificity : 0.8960 | 0.15 | 992 features

xgbFit.full = list(xgbFit = xgbFit,
                   predictors = predictors)
xgbFit.top = list(xgbFit = xgbFit,
                  predictors = predictors)
save(xgbFit.full, xgbFit.top, file="./modelData/xgb20170514.RData")
rm(dtrain); rm(dval); rm(dtest); rm(trainBC); rm(validationBC); rm(testBC); gc()
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
write.csv(submissions, file = "submission20170514_0975_09668.csv", row.names = F)
# lstsub = fread("./submission/submission20170508.csv")

# a = merge(submissions, lstsub, by = 'Patient_ID')
# a[, lst := ifelse(Diabetes.y >= 0.5, 1,0)]
# a[, this := ifelse(Diabetes.x >= 0.5, 1,0)]
# table(a[Diabetes.y == 0.00001]$this == a[Diabetes.y == 0.00001]$lst)



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
