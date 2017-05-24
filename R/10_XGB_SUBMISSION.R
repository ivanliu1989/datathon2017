# Cross Validation --------------------------------------------------------
# trainIndex <- createFolds(trainBC$response, k = 5, list = FALSE)
# head(trainIndex)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_scale_20170524_nvz.RData")
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
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



# xgboost -----------------------------------------------------------------
setDT(fnl.dat)
training = fnl.dat[Patient_ID <= 279201]
test = fnl.dat[Patient_ID > 279201]
rm(fnl.dat); gc()
library(xgboost)

xgbFit.all = list()
for(i in 1:10){
    set.seed(i)
    idx = sample(1:nrow(training), 0.5 * nrow(training))
    trainBC = convertNum(as.data.frame(training[-idx]))
    validationBC = convertNum(as.data.frame(training[idx]))
    
    predictors =colnames(trainBC)[!colnames(trainBC) %in% c('Patient_ID','response')]
    response = 'response'
    dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
    dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
    watchlist <- list(train = dtrain, eval = dval)
    
    param <- list(
        max_depth = 6,
        eta = 0.01,
        nthread = 6,
        objective = "binary:logistic", #"reg:linear",
        eval_metric = "auc",
        eval_metric = "rmse",
        booster = "gbtree",
        # booster = "gblinear",
        gamma = 0.01,
        min_child_weight = 10,
        subsample = 0.8,
        colsample_bytree = 0.15
    )
    xgbFit <- xgb.train(param,dtrain,nrounds = 10000,watchlist,print_every_n = 50,
                        early_stopping_rounds = 20,verbose = 1)
    
    xgbFit.all[[i]] = xgbFit
}


dtesting <- xgb.DMatrix(data.matrix(convertNum(test[, predictors])), label = test[, response])
pred.all <- list()

for(i in 1:length(xgbFit.all)){
    pred.full = predict(xgbFit.all[[i]], dtesting)
    pred.all[[i]] = pred.full
}




# Submissions -------------------------------------------------------------
library(data.table)
submissions = fread("./datathon2017/data/diabetes_submission_example.csv")
submit_score = data.table(Patient_ID = test$Patient_ID, Diabetes = pred.full)

options(scipen = 3)
submissions = merge(submissions, submit_score, by = "Patient_ID", all.x = T)
submissions[, Diabetes.x := NULL]
setnames(submissions, c("Patient_ID", "Diabetes"))
submissions[is.na(Diabetes), Diabetes := 0.000001]
submissions[, Patient_ID := as.character(Patient_ID)]

write.csv(submissions, file = "submission20170524_09559_09534_gblinear.csv", row.names = F)


submissions_no1 = fread("./submission/submission20170508.csv")
submissions_no2 = fread("./submission20170514_0975_09668.csv")
table(ifelse(submissions_no2$Diabetes >= 0.5, 1,0) == ifelse(submissions$Diabetes >= 0.5, 1, 0))
table(ifelse(submissions_no2$Diabetes >= 0.5, 1,0) == ifelse(submissions_no1$Diabetes >= 0.5, 1, 0))

