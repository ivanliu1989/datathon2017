rm(list = ls()); gc()
library(data.table)
# library(doParallel)
# library(doMC)
# registerDoMC(cores = 6)
# setDTthreads(8)
# getDTthreads()
# trans = fread('./data/transactions_all.csv')
load("./data/meta.RData")
source("./datathon2017/R/featureEngineeringMain.R")

# 0. General --------------------------------------------------------------
# trans[, Dispense_Week := as.Date(Dispense_Week)]
# trans[, Year := year(Dispense_Week)]
# trans[, Month := month(Dispense_Week)]
# trans[, Qtr := as.numeric(paste0(Year,ceiling(Month/3)))]
# trans[, Prescription_ID := paste0(Patient_ID, Prescriber_ID, as.numeric(as.Date(Prescription_Week)))]
# pres = unique(trans$Prescription_ID)
# pres = data.table(Prescription_ID = pres, Presc_ID = 1:length(pres))
# trans = merge(trans, pres, by = "Prescription_ID", all.x = TRUE)
# rm(pres); trans[,Prescription_ID := NULL]
# trans[, Presc_Itm_ID := paste0(Presc_ID, Drug_ID)]
# trans = merge(trans, chron[, .(ChronicIllness,MasterProductID)], by.x = "Drug_ID", by.y = "MasterProductID", all.x = TRUE)
# trans = merge(trans, drug[,.(MasterProductID, ATCLevel3Code)], by.x = "Drug_ID", by.y = "MasterProductID", all.x = TRUE)
# trans[ChronicIllness =="Chronic Obstructive Pulmonary Disease (COPD)", ChronicIllness := "COPD"]
# trans[is.na(ChronicIllness), ChronicIllness := 'Others']
# save(trans, file = "./5_xgb_model.RData")
load(file = "./5_xgb_model.RData")
# Sample
txns = copy(trans[Patient_ID <= 279201]) #  & ChronicIllness != "Others"
# txns.text = copy(trans[Patient_ID > 279201 & ChronicIllness != "Others"])
txns = txns[Patient_ID %in% sample(unique(txns$Patient_ID), 100000)]
gc(); 

# 27 to 
tmp_outcomes2016 = unique(txns[Dispense_Week >= as.Date("2016-01-01") & ChronicIllness == "Diabetes", Patient_ID])
training = generateHistFeatures(txns[!(Dispense_Week >= as.Date("2016-01-01") & ChronicIllness == "Diabetes")], lstTrans = as.Date(max(txns$Dispense_Week)), cores = 2)
# test = generateHistFeatures(txns.text[!(Dispense_Week >= as.Date("2016-01-01") & ChronicIllness == "Diabetes")], lstTrans = as.Date("2016-01-01"))
training[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
# save(training,test, file ="./xgboost_training.RData")

# xgboost -----------------------------------------------------------------
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
idx2 = sample(1:nrow(trainBC), 0.3 * nrow(trainBC))
testBC = trainBC[idx2,]
trainBC = trainBC[-idx2,]

# predictors =intersect(colnames(training), colnames(test))[!intersect(colnames(training), colnames(test)) %in% c('Patient_ID','response')]
predictors =colnames(training)[!colnames(training) %in% c('Patient_ID','response')]
response = 'response'
dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
watchlist <- list(train = dtrain, eval = dval)

param <- list(
    max_depth = 6,
    eta = 0.001,
    nthread = 7,
    objective = "binary:logistic", #"reg:linear",
    eval_metric = "auc",
    eval_metric = "rmse",
    booster = "gbtree",
    gamma = 0.001,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.15
)
xgbFit <- xgb.train(param,dtrain,nrounds = 10000,watchlist,
                    early_stopping_rounds = 20,verbose = 1)

var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
xgb.plot.importance(var.imp[1:20,])
val = predict(xgbFit, dtest)
val.f = ifelse(val>= 0.5, 1, 0); table(val.f)
table(val.f == testBC$response)[2]/sum(table(val.f == testBC$response)) 

caret::confusionMatrix(val.f,testBC$response)
# ModelMetrics::confusionMatrix(testBC$response, val, cutoff = 0.5)
pROC::roc(testBC$response, val) 





# Validate using basic algo
benchmark = unique(trans[Patient_ID <= 279201 & ChronicIllness == "Diabetes" & Dispense_Week < as.Date("2016-01-01"), Patient_ID])
setDT(testBC)
testBC[, bncmark := ifelse(Patient_ID %in% benchmark, 1, 0)]
confusionMatrix(testBC$bncmark,testBC$response)
pROC::roc(testBC$response, testBC$bncmark) 

# Submission --------------------------------------------------------------
idx = sample(1:nrow(training), 0.4 * nrow(training))
trainBC = convertNum(as.data.frame(training[-idx]))
validationBC = convertNum(as.data.frame(training[idx]))
predictors =intersect(colnames(training), colnames(test))[!intersect(colnames(training), colnames(test)) %in% c('Patient_ID','response')]
response = 'response'
dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
watchlist <- list(train = dtrain, eval = dval)

xgbFit <- xgb.train(param,dtrain,nrounds = 1000,watchlist,
                    early_stopping_rounds = 20,verbose = 1)


submissions = fread("./datathon2017/data/diabetes_submission_example.csv")
test[, response := 0]
submissionBC = convertNum(as.data.frame(test))
dtest <- xgb.DMatrix(data.matrix(submissionBC[, predictors]), label = submissionBC[, response])
submit_score = predict(xgbFit, data.matrix(submissionBC[, predictors]))
submit_score = data.table(Patient_ID = test$Patient_ID, Diabetes = submit_score)


submissions = merge(submissions, submit_score, by = "Patient_ID", all.x = T)
submissions[, Diabetes.x := NULL]
setnames(submissions, c("Patient_ID", "Diabetes"))
# submissions[is.na(Diabetes), Diabetes := 0.001]
write.csv(submissions.fnl, file = "submission20170508.csv", row.names = F)
# 0.173998 0.96414
# 0.173326/0.973297/0.174&0.97275/0.973304&0.172772

# benchmark = unique(trans[Patient_ID %in% unique(submissions$Patient_ID) & ChronicIllness == "Diabetes" & Dispense_Week < as.Date("2016-01-01"), Patient_ID])
# benchmark = data.table(Patient_ID = benchmark, Resp = NA)
# benchmark[, Resp := 1]
# submissions = merge(submissions, benchmark, by = "Patient_ID", all.x = T)
# 
# 
# submissions[,  Model := ifelse(Diabetes >= 0.5, 1,0)]
# submissions[,  Resp := ifelse(is.na(Resp) ,0,1)]
# table(submissions$Resp == submissions$Model)
# 
# 
# table(submissions[is.na(Diabetes), Resp])


# submissions.fnl = submissions
submissions.fnl = merge(submissions.fnl, submissions, by = "Patient_ID")
setnames(submissions.fnl, c("Patient_ID",  "d1",  "d2",  "d3",  "d4"))
submissions.fnl[, Diabetes := .20*d1 + 
                    .35*d2 + 
                    .10*d3 + 
                    .35*d4]
submissions.fnl[, d4:= NULL]
submissions.fnl[is.na(Diabetes), Diabetes:= 0.0001]
# param <- list(
#     max_depth = 8,
#     eta = 0.01,
#     nthread = 7,
#     objective = "binary:logistic", #"reg:linear",
#     eval_metric = "rmse",
#     # eval_metric = "auc",
#     booster = "gbtree",
#     gamma = 0.01,
#     min_child_weight = 5,
#     subsample = 0.7,
#     colsample_bytree = 0.15
# )



