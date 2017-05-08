rm(list = ls()); gc()
trans = fread('./data/transactions_all.csv')
load("./data/meta.RData")


# Split data --------------------------------------------------------------
training = trans[as.Date(Prescription_Week) < as.Date("2015-01-01")]
test = trans[as.Date(Prescription_Week) >= as.Date("2016-01-01")]
validation = trans[as.Date(Prescription_Week) < as.Date("2016-01-01") & as.Date(Prescription_Week) >= as.Date("2015-01-01")]
rm(trans); gc()


# Transactional features --------------------------------------------------
training = merge(training, chron, by.x = "Drug_ID", by.y = "MasterProductID", all.x = TRUE)
test = merge(test, chron, by.x = "Drug_ID", by.y = "MasterProductID", all.x = TRUE)
validation = merge(validation, chron, by.x = "Drug_ID", by.y = "MasterProductID", all.x = TRUE)
featureEngineeringMain = function(x){
    x[, ChronicIllness := ifelse(is.na(ChronicIllness), "Others", ChronicIllness)]
    x[, Drug_Cnt := .N, by = c("Patient_ID", "ChronicIllness")]
    x = unique(x[, .(Patient_ID, ChronicIllness, Drug_Cnt)])
    x = dcast(x, Patient_ID ~ ChronicIllness, value.var = "Drug_Cnt")
    x[, target := ifelse(Diabetes >0, 1,0)]
    x[, target := ifelse(is.na(target), 0,target)]
    x[, Diabetes := NULL]
    return(x)
}

training.new = featureEngineeringMain(training)
test.new = featureEngineeringMain(test[Patient_ID <279201])
validation.new = featureEngineeringMain(validation)



# xgboost -----------------------------------------------------------------
library(xgboost)
trainBC = as.data.frame(training.new[, -1])
testBC = as.data.frame(test.new[, -1])
validationBC = as.data.frame(validation.new[, -1])
for(i in 1:ncol(testBC)){
    testBC[, i] = as.numeric(testBC[, i])
}

predictors = names(trainBC)[!names(trainBC) %in% c('target')]
response = 'target'
dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
watchlist <- list(train = dtrain, eval = dval)

param <- list(
    max_depth = 3,
    eta = 0.01,
    nthread = 6,
    objective = "binary:logistic", #"reg:linear", 
    eval_metric = "rmse",
    # eval_metric = "auc",
    booster = "gbtree",
    gamma = 0.01,
    min_child_weight = 1,
    subsample = 0.7,
    colsample_bytree = 1
)
xgbFit <- xgb.train(param,dtrain,nrounds = 1000,watchlist,
                    early_stopping_rounds = 100,verbose = 1)

var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
xgb.plot.importance(var.imp[1:20,])
val = predict(xgbFit, dtest)
xgbRes = binaryClassifierEvaluation(val, testBC$target)
xgbRes$summaryStats
# 0.683 auc
# 0.591 accuracy




# 1	Kurt/Skew of different drug
# 2	Kurt/Skew of total drugs/unique drugs
# 3	Illness number
# 4	Ingradient number
# 5	Drug & drug combination
# 6	Illness & illness combination
# 7	repeats interval
# 8	last buy durgs / illness
# 9	Prices, reclain of different drugs, illness
# 10	# of drugs, illness in the same prescription
# 11	1st-2nd drugs combinations
# 
# 12	Manufacturer #
# 13	Prescriber #
# 14	Gender, age, dosage index
