rm(list = ls()); gc()
library(data.table)
library(doParallel)
library(doMC)
registerDoMC(cores = 8)
setDTthreads(8)
getDTthreads()
# trans = fread('./data/transactions_all.csv')
load("./data/meta.RData")


##### 1	Kurt/Skew of different drugs / dosage
##### 2	Kurt/Skew of total drugs/unique drugs
##### 3	Illness number
##### 8	last buy durgs / illness
##### 10 # of drugs, illness in the same prescription
# 7	repeats interval
# 9	Prices, reclain of different drugs, illness
# 11 1st-2nd drugs combinations
# 
# 4	Ingradient number
# 5	Drug & drug combination
# 6	Illness & illness combination
# 12	Manufacturer #
# 13	Prescriber #
# 14	Gender, age, dosage index

# 17 Sequential association
# 15 System # (illness)
# 16 Ingredient Text Mining

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
gc()

generateHistFeatures = function(txns, lstTrans = as.Date("2016-01-01"), cores = 6){
    
    setorder(txns, Patient_ID, Dispense_Week)


    # 1. Kurt/Skew of drug qty ------------------------------------------------
    cat("\nFeature Sets 1...")
    library(moments)
    txns[, illSkew := skewness(Dispense_Week, na.rm = T), by = .(Patient_ID, ChronicIllness)]
    txns[, drugSkew := skewness(Dispense_Week, na.rm = T), by = .(Patient_ID, Drug_ID)]
    txns[, ATCSkew := skewness(Dispense_Week, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
    txns[, illHurt := kurtosis(Dispense_Week, na.rm = T), by = .(Patient_ID, ChronicIllness)]
    txns[, drugHurt := kurtosis(Dispense_Week, na.rm = T), by = .(Patient_ID, Drug_ID)]
    txns[, ATCHurt := kurtosis(Dispense_Week, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
    
    txns[, dosage := as.numeric((as.numeric(Dispense_Week - shift(Dispense_Week))/7)/(shift(RepeatsLeft_Qty)-RepeatsLeft_Qty)), by = Presc_Itm_ID]
    txns[, dosageIll := sum(dosage, na.rm = T), by = .(Patient_ID, Dispense_Week, ChronicIllness)]
    txns[, dosageATC := sum(dosage, na.rm = T), by = .(Patient_ID, Dispense_Week, ATCLevel3Code)]
    txns[, dosageQtrIll := sum(dosage, na.rm = T), by = .(Patient_ID, Qtr, ChronicIllness)]
    txns[, dosageQtrATC := sum(dosage, na.rm = T), by = .(Patient_ID, Qtr, ATCLevel3Code)]
    
    txns[, dosageIllSkew := skewness(dosageIll, na.rm = T), by = .(Patient_ID, ChronicIllness)]
    txns[, dosageIllHurt := kurtosis(dosageIll, na.rm = T), by = .(Patient_ID, ChronicIllness)]
    txns[, dosageATCSkew := skewness(dosageIll, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
    txns[, dosageATCHurt := kurtosis(dosageIll, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
    
    # generating features
    feat.p1 = copy(unique(txns[, .(Patient_ID, Qtr, Dispense_Week, Drug_ID, ChronicIllness, ATCLevel3Code,
                                   illSkew, drugSkew, ATCSkew, illHurt, drugHurt, ATCHurt, 
                                   dosageQtrIll, dosageQtrATC, 
                                   dosageIllSkew, dosageIllHurt, dosageATCSkew, dosageATCHurt)]))
    feat.p1[is.na(ChronicIllness), ChronicIllness:="Others"]
    f1.1 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "illSkew", fun.aggregate = mean, fill = NA); colnames(f1.1) = c("Patient_ID", paste0("f1.1_", colnames(f1.1[,-1,with = F])))
    # f2 = dcast(feat.p1, Patient_ID ~ Drug_ID, value.var = "drugSkew", fun.aggregate = mean, fill = NA)
    f1.3 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "ATCSkew", fun.aggregate = mean, fill = NA); colnames(f1.3) = c("Patient_ID", paste0("f1.3_", colnames(f1.3[,-1,with = F])))
    f1.4 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "illHurt", fun.aggregate = mean, fill = NA); colnames(f1.4) = c("Patient_ID", paste0("f1.4_", colnames(f1.4[,-1,with = F])))
    # f5 = dcast(feat.p1, Patient_ID ~ Drug_ID, value.var = "drugHurt", fun.aggregate = mean, fill = NA)
    f1.6 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "ATCHurt", fun.aggregate = mean, fill = NA); colnames(f1.6) = c("Patient_ID", paste0("f1.6_", colnames(f1.6[,-1,with = F])))
    
    f1.7 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "dosageQtrIll", fun.aggregate = mean, fill = 0); colnames(f1.7) = c("Patient_ID", paste0("f1.7_", colnames(f1.7[,-1,with = F])))
    f1.8 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "dosageQtrATC", fun.aggregate = mean, fill = 0); colnames(f1.8) = c("Patient_ID", paste0("f1.8_", colnames(f1.8[,-1,with = F])))
    
    f1.9 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "dosageIllSkew", fun.aggregate = mean, fill = NA); colnames(f1.9) = c("Patient_ID", paste0("f1.9_", colnames(f1.9[,-1,with = F])))
    f1.10 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "dosageIllHurt", fun.aggregate = mean, fill = NA); colnames(f1.10) = c("Patient_ID", paste0("f1.10_", colnames(f1.10[,-1,with = F])))
    f1.11 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "dosageATCSkew", fun.aggregate = mean, fill = NA); colnames(f1.11) = c("Patient_ID", paste0("f1.11_", colnames(f1.11[,-1,with = F])))
    f1.12 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "dosageATCHurt", fun.aggregate = mean, fill = NA); colnames(f1.12) = c("Patient_ID", paste0("f1.12_", colnames(f1.12[,-1,with = F])))
    
    
    
    # 2/3. Total unique Drugs/Illness/ATC by week/kurt/skew -------------------
    cat("\nFeature Sets 2...")
    txns[, totDrugPres := length(unique(Drug_ID)), by = Presc_ID]
    txns[, totIllPres := length(unique(ChronicIllness)), by = Presc_ID]
    txns[, totATCPres := length(unique(ATCLevel3Code)), by = Presc_ID]
    
    txns[, totDrugQtr := length(unique(Drug_ID)), by = .(Patient_ID, Qtr)]
    txns[, totIllQtr := length(unique(ChronicIllness)), by = .(Patient_ID, Qtr)]
    txns[, totChronQtr := length(unique(ChronicIllness)), by = .(Patient_ID, ChronicIllness, Qtr)]
    txns[, totChronQtrPerc := totChronQtr/totIllQtr]
    txns[, skewChronQtrPerc := skewness(totChronQtrPerc, na.rm = T), by = .(Patient_ID, ChronicIllness)]
    txns[, totATCQtr := length(unique(ATCLevel3Code)), by = .(Patient_ID, Qtr)]
    
    txns[, totDrugYr := length(unique(Drug_ID)), by = .(Patient_ID, Year)]
    txns[, totIllYr := length(unique(ChronicIllness)), by = .(Patient_ID, Year)]
    txns[, totATCYr := length(unique(ATCLevel3Code)), by = .(Patient_ID, Year)]
    
    # generating features
    feat.p2 = copy(unique(txns[, .(Patient_ID, Presc_ID, Qtr, Year, ChronicIllness,
                                   totDrugPres, totIllPres, totATCPres,
                                   totDrugQtr, totIllQtr, totATCQtr,
                                   totDrugYr, totIllYr, totATCYr, 
                                   totChronQtr, totChronQtrPerc,
                                   skewChronQtrPerc)]))
    f2.1 = dcast(feat.p2, Patient_ID ~ Qtr, value.var = "totDrugQtr", fun.aggregate = mean, fill = 0)
    f2.1[, skew := skewness(as.numeric(.SD), na.rm = TRUE), .SDcols = as.character(unique(feat.p2$Qtr)), by = Patient_ID]; colnames(f2.1) = c("Patient_ID", paste0("f2.1_", colnames(f2.1[,-1,with = F])))
    
    f2.2 = dcast(feat.p2, Patient_ID ~ Qtr, value.var = "totIllQtr", fun.aggregate = mean, fill = 0)
    f2.2[, skew := skewness(as.numeric(.SD), na.rm = TRUE), .SDcols = as.character(unique(feat.p2$Qtr)), by = Patient_ID]; colnames(f2.2) = c("Patient_ID", paste0("f2.2_", colnames(f2.2[,-1,with = F])))
    
    f2.3 = dcast(feat.p2, Patient_ID ~ Qtr, value.var = "totATCQtr", fun.aggregate = mean, fill = 0)
    f2.3[, skew := skewness(as.numeric(.SD), na.rm = TRUE), .SDcols = as.character(unique(feat.p2$Qtr)), by = Patient_ID]; colnames(f2.3) = c("Patient_ID", paste0("f2.3_", colnames(f2.3[,-1,with = F])))
    
    f2.4 = dcast(feat.p2, Patient_ID ~ ChronicIllness + Qtr, value.var = "totChronQtrPerc", fun.aggregate = mean, fill = 0); colnames(f2.4) = c("Patient_ID", paste0("f2.4_", colnames(f2.4[,-1,with = F])))
    f2.5 = dcast(feat.p2, Patient_ID ~ ChronicIllness, value.var = "skewChronQtrPerc", fun.aggregate = mean, fill = NA); colnames(f2.5) = c("Patient_ID", paste0("f2.5_", colnames(f2.5[,-1,with = F])))
    
    
    # 8. Last Drugs/Illness, Days to today ------------------------------------
    cat("\nFeature Sets 3...")
    txns[, lstIllBuy := as.numeric(lstTrans - max(Dispense_Week))/7, by = .(Patient_ID, ChronicIllness)]
    txns[, lstDrugBuy := as.numeric(lstTrans - max(Dispense_Week))/7, by = .(Patient_ID, Drug_ID)]
    txns[, lstATCBuy := as.numeric(lstTrans - max(Dispense_Week))/7, by = .(Patient_ID, ATCLevel3Code)]
    txns[, illLength := as.numeric(max(Dispense_Week) - min(Dispense_Week))/7, by = .(Patient_ID, ChronicIllness)]
    txns[, drugLength := as.numeric(max(Dispense_Week) - min(Dispense_Week))/7, by = .(Patient_ID, Drug_ID)]
    txns[, ATCLength := as.numeric(max(Dispense_Week) - min(Dispense_Week))/7, by = .(Patient_ID, ATCLevel3Code)]
    
    # generating features
    feat.p3 = copy(unique(txns[, .(Patient_ID, Drug_ID, ChronicIllness, ATCLevel3Code,
                                   lstIllBuy, lstDrugBuy, lstATCBuy,
                                   illLength, drugLength, ATCLength)]))
    f3.1 = dcast(feat.p3, Patient_ID ~ ChronicIllness, value.var = "lstIllBuy", fun.aggregate = mean, fill = NA); colnames(f3.1) = c("Patient_ID", paste0("f3.1_", colnames(f3.1[,-1,with = F])))
    f3.2 = dcast(feat.p3, Patient_ID ~ ATCLevel3Code, value.var = "lstATCBuy", fun.aggregate = mean, fill = NA); colnames(f3.2) = c("Patient_ID", paste0("f3.2_", colnames(f3.2[,-1,with = F])))
    f3.3 = dcast(feat.p3, Patient_ID ~ ChronicIllness, value.var = "illLength", fun.aggregate = mean, fill = NA); colnames(f3.3) = c("Patient_ID", paste0("f3.3_", colnames(f3.3[,-1,with = F])))
    f3.4 = dcast(feat.p3, Patient_ID ~ ATCLevel3Code, value.var = "ATCLength", fun.aggregate = mean, fill = NA); colnames(f3.4) = c("Patient_ID", paste0("f3.4_", colnames(f3.4[,-1,with = F])))
    
    
    
    cat("\nFinal Merging...")
    mymerge = function(x,y) merge(x,y, all = T)
    fnl.dat = Reduce(mymerge,list(f1.1, f1.3, f1.4, f1.6, f1.7, f1.8, f1.9, f1.10, f1.11, f1.12, 
                        f2.1, f2.2, f2.3, f2.4, f2.5,
                        f3.1, f3.2, f3.3, f3.4))
    return(fnl.dat)
}


tmp_outcomes2016 = unique(txns[Dispense_Week >= as.Date("2016-01-01") & ChronicIllness == "Diabetes", Patient_ID])
training = generateHistFeatures(txns[!(Dispense_Week >= as.Date("2016-01-01") & ChronicIllness == "Diabetes")], lstTrans = as.Date("2016-01-01"))
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
    eta = 0.01,
    nthread = 7,
    objective = "binary:logistic", #"reg:linear",
    eval_metric = "auc",
    eval_metric = "rmse",
    booster = "gbtree",
    gamma = 0.01,
    min_child_weight = 20,
    subsample = 0.8,
    colsample_bytree = 0.05
)
xgbFit <- xgb.train(param,dtrain,nrounds = 10000,watchlist,
                    early_stopping_rounds = 100,verbose = 1)

var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
xgb.plot.importance(var.imp[1:20,])
val = predict(xgbFit, dtest)
val.f = ifelse(val>= 0.5, 1, 0); table(val.f)
table(val.f == testBC$response)[2]/sum(table(val.f == testBC$response)) 

caret::confusionMatrix(val.f,testBC$response)
# ModelMetrics::confusionMatrix(testBC$response, val, cutoff = 0.5)
pROC::roc(testBC$response, val) 


# 0.954040  50000 obs 2015
# 0.971223  100000 obs 2015
# 0.970773 50000 obs 2016
# 0.972863 100000 obs 2016  Acc 0.9608472 
# 0.972075 AUC All obs 2016 Acc 0.9621118
# 0.9714 AUC 100000 obs 2016 Acc 0.9637917 

# 0.9722/0.9428(bm) RMSE All obs 2016 Acc 0.9622/0.9461(bm)
# 0.172623 / 0.9733


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