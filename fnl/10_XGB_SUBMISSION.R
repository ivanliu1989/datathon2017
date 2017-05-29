library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_scale_20170528_stacking.RData")
load(file = "./datathon2017/featureImportant.RData")

# Predictors and Response -------------------------------------------------
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]


save(predictors, file = "./datathon2017/final_features_simp.RData")
lst.files = list.files("./featImp3/", full.name = T)
impFeatures = lapply(lst.files, function(x){
    dt = head(fread(x, data.table = F), 500)
})
predictors = unique(rbindlist(impFeatures)$Feature)
predictors =unique(predictors[predictors%in%colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]])
predictors =colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]
response = 'response'

# xgboost -----------------------------------------------------------------
setDT(fnl.dat)
training = fnl.dat[Patient_ID <= 279201]
rm(fnl.dat); gc()
library(xgboost)

# xgbFit.all = list()
for(i in 1:5){
    # i = 10
    set.seed(i)
    ss = 0.5#sample(c(0.2, 0.3, 0.4), 1)
    print(paste0("round ", i, " ", ss))
    idx = sample(1:nrow(training), ss * nrow(training))
    trainBC = as.data.frame(training[-idx])
    validationBC = as.data.frame(training[idx])
    
    print("xgb matrix")
    dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
    dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
    watchlist <- list(train = dtrain, eval = dval)
    gc()
    param <- list(
        max_depth = 10,
        eta = 0.1,
        nthread = 7,
        objective = "binary:logistic",
        eval_metric = "auc",
        eval_metric = "rmse",
        booster = "gbtree",
        # booster = "gblinear",
        gamma = 0.2,
        min_child_weight = 16,
        subsample = 0.8,
        colsample_bytree = 0.35,
        lambda = 4,
        alpha = 4, # 1e-5, 1e-2, 0.1, 1, 100
        scale_pos_weight = 1,
        seed = 1989
    )
    xgbFit <- xgb.train(param,dtrain,nrounds = 2000,watchlist,print_every_n = 50,
                        early_stopping_rounds = 50,verbose = 1)
    var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
    write.csv(var.imp, file = paste0("./featureImportance_Fnl_", i, ".csv"), row.names = F)
    
    save(predictors, xgbFit, file = paste0("./xgboostModels/feat_stacking_xgb_80_", xgbFit$best_score,"_",i,".RData"))
    
    # xgbFit.all[[i]] = xgbFit
    rm(trainBC); rm(validationBC); rm(dtrain); rm(dval); rm(xgbFit); rm(var.imp); gc()
}
save(predictors,file="./predictors.RData")

# save(xgbFit, file = "./xgboostModels/80_0_16_8_0001_07_075_0971160.RData")
# save(xgbFit.all, file = "./xgboostModels_0969605_0969836.RData")


# Predicting --------------------------------------------------------------
test = as.data.frame(fnl.dat[Patient_ID > 279201])
rm(fnl.dat); gc()
library(xgboost)
dtesting <- xgb.DMatrix(data.matrix(convertNum(test[, predictors])), label = test[, response])
gc()
pred.all = list()
load("./xgboostModels/80_0_16_6_001_07_075_0971183_imputed.RData")
pred.all[[1]] = predict(xgbFit, dtesting, ntreelimit=xgbFit$bestInd)
load("./xgboostModels/80_0_16_8_0001_07_075_0971160_imputed.RData")
pred.all[[2]] = predict(xgbFit, dtesting, ntreelimit=xgbFit$bestInd)
# pred.all <- list()
# gc()
# for(i in 1:length(xgbFit.all)){
#     pred.all[[i]] = predict(xgbFit.all[[i]], dtesting)
# }
# save(pred.all, file = "./submission/2_folders_0969605_0969836.RData")


# Submissions -------------------------------------------------------------
library(data.table)
makeSubmit <- function(pred){
    library(data.table)
    submissions = fread("./datathon2017/data/diabetes_submission_example.csv")
    submit_score = data.table(Patient_ID = test$Patient_ID, Diabetes = pred)
    options(scipen = 3)
    submissions = merge(submissions, submit_score, by = "Patient_ID", all.x = T)
    submissions[, Diabetes.x := NULL]
    setnames(submissions, c("Patient_ID", "Diabetes"))
    submissions[is.na(Diabetes), Diabetes := 0.0000000001]
    submissions[, Patient_ID := as.character(Patient_ID)]
    submissions
}

submissions = makeSubmit(pred.all[[13]])
write.csv(submissions, file = "./topSubmissions/80_0_16_8_0001_07_075_0971160_imputed.csv", row.names = F)



# Submission Corr ---------------------------------------------------------
load("./submission/2_folders_0969605_0969836.RData")
submissions_no1 = fread("./submission/submission20170524_09687_fixed.csv")
submissions_no2 = fread("./submission/submission20170514_0975_09668.csv")

submissions_all = list()
for(i in 1:length(pred.all)){

    submissions = fread("./datathon2017/data/diabetes_submission_example.csv")
    submit_score = data.table(Patient_ID = test$Patient_ID, Diabetes = pred.all[[i]])
    
    options(scipen = 3)
    submissions = merge(submissions, submit_score, by = "Patient_ID", all.x = T)
    submissions[, Diabetes.x := NULL]
    setnames(submissions, c("Patient_ID", "Diabetes"))
    submissions[is.na(Diabetes), Diabetes := 0.000001]
    submissions[, Patient_ID := as.character(Patient_ID)]
    
    submissions_all[[i]] = submissions
    
    print(table(ifelse(submissions_no2$Diabetes >= 0.5, 1,0) == ifelse(submissions$Diabetes >= 0.5, 1, 0)))    
}

merge.submission = (pred.all[[1]] + pred.all[[2]])/2
submissions = fread("./datathon2017/data/diabetes_submission_example.csv")
submit_score = data.table(Patient_ID = test$Patient_ID, Diabetes = merge.submission)
options(scipen = 3)
submissions = merge(submissions, submit_score, by = "Patient_ID", all.x = T)
submissions[, Diabetes.x := NULL]
setnames(submissions, c("Patient_ID", "Diabetes"))
submissions[is.na(Diabetes), Diabetes := 0.000001]
submissions[, Patient_ID := as.integer(Patient_ID)]
write.csv(submissions, file = "submission20170526_09696_fixed_blending.csv", row.names = F)


table(ifelse(submissions_no1$Diabetes >= 0.5, 1,0) == ifelse(submissions$Diabetes >= 0.5, 1, 0))
table(ifelse(submissions_no1$Diabetes>=0.5,1,0))
table(ifelse(submissions$Diabetes>=0.5,1,0))
