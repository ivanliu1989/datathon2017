library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_scale_20170528_stacking.RData")
# load(file = "./datathon2017/final_features.RData")
load(file = "./datathon2017/final_features_simp.RData")

makeSubmit <- function(pred){
    library(data.table)
    submissions = fread("./datathon2017/data/diabetes_submission_example.csv")
    submit_score = data.table(Patient_ID = testID, Diabetes = pred)
    options(scipen = 3)
    submissions = merge(submissions, submit_score, by = "Patient_ID", all.x = T)
    submissions[, Diabetes.x := NULL]
    setnames(submissions, c("Patient_ID", "Diabetes"))
    submissions[is.na(Diabetes), Diabetes := 0.0000000001]
    submissions[, Patient_ID := as.character(Patient_ID)]
    submissions
}

# Predictors and Response -------------------------------------------------
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
response = 'response'

# xgboost -----------------------------------------------------------------
setDT(fnl.dat)
training = fnl.dat[Patient_ID <= 279201]
test = as.data.frame(fnl.dat[Patient_ID > 279201])
rm(fnl.dat); gc()
library(xgboost)
dtesting <- xgb.DMatrix(data.matrix(test[, predictors]), label = test[, response])
testID = test$Patient_ID
rm(test); gc()

for(i in 88:108){
    print(i)
    
    set.seed(i)
    ss = 0.5
    idx = sample(1:nrow(training), ss * nrow(training))
    trainBC = as.data.frame(training[-idx])
    validationBC = as.data.frame(training[idx])
    dtrain.full = xgb.DMatrix(data.matrix(as.data.frame(training)[, predictors]), label = as.data.frame(training)[, response])
    dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
    dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
    watchlist <- list(train = dtrain, eval = dval)
    gc()
    
    param <- list(
        max_depth = 6,
        eta = 0.03,
        nthread = 7,
        objective = "binary:logistic",
        # eval_metric = "logloss",
        eval_metric = "auc",
        booster = "gbtree",
        gamma = 0.01,
        min_child_weight = 16,
        subsample = 0.9,
        colsample_bytree = 0.15,
        lambda = 100,
        alpha = 1e-2,
        scale_pos_weight = 4,
        # max_delta_step = 1,
        seed = 19890624
    )
    xgbFit <- xgb.train(param,dtrain,nrounds = 5000,watchlist,print_every_n = 50,
                        early_stopping_rounds = 50,verbose = 1)
    
    xgbFit.full <- xgb.train(param,dtrain.full,nrounds = xgbFit$bestInd,print_every_n = 50,verbose = 1)
    
    pred = predict(xgbFit.full, dtesting)
    pred = makeSubmit(pred)
    write.csv(pred, file = paste0("./submit20170529/logloss/XGB_SUBMIT_AUC_", xgbFit$best_score,"_",i,".csv"),
              row.names = F)
    
    save(predictors, xgbFit, file = paste0("./submit20170529/logloss/XGB_SUBMIT_AUC_", xgbFit$best_score,"_",i,".RData"))
    
    rm(trainBC); rm(validationBC); rm(dtrain); rm(dval); rm(xgbFit); gc()
}


# for(i in 1:5){
#     print(i)
#     
#     set.seed(i)
#     ss = 0.3
#     idx = sample(1:nrow(training), ss * nrow(training))
#     trainBC = as.data.frame(training[-idx])
#     validationBC = as.data.frame(training[idx])
#     dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
#     dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
#     watchlist <- list(train = dtrain, eval = dval)
#     gc()
#     
#     param <- list(
#         max_depth = 8,
#         eta = 0.01,
#         nthread = 7,
#         objective = "binary:logistic",
#         eval_metric = "auc",
#         booster = "gbtree",
#         gamma = 0.01,
#         min_child_weight = 16,
#         subsample = 0.9,
#         colsample_bytree = 0.75,
#         lambda = 100,
#         alpha = 1e-2,
#         scale_pos_weight = 4,
#         # max_delta_step = 1,
#         seed = 1989
#     )
#     xgbFit <- xgb.train(param,dtrain,nrounds = 2000,watchlist,print_every_n = 50,
#                         early_stopping_rounds = 50,verbose = 1)
#     
#     pred = predict(xgbFit, dtesting, ntreelimit=xgbFit$bestInd)
#     pred = makeSubmit(pred)
#     write.csv(pred, file = paste0("./submit20170529/auc/XGB_SUBMIT_AUC_SIM_COMP", xgbFit$best_score,"_",i,".csv"),
#               row.names = F)
#     
#     save(predictors, xgbFit, file = paste0("./submit20170529/auc/XGB_SUBMIT_AUC_SIM_COMP", xgbFit$best_score,"_",i,".RData"))
#     
#     rm(trainBC); rm(validationBC); rm(dtrain); rm(dval); rm(xgbFit); gc()
# }
# 
# 
# for(i in 21:25){
#     print(i)
#     
#     set.seed(i)
#     ss = 0.3
#     idx = sample(1:nrow(training), ss * nrow(training))
#     trainBC = as.data.frame(training[-idx])
#     validationBC = as.data.frame(training[idx])
#     dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
#     dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
#     watchlist <- list(train = dtrain, eval = dval)
#     gc()
#     
#     param <- list(
#         max_depth = 8,
#         eta = 0.01,
#         nthread = 7,
#         objective = "binary:logistic",
#         eval_metric = "auc",
#         eval_metric = "rmse",
#         booster = "gbtree",
#         gamma = 0.01,
#         min_child_weight = 16,
#         subsample = 0.9,
#         colsample_bytree = 0.75,
#         lambda = 100,
#         alpha = 1e-2,
#         # scale_pos_weight = 4,
#         max_delta_step = 1,
#         seed = 1989
#     )
#     xgbFit <- xgb.train(param,dtrain,nrounds = 2000,watchlist,print_every_n = 50,
#                         early_stopping_rounds = 50,verbose = 1)
#     
#     pred = predict(xgbFit, dtesting, ntreelimit=xgbFit$bestInd)
#     pred = makeSubmit(pred)
#     write.csv(pred, file = paste0("./submit20170529/auc/XGB_SUBMIT_RMSE_SIM_COMP", xgbFit$best_score,"_",i,".csv"),
#               row.names = F)
#     
#     save(predictors, xgbFit, file = paste0("./submit20170529/auc/XGB_SUBMIT_RMSE_SIM_COMP", xgbFit$best_score,"_",i,".RData"))
#     
#     rm(trainBC); rm(validationBC); rm(dtrain); rm(dval); rm(xgbFit); gc()
# }
# 
# 
# # data.table::fread("./submit20170529/XGB_SUBMIT_RMSE_SIM0.175397_22.csv")
