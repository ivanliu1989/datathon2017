library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
# load(file = "./modelData/feat_all_scale_20170528_stacking.RData")
load(file = "./modelData/feat_simplified_20170529.RData")
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

setDT(fnl.dat)
training = fnl.dat[Patient_ID <= 279201]
test = as.data.frame(fnl.dat[Patient_ID > 279201])
rm(fnl.dat); gc()
testID = test$Patient_ID


# h2o ---------------------------------------------------------------------
library(h2o)
reg = FALSE
localH2O <- h2o.init(max_mem_size = '30g', nthreads = 5)
h2o.removeAll()
nfolds = 5
evalMetrics = ifelse(reg, 'MSE', 'AUC')
maxmize = ifelse(evalMetrics == 'AUC', TRUE, FALSE)
glm.family = ifelse(reg, 'gaussian', 'binomial')

# GLM ---------------------------------------------------------------------
# h2o.glm.learner = h2o.glm(training_frame=as.h2o(training),
#                           # validation_frame=as.h2o(testBC),
#                           x=predictors,
#                           y=response,
#                           family=glm.family,
#                           max_iterations=1e5,
#                           # standardize = TRUE,
#                           # intercept = FALSE,
#                           alpha = 0, # 1 lasso | 0 ridge
#                           lambda_search=TRUE,
#                           # fold_assignment = "Modulo",
#                           nfolds = nfolds,
#                           early_stopping=TRUE
# )
# 0.9667094  / 0.9563201 
# h2o.saveModel(h2o.glm.learner, dir = "", name = "", filename = "", force = FALSE)
# h2o.loadModel(path, conn = h2o.getConnection())
# save(h2o.glm.learner, file ="./h2oGLM.RData")
# h2o.glm.pred = predict(h2o.glm.learner, as.h2o(test[, predictors]))
# pred.glm = as.data.frame(h2o.glm.pred[,3])[,1]
# pred = makeSubmit(pred.glm)
# write.csv(pred, file = paste0("./submit20170529/GLM_SUBMIT_", h2o.glm.pred$best_score,".csv"),
#           row.names = F)



# NB ----------------------------------------------------------------------
# training$response = ifelse(training$response == 1, 'Y', 'N')
# training$response = as.factor(training$response)
# h2o.nb.learner = h2o.naiveBayes(training_frame=as.h2o(training),
#                                   x=predictors,
#                                   y=response,keep_cross_validation_predictions = T,
#                                     
#                                   nfolds = nfolds
#         )
# 
# save(h2o.glm.learner, file ="./h2oGLM.RData")
# h2o.glm.pred = predict(h2o.glm.learner, as.h2o(test[, predictors]))
# pred.glm = as.data.frame(h2o.glm.pred[,3])[,1]
# pred = makeSubmit(pred.glm)
# write.csv(pred, file = paste0("./submit20170529/GLM_SUBMIT_", h2o.glm.pred$best_score,".csv"),
#           row.names = F)


# Random Forest -----------------------------------------------------------
# training$response = ifelse(training$response == 1, 'Y', 'N')
# h2o.rf.learner <- h2o.randomForest(training_frame=as.h2o(training),
#                  x=predictors,
#                  y=response,
#                  keep_cross_validation_predictions = T,
#                  nfolds = nfolds)
# 
# h2o.rf.pred = predict(h2o.rf.learner, as.h2o(test[, predictors]))
# pred.rf = as.data.frame(h2o.rf.pred[,1])[,1]
# pred = makeSubmit(pred.rf)
# write.csv(pred, file = paste0("./submit20170529_RF_SUBMIT.csv"),row.names = F)


# Deep learning -----------------------------------------------------------
training$response = as.factor(ifelse(training$response == 1, 'Y', 'N'))
h2o.dl.learner <- h2o.deeplearning(training_frame=as.h2o(training),
                                   x=predictors,
                                   y=response,
                                   keep_cross_validation_predictions = F,
                                   nfolds = nfolds,
                                   distribution = 'bernoulli',
                                   
                                   epochs=1000,
                                   stopping_rounds=2,
                                   rate=0.01,
                                   rate_annealing=2e-6,
                                   momentum_start=0.2,             ## manually tuned momentum
                                   momentum_stable=0.4,
                                   momentum_ramp=1e7,
                                   l1=1e-5,                        ## add some L1/L2 regularization
                                   l2=1e-5,
                                   max_w2=10,                       ## helps stability for Rectifier
                                   stopping_metric="AUC", ## could be "MSE","logloss","r2"
                                   stopping_tolerance=0.001
                                   # variable_importances=T    ## not enabled by default
                                   )
h2o.dl.pred = predict(h2o.dl.learner, as.h2o(test[, predictors]))
pred.dl = as.data.frame(h2o.dl.pred[,3])[,1]
pred = makeSubmit(pred.dl)
write.csv(pred, file = paste0("./submit20170529/NNET_SUBMIT.csv"),row.names = F)




############
# glm ####
############
# training[,response] = as.factor(training[,response])
# library(caret)
# library(glmnet)
# set.seed(5)
# cv <- 10
# folds <- createFolds(training[,response], k = cv, list = FALSE)
# f <- folds == 2
# fit <- glmnet(as.matrix(training[!f, predictors]), training[!f, response],
#               family = 'binomial', alpha = 1, standardize = TRUE,
#               intercept = TRUE, thresh = 1e-7, maxit = 10^5, type.gaussian = 'naive',
#               type.logistic = 'modified.Newton'
# )
# preds <- predict(fit, as.matrix(training[f,predictors]),type="class")
# evalerror(as.numeric(preds[,2]),training[f,response])
# # 0.7961559