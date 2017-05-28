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

setDT(fnl.dat)
training = fnl.dat[Patient_ID <= 279201]
test = as.data.frame(fnl.dat[Patient_ID > 279201])
rm(fnl.dat); gc()


# setDT(training)
# set.seed(5)
# idx = sample(1:nrow(training), 0.5 * nrow(training))
# trainBC = as.data.frame(training[-idx])
# validationBC = as.data.frame(training[idx])

# h2o ---------------------------------------------------------------------
library(h2o)
reg = FALSE
localH2O <- h2o.init(max_mem_size = '40g', nthreads = 7)
h2o.removeAll()
nfolds = 5
evalMetrics = ifelse(reg, 'MSE', 'AUC')
maxmize = ifelse(evalMetrics == 'AUC', TRUE, FALSE)
glm.family = ifelse(reg, 'gaussian', 'binomial')

# GLM ---------------------------------------------------------------------
h2o.glm.learner = h2o.glm(training_frame=as.h2o(trainBC),
                          # validation_frame=as.h2o(testBC),
                          x=predictors,
                          y=response,
                          family=glm.family,
                          max_iterations=1e5,
                          # standardize = TRUE,
                          # intercept = FALSE,
                          alpha = 0, # 1 lasso | 0 ridge
                          lambda_search=TRUE,
                          # fold_assignment = "Modulo",
                          nfolds = nfolds,
                          early_stopping=TRUE
)
# 0.9667094  / 0.9563201 
h2o.glm.pred = predict(h2o.glm.learner, as.h2o(testBC))
val.glm = as.data.frame(h2o.glm.pred[,3])[,1]










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