library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_extra_imputed_cleaned_pca_0527.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all_extra_imputed_cleaned.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all_extra.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all_extra_imputed.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all_impute.RData")
setDT(fnl.dat)
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
predictors =colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]
response = 'response'

training = fnl.dat[Patient_ID <= 279201]
rm(fnl.dat); gc()
library(xgboost)
setDT(training)
set.seed(5)
idx = sample(1:nrow(training), 0.5 * nrow(training))
trainBC = as.data.frame(training[-idx])
validationBC = as.data.frame(training[idx])

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
