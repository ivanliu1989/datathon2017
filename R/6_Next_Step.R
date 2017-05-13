# Different Objective XXXXX
# Cross validation
# PCA XXXXX
# Feature stacking - rf, h2o, kmean, tsne, glm, nb, gbm
# Blending Deep Learning

set.seed(3456)
library(xgboost)
convertNum = function(x){
    for(i in 1:ncol(x)){
        x[, i] = as.numeric(x[, i])
    }
    x
}
idx = sample(1:nrow(training), 0.2 * nrow(training))
idx2 = sample(1:nrow(trainBC), 0.3 * nrow(trainBC))


# Different Target --------------------------------------------------------
buy2015 = unique(txns[Dispense_Week < as.Date("2016-01-01") & ChronicIllness == "Diabetes", Patient_ID])
buy2016 = unique(txns[Dispense_Week >= as.Date("2016-01-01") & ChronicIllness == "Diabetes", Patient_ID])
allPat = unique(txns[, Patient_ID])
noBuy2016 = allPat[!allPat %in% buy2016]

buy_nbuy = allPat[(allPat %in% buy2015) & (!(allPat %in% buy2016))] # 0.986408 / 0.9862 / 0.9777
buy_buy = allPat[(allPat %in% buy2015) & (allPat %in% buy2016)] # 0.996682 / 0.9967 / 0.978
nbuy_buy = allPat[(!(allPat %in% buy2015)) & (allPat %in% buy2016)]
nbuy_nbuy = allPat[(!(allPat %in% buy2015)) & (!(allPat %in% buy2016))]


# Different Objective -----------------------------------------------------
training[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
training[,response := NULL]; training[, response := ifelse(Patient_ID %in% buy_nbuy, 1, 0)]
training[,response := NULL]; training[, response := ifelse(Patient_ID %in% buy_buy, 1, 0)]
training[,response := NULL]; training[, response := ifelse(Patient_ID %in% nbuy_buy, 1, 0)]
training[,response := NULL]; training[, response := ifelse(Patient_ID %in% nbuy_nbuy, 1, 0)]


# xgboost -----------------------------------------------------------------
trainBC = convertNum(as.data.frame(training[-idx]))
validationBC = convertNum(as.data.frame(training[idx]))
testBC = trainBC[idx2,]
trainBC = trainBC[-idx2,]
table(validationBC$response); table(trainBC$response); table(testBC$response)

predictors =colnames(trainBC)[!colnames(trainBC) %in% c('Patient_ID','response','buy_nbuy','buy_buy','nbuy_buy','nbuy_nbuy')]
response = 'response'
dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
watchlist <- list(train = dtrain, eval = dval)
gc()
param <- list(
    max_depth = 6,
    eta = 0.01,
    nthread = 7,
    objective = "binary:logistic",
    eval_metric = "auc",
    eval_metric = "rmse",
    booster = "gbtree",
    gamma = 0.01,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.15
)
xgbFit <- xgb.train(param,dtrain,nrounds = 10000,watchlist,print_every_n = 50,
                    early_stopping_rounds = 20,verbose = 1)

var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
val = predict(xgbFit, dtest)
val.f = ifelse(val>= 0.5, 1, 0); table(val.f)
caret::confusionMatrix(val.f,testBC$response)
pROC::roc(testBC$response, val) 



val.all = val # 0.9713 / 0.9621          
val_bn = val # 0.9872 / 0.9764
val_bb = val # 0.9965 / 0.9761          
val_nb = val # 0.7529 / 0.9856          
val_nn = val # 0.9808 / 0.9856          

# 0.9712 / 0.9619

val.fnl = data.table(Patient_ID = testBC$Patient_ID, val.all = val.all, val_bn = val_bn, val_bb = val_bb, val_nb = val_nb, val_nn = val_nn)
val.fnl[, Year:= ifelse(Patient_ID %in% buy2015, 1, 0)]
val.fnl[Year == 1, fnl.score := (val.all+(1-val_bn)+val_bb)/3]
val.fnl[Year == 0, fnl.score := (val.all+(1-val_nn))/2]
val.fnl[, fnl.score := (val.all+val_bb + val_nb)/3]
val.fnl[, fnl.score.f := ifelse(fnl.score>= 0.5, 1, 0)]
val.fnl[, actual := testBC$response]


training[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
trainBC = convertNum(as.data.frame(training[-idx]))
validationBC = convertNum(as.data.frame(training[idx]))
testBC = trainBC[idx2,]
trainBC = trainBC[-idx2,]
table(validationBC$response); table(trainBC$response); table(testBC$response)

caret::confusionMatrix(val.fnl$fnl.score.f,testBC$response)
pROC::roc(testBC$response, val.fnl$fnl.score) 

# Actual 1 pred 0




# PCA ---------------------------------------------------------------------
library(caret)
nzv <- nearZeroVar(as.data.frame(training)[,!colnames(training) %in% c('Patient_ID','response')], saveMetrics= TRUE)
toDel = rownames(nzv[nzv$zeroVar,])
training[, (toDel):=NULL]
predictors =colnames(training)[!colnames(training) %in% c('Patient_ID','response')]
response = 'response'

pp_hpc <- preProcess(as.data.frame(training)[, predictors], method = c("pca"))

trainBC = convertNum(as.data.frame(training[-idx]))
validationBC = convertNum(as.data.frame(training[idx]))
testBC = trainBC[idx2,]
trainBC = trainBC[-idx2,]

transformed.train <- predict(pp_hpc, newdata = trainBC[, predictors])
transformed.test <- predict(pp_hpc, newdata = testBC[, predictors])
transformed.val <- predict(pp_hpc, newdata = validationBC[, predictors])
head(transformed.train)

trainBC = data.table(Patient_ID = trainBC$Patient_ID, transformed.train, response = trainBC$response)
testBC = data.table(Patient_ID = testBC$Patient_ID, transformed.test, response = testBC$response)
validationBC = data.table(Patient_ID = validationBC$Patient_ID, transformed.val, response = validationBC$response)

colnames(validationBC) <- c('Patient_ID', paste0('PC',1:638), 'response')

# Cross Validation --------------------------------------------------------
trainIndex <- createFolds(trainBC$response, k = 5, list = FALSE)
head(trainIndex)

for(i in 1:5){
    
}


# Blending ----------------------------------------------------------------
val = predict(xgbFit, dtest)
val.blend = (as.data.frame(h2o.glm.pred$p1)[,1] + val)/2
val.blend.f = ifelse(val.blend>= 0.5, 1, 0); table(val.blend.f)
table(val.blend.f == testBC$response)[2]/sum(table(val.blend.f == testBC$response)) 

caret::confusionMatrix(val.blend.f,testBC$response)
pROC::roc(testBC$response, val.blend) 

# 0.9668(xgb) + 0.9676(glm) = 0.9685 | PCA

# Different Objective -----------------------------------------------------
training[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]

# xgboost -----------------------------------------------------------------
trainBC = convertNum(as.data.frame(training[-idx]))
validationBC = convertNum(as.data.frame(training[idx]))
testBC = trainBC[idx2,]
trainBC = trainBC[-idx2,]
table(validationBC$response); table(trainBC$response); table(testBC$response)

predictors =colnames(trainBC)[!colnames(trainBC) %in% c('Patient_ID','response','buy_nbuy','buy_buy','nbuy_buy','nbuy_nbuy')]
response = 'response'
dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dval <- xgb.DMatrix(data.matrix(validationBC[, predictors]), label = validationBC[, response])
dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
watchlist <- list(train = dtrain, eval = dval)
gc()
param <- list(
    max_depth = 6,
    eta = 0.01,
    nthread = 7,
    objective = "binary:logistic",
    eval_metric = "auc",
    eval_metric = "rmse",
    booster = "gbtree",
    gamma = 0.01,
    min_child_weight = 10,
    subsample = 0.8,
    colsample_bytree = 0.15
)
xgbFit <- xgb.train(param,dtrain,nrounds = 10000,watchlist,print_every_n = 50,
                    early_stopping_rounds = 20,verbose = 1)

var.imp = xgb.importance(colnames(dtrain), model = xgbFit)
val = predict(xgbFit, dtest)
val.f = ifelse(val>= 0.5, 1, 0); table(val.f)
caret::confusionMatrix(val.f,testBC$response)
pROC::roc(testBC$response, val) 

val.xgb = val


# h2o ---------------------------------------------------------------------
library(h2o)
reg = FALSE
localH2O <- h2o.init(max_mem_size = '40g', nthreads = -1)
h2o.removeAll()
nfolds = 5
evalMetrics = ifelse(reg, 'MSE', 'AUC')
maxmize = ifelse(evalMetrics == 'AUC', TRUE, FALSE)
glm.family = ifelse(reg, 'gaussian', 'binomial')
# Deep learning -----------------------------------------------------------
hyper_params <- list(
    activation=c("Rectifier","Maxout","RectifierWithDropout", "MaxoutWithDropout"),# "Tanh","TanhWithDropout",
    hidden=list(c(20,20),c(50,50),c(30,30,30),c(25,25,25,25)),
    input_dropout_ratio=c(0,0.05),
    l1=seq(0,1e-6,1e-8),
    l2=seq(0,1e-6,1e-8),
    epochs=c(1,5) #,10,30
)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=999,
                       stopping_rounds=5, stopping_tolerance=1e-2)
dl_random_grid <- h2o.grid(
    algorithm="deeplearning",
    grid_id = "dl_grid_random",
    training_frame=as.h2o(trainBC),
    # validation_frame=as.h2o(testBC),
    x=predictors,
    y=response,
    stopping_metric=evalMetrics,
    stopping_tolerance=1e-2,
    stopping_rounds=5,
    score_duty_cycle=0.025,
    max_w2=10,
    nfolds = nfolds,
    fold_assignment = "Modulo",
    keep_cross_validation_predictions = TRUE,
    hyper_params = hyper_params,
    search_criteria = search_criteria
)
grid <- h2o.getGrid("dl_grid_random",sort_by=evalMetrics,decreasing=maxmize)
grid@summary_table[1,]
h2o.dl.learner.1 <- h2o.getModel(grid@model_ids[[1]])
h2o.dl.learner.2 <- h2o.getModel(grid@model_ids[[2]])
h2o.dl.learner.3 <- h2o.getModel(grid@model_ids[[3]])
h2o.dl.learner.4 <- h2o.getModel(grid@model_ids[[4]])
h2o.dl.learner.5 <- h2o.getModel(grid@model_ids[[5]])

# GBM ---------------------------------------------------------------------
h2o.gbm.learner <- h2o.gbm(
    training_frame=as.h2o(trainBC),
    # validation_frame=as.h2o(testBC),
    x=predictors,
    y=response,
    ntrees = 1000,
    learn_rate = 0.01,
    learn_rate_annealing = 0.99,
    min_rows = 10,
    max_depth = 3,
    stopping_rounds = 2,
    stopping_tolerance = 0.01,
    fold_assignment = "Modulo",
    nfolds = nfolds,
    seed = 999)


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
w.glm = h2o.glm.learner@model$validation_metrics@metrics$AUC
# 0.9667094  / 0.9563201 
h2o.glm.pred = predict(h2o.glm.learner, as.h2o(testBC))


# RF ----------------------------------------------------------------------
h2o.rf.learner <- h2o.randomForest(
    training_frame=as.h2o(trainBC),
    # validation_frame=as.h2o(testBC),
    x=predictors,
    y=response,
    # mtries = 12,
    # col_sample_rate_change_per_level = 0.8,
    # sample_rate = 0.632,
    # col_sample_rate_per_tree = 0.8,
    ntrees = 200,
    max_depth = 6,
    # min_rows = 10,
    binomial_double_trees = TRUE,
    balance_classes = TRUE,
    stopping_metric = evalMetrics,
    stopping_rounds = 2,
    stopping_tolerance = 1e-2,
    score_each_iteration = T,
    fold_assignment = "Modulo",
    nfolds = nfolds,
    seed=999)

# NB ----------------------------------------------------------------------
trainBC.nb = copy(trainBC)
trainBC.nb$response = ifelse(trainBC.nb$response == 0, 'N', 'Y')
h2o.nb.learner <- h2o.naiveBayes(
    training_frame=as.h2o(trainBC.nb),
    # validation_frame=as.h2o(testBC),
    x=predictors,
    y=response,
    ignore_const_cols=TRUE,
    compute_metrics=TRUE,
    fold_assignment = "Modulo",
    nfolds = nfolds,
    seed=999
)
w.NB = h2o.nb.learner@model$validation_metrics@metrics$AUC
# 0.9667094  / 0.9563201 
h2o.nb.pred = predict(h2o.nb.learner, as.h2o(testBC))

# Stacking ----------------------------------------------------------------
h2o.dl.pred.1 = tail(as.data.frame(h2o.predict(h2o.dl.learner.1, as.h2o(trainBC)))[,3],1)
h2o.dl.pred.2 = tail(as.data.frame(h2o.predict(h2o.dl.learner.2, as.h2o(trainBC)))[,3],1)
h2o.dl.pred.3 = tail(as.data.frame(h2o.predict(h2o.dl.learner.3, as.h2o(trainBC)))[,3],1)
h2o.dl.pred.4 = tail(as.data.frame(h2o.predict(h2o.dl.learner.4, as.h2o(trainBC)))[,3],1)
h2o.dl.pred.5 = tail(as.data.frame(h2o.predict(h2o.dl.learner.5, as.h2o(trainBC)))[,3],1)

