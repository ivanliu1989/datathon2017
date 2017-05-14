# rf, h2o, kmean, tsne, glm, nb, gbm

# RF ----------------------------------------------------------------------
h2o.rf.learner <- h2o.randomForest(
    training_frame=as.h2o(trainBC.dl),
    validation_frame=as.h2o(validationBC.dl),
    x=predictors,
    y=response,
    # mtries = 12,
    col_sample_rate_change_per_level = 0.8,
    sample_rate = 0.632,
    col_sample_rate_per_tree = 0.3,
    ntrees = 1200,
    max_depth = 12,
    # min_rows = 10,
    binomial_double_trees = TRUE,
    balance_classes = TRUE,
    stopping_metric = evalMetrics,
    stopping_rounds = 2,
    stopping_tolerance = 1e-2,
    score_each_iteration = T,
    # fold_assignment = "Modulo",
    # nfolds = nfolds,
    seed=999)
# 0.8931312  / 0.8257826 
h2o.rf.pred = predict(h2o.rf.learner, as.h2o(testBC.dl))
val.rf = as.data.frame(h2o.rf.pred[,3])[,1]


# NB ----------------------------------------------------------------------
h2o.nb.learner <- h2o.naiveBayes(
    training_frame=as.h2o(trainBC.dl),
    # validation_frame=as.h2o(testBC),
    x=predictors,
    y=response,
    ignore_const_cols=TRUE,
    compute_metrics=TRUE,
    fold_assignment = "Modulo",
    nfolds = nfolds,
    seed=999
)
# 0.9667094  / 0.9563201 
h2o.nb.pred = predict(h2o.nb.learner, as.h2o(testBC.dl))
val.nb = as.data.frame(h2o.nb.pred[,3])[,1]