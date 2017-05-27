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
setDF(training)


############
# glm ####
############
training[,response] = as.factor(training[,response])
library(caret)
library(glmnet)
set.seed(5)
cv <- 10
folds <- createFolds(training[,response], k = cv, list = FALSE)
f <- folds == 2 
fit <- glmnet(as.matrix(training[!f, predictors]), training[!f, response],
              family = 'binomial', alpha = 1, standardize = TRUE,
              intercept = TRUE, thresh = 1e-7, maxit = 10^5, type.gaussian = 'naive',
              type.logistic = 'modified.Newton'
)
preds <- predict(fit, training[f,predictors],type="class")
evalerror(as.numeric(preds[,2]),training[f,response])
# 0.7961559

############
# vw ####
###a########
# devtools::install_github("JohnLangford/vowpal_wabbit", subdir = "R/r.vw")
library(r.vw)
train_dt <- dt2vw(data = training[!f,predictors], target = response, fileName = '../modelData/vw/train_dt.vw')
test_dt <- to_vw(training[f,], feat, target, '../modelData/vw/test_dt.vw')
write.table(test_dt[,hat], file='data/vw/test_labels.txt', row.names = F, col.names = F, quote = F)

training_data='../data/vw/train_dt.vw'
test_data='../data/vw/test_dt.vw'
test_labels = "../data/vw/test_labels.txt"
out_probs = "../data/vw/sub.txt"
model = "../data/vw/mdl.vw"

# AUC using perf - Download at: osmot.cs.cornell.edu/kddcup/software.html
# Shows files in the working directory: /data
list.files('../data/vw')
grid = expand.grid(eta=c(0.1, 0.3),
                   extra=c('--holdout_period 50 --normalized --adaptive --invariant', 
                           '--nn 30 --holdout_period 50 --normalized --adaptive --invariant',
                           '-q:: --holdout_period 50 --normalized --adaptive --invariant'))
for(i in 1:nrow(grid)){
    g = grid[i, ]
    out_probs = paste0("../data/vw/preds/submission_vw_20160428_NoReg_", g[['eta']], "_", i,".txt")
    model = paste0("../data/vw/mdl",i,".vw")
    # out_probs = paste0("predictions/submission_vw_20151126_0.25_1.txt")
    auc = vw(training_data, training_data, loss = "logistic",
             model, b = 30, learning_rate = g[['eta']], 
             passes = 10, l1=1e-12, l2=NULL, early_terminate = 2,
             link_function = "--link=logistic", extra = g[['extra']],
             out_probs = out_probs, validation_labels = test_labels, verbose = TRUE, 
             do_evaluation = F, use_perf=FALSE, plot_roc=F)
    #extra='--decay_learning_rate 0.9 --ksvm --kernel linear -q ::'
    cat(paste0('Iteration - ',i,'. AUC Score: ', auc, '. GINI Score:', (auc-0.5)*2, '. \n'))
}