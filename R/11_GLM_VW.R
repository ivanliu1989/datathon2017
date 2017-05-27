library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
# load(file = "./modelData/feat_all_extra_imputed_cleaned_pca_0527.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all_extra_imputed_cleaned.RData")
load(file = "./modelData/feat_all_scale_20170525_fix_all_extra.RData")

setDT(fnl.dat)
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
predictors =colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]
response = 'response'

training = fnl.dat[Patient_ID <= 279201]
testing = fnl.dat[Patient_ID > 279201]
rm(fnl.dat); gc()
library(xgboost)
setDT(training)


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

############
# vw ####
###a########
library(r.vw)
setDT(training)
idx = 1:139600

# Function used to select variables for each namespace
get_feature_type <- function(X, threshold = 50, verbose = FALSE) {
    q_levels <- function (x)
    {
        if (data.table::is.data.table(x)) {
            unlist(x[, lapply(.SD, function(x) length(unique(x)))])
        }
        else {
            apply(x, 2, function(x) length(unique(x)))
        }
    }
    
    lvs = q_levels(X)
    fact_vars = names(lvs[lvs < threshold])
    num_vars = names(lvs[lvs >= threshold])
    if (verbose) {
        print(data.frame(lvs))
    }
    list(fact_vars = fact_vars, num_vars = num_vars)
}

# setwd where the data would be
target = 'response'
tag = 'Patient_ID'
data_types = get_feature_type(training[, setdiff(names(training), c(target, tag)), with=F], threshold = 50)
namespaces = list(n = list(varName = data_types$num_vars, keepSpace=F))


training$response = with(training, ifelse(response == 1, 1, -1))
setDT(training)
dt2vw(data = training[idx], fileName = './modelData/vw/train_dt.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL)
dt2vw(data = training[-idx], fileName = './modelData/vw/val_dt.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL)
dt2vw(data = testing, fileName = './modelData/vw/val_dt.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL)

write.table( training[idx, response], file='./modelData/vw/train_labels.txt', row.names = F, col.names = F, quote = F)
write.table( training[-idx, response], file='./modelData/vw/valid_labels.txt', row.names = F, col.names = F, quote = F)

training_data='./modelData/vw/train_dt.vw'
validation_data='./modelData/vw/validation_dt.vw'
validation_labels = "./modelData/vw/valid_labels.txt"
out_probs = "./modelData/vw/out.txt"
model = "./modelData/vw/mdl.vw"

list.files()

auc = vw(training_data, validation_data, loss = "logistic",
         model, b = 25, learning_rate = 0.5, passes = 1,
         l1 = NULL, l2 = NULL, early_terminate = NULL,
         link_function = "--link=logistic", extra = NULL, out_probs = "out.txt",
         validation_labels = validation_labels, verbose = TRUE, do_evaluation = TRUE,
         use_perf=FALSE, plot_roc=TRUE)

print(auc)
# [1] 0.9944229

# AUC using pROC - Saving plots to disk
### create a parameter grid
grid = expand.grid(l1=c(1e-07, 1e-08),
                   l2=c(1e-07, 1e-08),
                   eta=c(0.1, 0.05),
                   extra=c('--nn 10', ''))


cat('Running grid search\n')
pdf('ROCs.pdf')
aucs <- lapply(1:nrow(grid), function(i){
    g = grid[i, ]
    auc = vw(training_data=training_data, # files relative paths
             validation_data=validation_data,
             validation_labels=validation_labels, model=model,
             # grid options
             loss='logistic', b=25, learning_rate=g[['eta']],
             passes=2, l1=g[['l1']], l2=g[['l2']],
             early_terminate=2, extra=g[['extra']],
             # ROC-AUC related options
             use_perf=FALSE, plot_roc=TRUE,
             do_evaluation = TRUE # If false doesn't compute AUC, use only for prediction
    )
    auc
})
dev.off()

results = cbind(iter=1:nrow(grid), grid, auc=do.call(rbind, aucs))
print(results)
# l1    l2  eta   extra     auc
# 1  1e-07 1e-07 0.10 --nn 10 0.9964736
# 2  1e-08 1e-07 0.10 --nn 10 0.9964945
# 3  1e-07 1e-08 0.10 --nn 10 0.9964736
# 4  1e-08 1e-08 0.10 --nn 10 0.9964946
# 5  1e-07 1e-07 0.05 --nn 10 0.9956487
# 6  1e-08 1e-07 0.05 --nn 10 0.9956629
# 7  1e-07 1e-08 0.05 --nn 10 0.9956487
# 8  1e-08 1e-08 0.05 --nn 10 0.9956629
# 9  1e-07 1e-07 0.10         0.9878654
# 10 1e-08 1e-07 0.10         0.9919489
# 11 1e-07 1e-08 0.10         0.9878646
# 12 1e-08 1e-08 0.10         0.9919487
# 13 1e-07 1e-07 0.05         0.9883343
# 14 1e-08 1e-07 0.05         0.9915172
# 15 1e-07 1e-08 0.05         0.9883339
# 16 1e-08 1e-08 0.05         0.9915170

p = ggplot(results, aes(iter, auc, color=extra)) +
    geom_point(size=3) +
    theme_bw() +
    labs(list(x='Iteration', y='AUC',
              title='Logistic regression results'))

print(p)
ggsave('results_plot.png', plot=p)




















# Previous ----------------------------------------------------------------

train_dt <- dt2vw(data = training[idx], target = response, tag = 'Patient_ID', fileName = '../modelData/vw/train_dt.vw')
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


