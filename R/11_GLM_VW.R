library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_extra_imputed_cleaned_pca_0527.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all_extra_imputed_cleaned.RData")
# load(file = "./modelData/feat_all_scale_20170525_fix_all_extra.RData")
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
############
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
data_types = get_feature_type(training[, setdiff(names(training), c(target, tag)), with=F], threshold = 0)
namespaces = list(n = list(varName = data_types$num_vars, keepSpace=F))

training$response = with(training, ifelse(response == 1, 1, -1))
setDT(training);gc()
dt2vw(data = training[120001:139600], fileName = './modelData/vw/train_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = training[139600:160000], fileName = './modelData/vw/val_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = training[160001:180000], fileName = './modelData/vw/val_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = training[180001:200000], fileName = './modelData/vw/val_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = training[200001:220000], fileName = './modelData/vw/val_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = training[220001:240000], fileName = './modelData/vw/val_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = training[240001:260000], fileName = './modelData/vw/val_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = training[260001:279201], fileName = './modelData/vw/val_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
# 279201
# 279151
dim(testing)
dt2vw(data = testing[1:20000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[20001:40000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[40001:60000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[60001:80000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[80001:100000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[100001:120000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[120001:140000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[140001:160000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[160001:180000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[180001:200000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[200001:220000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[220001:240000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[240001:260000], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)
dt2vw(data = testing[260001:279151], fileName = './modelData/vw/test_dt_full.vw', namespaces = namespaces, target=target, tag = tag, weight=NULL, append = TRUE)

# write.table( training[idx, response], file='./modelData/vw/train_labels.txt', row.names = F, col.names = F, quote = F)
write.table( training[idx, response], file='./modelData/vw/train_labels.txt', row.names = F, col.names = F, quote = F)
write.table( training[-idx, response], file='./modelData/vw/val_labels.txt', row.names = F, col.names = F, quote = F)


