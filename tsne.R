# library(data.table)
# library(Rtsne)
# library(tsne)
# rm(list = ls()); gc()
# load(file = "./modelData/feat_all_extra_imputed_cleaned_pca_0527.RData")
# 
# predictors =colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]
# setDF(fnl.dat)
# fnl.dat = as.matrix(fnl.dat[, predictors])
# 
# set.seed(8) # Set a seed if you want reproducible results
# rm()
# tsne_out <- Rtsne(fnl.dat, check_duplicates = F) # Run TSNE
# 
# 
# tsne_out = tsne(fnl.dat)
# 
# gc()
# 
# 



library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_scale_20170528_stacking.RData")

colnames(fnl.dat) = c("Patient_ID", paste0("FEAT_", 1:(ncol(fnl.dat)-2)), "response")
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]

predictors =colnames(fnl.dat)[!colnames(fnl.dat) %in% c('Patient_ID','response')]
response = 'response'
setDT(fnl.dat)
training = fnl.dat[Patient_ID <= 279201]
rm(fnl.dat); gc()

library(mlr)
library(FSelector)
library(randomForestSRC)
setDF(training)
gc()
training$response = as.factor(training$response)
train.task <- makeClassifTask(data=training[, c(predictors[1:20], response)], target=response)
gc()
imp_var1 <- generateFilterValuesData(train.task, method=c("information.gain"))

# 96
for(i in 97:218){
    print(i)
    fn = 1:20 + 20*i
    train.task <- makeClassifTask(data=training[, c(predictors[fn], response)], target=response)
    gc()
    imp_var1 <- generateFilterValuesData(train.task, method=c("information.gain"))$data
    if(i == 0){
        imp_var = imp_var1
    }else{
        imp_var = rbind(imp_var, imp_var1)
    }
}

featLookup = data.frame(oldFeat = colnames(fnl.dat), 
                        newFeat = c("Patient_ID", paste0("FEAT_", 1:(ncol(fnl.dat)-1))))

varImp = merge(featLookup, imp_var, by.x = "newFeat", by.y = "name", all.x = T)
save(imp_var, varImp, file = "./datathon2017/featureImportant.RData")

var1 <- imp_var1$data[imp_var1$data$information.gain > 0.03, c('name')]


plot_infogain <- plotFilterValues(imp_var1, feat.type.cols=TRUE)
plot_infogain




View(trans[Patient_ID %in% c(349, 440, 2429, 4586, 5037)])





# patient_id         prob target ivan
# 1:        349 8.747661e-01      0    1
# 2:        440 6.436778e-01      0    1
# 3:       2429 9.350704e-01      0    1
# 4:       4586 6.754684e-01      0    1
# 5:       5037 5.916706e-05      0    1
# 6:       5962 6.168390e-01      0    1
# 7:       6302 5.514075e-01      0    1
# 8:       7795 7.988164e-01      0    1
# 9:       8238 6.642369e-01      0    1
# 10:       8620 5.258309e-06      0    1
# 11:       9170 3.976965e-01      0    1
# 12:      10590 8.197543e-01      0    1










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