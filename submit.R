0.177705
0.175707
0.177705
0.176024

# Cross Validation --------------------------------------------------------
library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./modelData/feat_all_scale_20170525_fix_all_extra.RData")
fnl.dat[, response := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]

convertNum = function(x){
    for(i in 1:ncol(x)){
        x[, i] = as.numeric(x[, i])
    }
    x
}
# fnl.dat =

load(file="./predictors.RData")

response = 'response'

# xgboost -----------------------------------------------------------------
setDT(fnl.dat)
test = as.data.frame(fnl.dat[Patient_ID > 279201])
rm(fnl.dat); gc()
library(xgboost)
dtesting <- xgb.DMatrix(data.matrix(convertNum(test[, predictors])), label = test[, response])
gc()



pred.all = list()
load("./xgboostModels/80_0_16_8_001_07_075_0.971276_5.RData")
pred.all[[1]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.969822_1.RData")
pred.all[[2]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.175707_1.RData")
pred.all[[3]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.174394_2.RData")
pred.all[[4]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.173736_3.RData")
pred.all[[5]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.177705_4.RData")
pred.all[[6]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.172382_5.RData")
pred.all[[7]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.175196_6.RData")
pred.all[[8]] = predict(xgbFit, dtesting)


load("./xgboostModels/80_0_16_8_001_07_075_0.174561_7.RData")
pred.all[[9]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.173812_8.RData")
pred.all[[10]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.176024_9.RData")
pred.all[[11]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.17445_10.RData")
pred.all[[12]] = predict(xgbFit, dtesting)
load("./xgboostModels/80_0_16_8_001_07_075_0.172525_11.RData")
pred.all[[13]] = predict(xgbFit, dtesting)






submissions = makeSubmit(pred.all[[13]])
table(ifelse(submissions$Diabetes >= 0.5, 1, 0))
write.csv(submissions, file = "./topSubmissions/new_0.971392_1.csv", row.names = F)





f = list.files("./topSubmissions/", full.names = T)#[-c(3,4,5,6)]
pred.all = lapply(f, function(x) fread(x))
pred.merge = pred.all[[1]]
pred.merge = as.data.frame(pred.merge)
for(i in 2:length(pred.all)){
    pred.merge$Diabetes = pred.merge$Diabetes + as.data.frame(pred.all[[i]])$Diabetes    
}

pred.merge$Diabetes = pred.merge$Diabetes / length(pred.all)
write.csv(pred.merge, file = "./topSubmissions/merge_all_over965_1.csv", row.names = F)









rm(list = ls()); gc()
library(data.table)
load("./data/meta.RData")
load(file = "./5_xgb_model.RData")


View(trans[Patient_ID == 331243])
