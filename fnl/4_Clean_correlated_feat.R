library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/feat_all_scale_20170525_fix_all_extra.RData")
gc()

setDF(fnl.dat)
toRM = c()
for(i in 1:ncol(fnl.dat)){
    if(length(unique(fnl.dat[,i]))==1){
        print(colnames(fnl.dat)[i])
        toRM = c(toRM, colnames(fnl.dat)[i])
    }
}


for(f in toRM){
    print(unique(fnl.dat[, f]))
}
fnl.dat = fnl.dat[, !(colnames(fnl.dat) %in% toRM)]
# fnl.dat[, Patient_ID := NULL]
gc()
save(fnl.dat, file = "./modelData/feat_all_scale_20170525_fix_all_extra.RData")



setDF(fnl.dat)
for(i in 1:ncol(fnl.dat)){
    if(sum(is.nan(fnl.dat[,i]))>0){
        print(colnames(fnl.dat)[i])
        fnl.dat[is.nan(fnl.dat[,i]),i] = 0
    }
}
for(i in 1:ncol(fnl.dat)){
    if(sum(is.na(fnl.dat[,i]))>0){
        print(colnames(fnl.dat)[i])
        fnl.dat[is.na(fnl.dat[,i]),i] = 0
    }
}
for(i in 1:ncol(fnl.dat)){
    if(sum(is.infinite(fnl.dat[,i]))>0){
        print(colnames(fnl.dat)[i])
        fnl.dat[is.infinite(fnl.dat[,i]),i] = 100
    }
}
setDT(fnl.dat)
save(fnl.dat, file = "./modelData/feat_all_scale_20170525_fix_all_extra_imputed.RData")









library(data.table)
rm(list = ls()); gc()
load(file = "./modelData/feat_all_scale_20170525_fix_all_extra_imputed_cleaned.RData")
gc()

setDF(fnl.dat)
library(caret)
# highlyCorDescr <- findCorrelation(fnl.dat, cutoff = .98, verbose = TRUE)
fnl.dat = fnl.dat[, c(3001:4194)]
gc()
preProcPCA3001_4194 <- preProcess(fnl.dat, method = c("pca"))
gc()
datPCA3001_4194 <- predict(preProcPCA3001_4194, fnl.dat)
dim(datPCA3001_4194)
save(datPCA3001_4194, file = "./datPCA3001_4194.RData")



load("./datPCA2_1000.RData")
load("./datPCA1001_2000.RData")
load("./datPCA2001_3000.RData")
load("./datPCA3001_4194.RData")

Patient_ID = fnl.dat$Patient_ID
rm(fnl.dat); gc()
fnl.dat = cbind(Patient_ID = Patient_ID, datPCA2_1000, datPCA1001_2000,datPCA2001_3000,datPCA3001_4194)
head(fnl.dat)
colnames(fnl.dat) = c("Patient_ID", paste0("PC", 1:3282))
save(fnl.dat, file = "./modelData/feat_all_extra_imputed_cleaned_pca_0527.RData")
