rm(list = ls()); gc()
library(data.table)
load("./data/meta.RData")
load(file = "./5_xgb_model.RData")

# x = trans[!(Dispense_Week >= as.Date("2016-01-01") & ChronicIllness == "Diabetes")]
x = trans[(Dispense_Week < as.Date("2016-01-01"))]
lstTrans = as.Date(max(x$Dispense_Week))
rm(trans); gc()
repNaN = function(x, rep = NA){
    x[,lapply(.SD,function(x){ifelse(is.nan(x),rep,x)})]    
}


# 1. Kurt/Skew of drug qty -----------------------------------------------
cat("\nFeature Sets 1...")
library(moments)
x[, illSkew := moments::skewness(Dispense_Week, na.rm = T), by = .(Patient_ID, ChronicIllness)]
x[, ATCSkew :=  moments::skewness(Dispense_Week, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
x[, PurchaseSkew :=  moments::skewness(Dispense_Week, na.rm = T), by = .(Patient_ID)]
x[, illHurt :=  moments::kurtosis(Dispense_Week, na.rm = T), by = .(Patient_ID, ChronicIllness)]
x[, ATCHurt :=  moments::kurtosis(Dispense_Week, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
x[, PurchaseHurt :=  moments::kurtosis(Dispense_Week, na.rm = T), by = .(Patient_ID)]

x[, dosage := as.numeric((as.numeric(Dispense_Week - shift(Dispense_Week))/7)/(shift(RepeatsLeft_Qty)-RepeatsLeft_Qty)), by = Presc_Itm_ID]
x[, dosageIll := sum(dosage, na.rm = T), by = .(Patient_ID, Dispense_Week, ChronicIllness)]
x[, dosageATC := sum(dosage, na.rm = T), by = .(Patient_ID, Dispense_Week, ATCLevel3Code)]

x[, dosageIllSkew :=  moments::skewness(dosageIll, na.rm = T), by = .(Patient_ID, ChronicIllness)]
x[, dosageIllHurt :=  moments::kurtosis(dosageIll, na.rm = T), by = .(Patient_ID, ChronicIllness)]
x[, dosageATCSkew :=  moments::skewness(dosageIll, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
x[, dosageATCHurt :=  moments::kurtosis(dosageIll, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]

### Ingredient 
gc()
# save(x, file = "./featureset1.RData")
# load(file = "./featureset1.RData")
# generating features
feat.p1 = unique(x[, .(Patient_ID, Qtr, Dispense_Week, Drug_ID, ChronicIllness, ATCLevel3Code,
                       illSkew, ATCSkew, illHurt, ATCHurt,
                       dosageIllSkew, dosageIllHurt, dosageATCSkew, dosageATCHurt
)])
feat.p1 = repNaN(feat.p1)+
feat.p1[is.na(ChronicIllness), ChronicIllness:="Others"]
f1.1 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "illSkew", fun.aggregate = mean, fill = 0); colnames(f1.1) = c("Patient_ID", paste0("f1.1_", colnames(f1.1[,-1,with = F])))
f1.3 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "ATCSkew", fun.aggregate = mean, fill = 0); colnames(f1.3) = c("Patient_ID", paste0("f1.3_", colnames(f1.3[,-1,with = F])))
f1.4 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "illHurt", fun.aggregate = mean, fill = 0); colnames(f1.4) = c("Patient_ID", paste0("f1.4_", colnames(f1.4[,-1,with = F])))
f1.6 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "ATCHurt", fun.aggregate = mean, fill = 0); colnames(f1.6) = c("Patient_ID", paste0("f1.6_", colnames(f1.6[,-1,with = F])))
f1.9 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "dosageIllSkew", fun.aggregate = mean, fill = 0); colnames(f1.9) = c("Patient_ID", paste0("f1.9_", colnames(f1.9[,-1,with = F])))
f1.10 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "dosageIllHurt", fun.aggregate = mean, fill = 0); colnames(f1.10) = c("Patient_ID", paste0("f1.10_", colnames(f1.10[,-1,with = F])))
f1.11 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "dosageATCSkew", fun.aggregate = mean, fill = 0); colnames(f1.11) = c("Patient_ID", paste0("f1.11_", colnames(f1.11[,-1,with = F])))
f1.12 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "dosageATCHurt", fun.aggregate = mean, fill = 0); colnames(f1.12) = c("Patient_ID", paste0("f1.12_", colnames(f1.12[,-1,with = F])))
rm(feat.p1); gc()

mymerge = function(x,y) merge(x,y, all = T)
feat.p1 = Reduce(mymerge,list(f1.1, f1.3, f1.4, f1.6, f1.9, f1.10, f1.11, f1.12))
save(feat.p1, file = "./feat_p1.RData")


# 3. Last Drugs/Illness, Days to today -----------------------------------
cat("\nFeature Sets 8...")
x[, lstIllBuy := as.numeric(lstTrans - max(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ChronicIllness)]
x[, lstDrugBuy := as.numeric(lstTrans - max(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, Drug_ID)]
x[, lstATCBuy := as.numeric(lstTrans - max(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ATCLevel3Code)]
x[, illLength := as.numeric(max(Dispense_Week, na.rm = T) - min(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ChronicIllness)]
# x[, drugLength := as.numeric(max(Dispense_Week, na.rm = T) - min(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, Drug_ID)]
x[, ATCLength := as.numeric(max(Dispense_Week, na.rm = T) - min(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ATCLevel3Code)]
# generating features
feat.p3 = unique(x[, .(Patient_ID, Drug_ID, ChronicIllness, ATCLevel3Code,
                       lstIllBuy, lstDrugBuy, lstATCBuy,
                       illLength, ATCLength)])
feat.p3 = repNaN(feat.p3)
f3.1 = dcast(feat.p3, Patient_ID ~ ChronicIllness, value.var = "lstIllBuy", fun.aggregate = mean, fill = NA); colnames(f3.1) = c("Patient_ID", paste0("f3.1_", colnames(f3.1[,-1,with = F])))
f3.2 = dcast(feat.p3, Patient_ID ~ ATCLevel3Code, value.var = "lstATCBuy", fun.aggregate = mean, fill = NA); colnames(f3.2) = c("Patient_ID", paste0("f3.2_", colnames(f3.2[,-1,with = F])))
f3.3 = dcast(feat.p3, Patient_ID ~ ChronicIllness, value.var = "illLength", fun.aggregate = mean, fill = 0); colnames(f3.3) = c("Patient_ID", paste0("f3.3_", colnames(f3.3[,-1,with = F])))
f3.4 = dcast(feat.p3, Patient_ID ~ ATCLevel3Code, value.var = "ATCLength", fun.aggregate = mean, fill = 0); colnames(f3.4) = c("Patient_ID", paste0("f3.4_", colnames(f3.4[,-1,with = F])))
rm(feat.p3);gc()

mymerge = function(x,y) merge(x,y, all = T)
feat.p3 = Reduce(mymerge,list(f3.1, f3.2, f3.3, f3.4))
save(feat.p3, file = "./feat_p3.RData")



# 4 Gender, age, dosage index (vs index) / ABS ---------------------------
cat("\nFeature Sets 14...")
mround <- function(x,base){ 
    base*round(x/base) 
} 
load("./data/meta.RData")
postcode = fread("./datathon2017/data/australian-postcodes.csv")
postcode[,postcode := ifelse(nchar(postcode) == 3, paste0('0', as.character(postcode)), postcode)]
patient[,postcode := ifelse(nchar(postcode) == 3, paste0('0', as.character(postcode)), postcode)]
store[,postcode := ifelse(nchar(postcode) == 3, paste0('0', as.character(postcode)), postcode)]

patient[,patPostInit:=as.numeric(substr(postcode, 1,1))]
patient[,patPostArea:=as.numeric(substr(postcode, 2,nchar(postcode)))]
store[,stPostInit:=as.numeric(substr(postcode, 1,1))]
store[,stPostArea:=as.numeric(substr(postcode, 2,nchar(postcode)))]
patient = merge(patient, postcode[!duplicated(postcode), .(postcode, lat, lon)], by = 'postcode', all.x = TRUE)
setnames(patient, c('patPostcode', 'Patient_ID', 'gender', 'year_of_birth', 'patPostInit', 'patPostArea', 'patLat', 'patLon'))
# patient[, gender := ifelse(gender == "F", 1, ifelse(gender == "M", 0, -1))]
patient[, age := ifelse(year_of_birth == 1900, NA, 2016 - year_of_birth)]
patient[, year_of_birth := NULL]
store = merge(store, postcode[!duplicated(postcode), .(postcode, lat, lon)], by = 'postcode', all.x = TRUE)
nSt = data.table(NA, -9999, 'Online', NA, NA, NA, NA, NA); setnames(nSt, colnames(store))
store = rbind(store, nSt)
setnames(store, c('stPostcode', 'Store_ID', 'StateCode', 'IsBannerGroup', 'stPostInit', 'stPostArea', 'stLat', 'stLon'))

patient = patient[Patient_ID %in% x$Patient_ID]
patient[, ageCat := ifelse(is.na(age), -1, mround(age,5))]
# f14.1 = dcast(patient, Patient_ID ~ patPostInit, fun = length); setnames(f14.1, c('Patient_ID', paste0('PatState_', colnames(f14.1)[-1])))
f14.2 = dcast(patient, Patient_ID ~ ageCat, fun = length); setnames(f14.2, c('Patient_ID', paste0('PatAge_', colnames(f14.2)[-1])))
f14.3 = dcast(patient, Patient_ID ~ gender, fun = length); setnames(f14.3, c('Patient_ID', paste0('PatGender_', colnames(f14.3)[-1])))
# f14.4 = patient[,.(Patient_ID, patPostArea)]
feat.p15 = merge(x, store, by = 'Store_ID', all.x = TRUE)
f15.1 = data.table::dcast(feat.p15,Patient_ID~IsBannerGroup,fun=length, fill = 0); setnames(f15.1, c('Patient_ID', paste0("f15.1_", colnames(f15.1[,-1,with = F]))))
f15.2 = data.table::dcast(feat.p15,Patient_ID~StateCode,fun=length, fill = 0); setnames(f15.2, c('Patient_ID', paste0("f15.2_", colnames(f15.2[,-1,with = F]))))
f15.3 = data.table::dcast(feat.p15,Patient_ID~SourceSystem_Code,fun=length, fill = 0); setnames(f15.3, c('Patient_ID', paste0("f15.3_", colnames(f15.3[,-1,with = F]))))
rm(feat.p15);gc()

mymerge = function(x,y) merge(x,y, all = T)
feat.p4 = Reduce(mymerge,list(f14.2, f14.3,
                              f15.1, f15.2, f15.3))
save(feat.p4, file = "./feat_p4.RData")





# 5. Slope / p Distribution ------------------------------------------------------




# Merge -------------------------------------------------------------------
load(file = './feat_p1.RData')
load(file = './feat_p3.RData')
mymerge = function(x,y) merge(x,y, all = T)
fnl.dat = Reduce(mymerge,list(feat.p1,feat.p3))
rm(feat.p1);rm(feat.p3);gc()
load(file = './feat_p4.RData')
fnl.dat = Reduce(mymerge,list(fnl.dat,feat.p4[, c(1:25), with = F]))
rm(feat.p4);gc()
gc()
toRm = c()
for(i in 1:ncol(fnl.dat)){
    r = nrow(unique(fnl.dat[, i, with = F]))
    if(r<=1) toRm = c(toRm, i)
}
fnl.dat[, (toRm) := NULL, with = F]
repNaN = function(x, rep = NA){
    x[,lapply(.SD,function(x){ifelse(is.nan(x),rep,x)})]    
}
fnl.dat = repNaN(fnl.dat)
save(fnl.dat, file = "./feat_all_20170525_fixed.RData")


# Scale NVZ ---------------------------------------------------------------
# NVZ
library(caret)
# Scale & Center
load(file = "./feat_all_20170525_fixed.RData")
cols <- names(fnl.dat)[-1]
fnl.dat[, (cols) := lapply(.SD, scale), .SDcols=cols]
save(fnl.dat, file = "./feat_all_scale_20170525_fixed.RData")

