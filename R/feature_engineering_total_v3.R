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
x[, dosage := as.numeric((as.numeric(Dispense_Week - shift(Dispense_Week))/7)/(shift(RepeatsLeft_Qty)-RepeatsLeft_Qty)), by = Presc_Itm_ID]
x[, dosageQtrIll := sum(dosage, na.rm = T), by = .(Patient_ID, Qtr, ChronicIllness)]
x[, dosageQtrATC := sum(dosage, na.rm = T), by = .(Patient_ID, Qtr, ATCLevel3Code)]
x[, dosageQtrATC5 := sum(dosage, na.rm = T), by = .(Patient_ID, Year, ATCLevel5Code)]
### Ingredient 
gc()
# save(x, file = "./featureset1.RData")
# load(file = "./featureset1.RData")
# generating features
feat.p1 = unique(x[, .(Patient_ID, Qtr, Dispense_Week, Drug_ID, ChronicIllness, ATCLevel3Code, ATCLevel5Code,
                       dosageQtrIll, dosageQtrATC, dosageQtrATC5)])
feat.p1 = repNaN(feat.p1)
feat.p1[is.na(ChronicIllness), ChronicIllness:="Others"]
f1.7 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "dosageQtrIll", fun.aggregate = mean, fill = 0); colnames(f1.7) = c("Patient_ID", paste0("f1.7_", colnames(f1.7[,-1,with = F])))
f1.8 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "dosageQtrATC", fun.aggregate = mean, fill = 0); colnames(f1.8) = c("Patient_ID", paste0("f1.8_", colnames(f1.8[,-1,with = F])))
f1.13 = dcast(feat.p1, Patient_ID ~ ATCLevel5Code, value.var = "dosageQtrATC5", fun.aggregate = mean, fill = 0); colnames(f1.13) = c("Patient_ID", paste0("f1.13_", colnames(f1.13[,-1,with = F])))
# rm(f1.7); gc()

mymerge = function(x,y) merge(x,y, all = T)
feat.p1 = Reduce(mymerge,list(f1.7, f1.8, f1.13))
save(feat.p1, file = "./feat_p1_v2.RData")





# 2/3. Total unique Drugs/Illness/ATC by week/kurt/skew -------------------
cat("\nFeature Sets 2...")
x[, totDrugPres := length(unique(Drug_ID)), by = Presc_ID]
x[, totIllPres := length(unique(ChronicIllness)), by = Presc_ID]
x[, totATCPres := length(unique(ATCLevel3Code)), by = Presc_ID]

x[, totDrugQtr := length(unique(Drug_ID)), by = .(Patient_ID, Qtr)]
x[, totIllQtr := length(unique(ChronicIllness)), by = .(Patient_ID, Qtr)]
x[, totChronQtr := length(unique(ChronicIllness)), by = .(Patient_ID, ChronicIllness, Qtr)]
x[, totChronQtrPerc := totChronQtr/totIllQtr]
x[, skewChronQtrPerc := moments::skewness(totChronQtrPerc, na.rm = T), by = .(Patient_ID, ChronicIllness)]
x[, totATCQtr := length(unique(ATCLevel3Code)), by = .(Patient_ID, Qtr)]

x[, totDrugYr := length(unique(Drug_ID)), by = .(Patient_ID, Year)]
x[, totIllYr := length(unique(ChronicIllness)), by = .(Patient_ID, Year)]
x[, totATCYr := length(unique(ATCLevel3Code)), by = .(Patient_ID, Year)]

# generating features
feat.p2 = unique(x[, .(Patient_ID, Presc_ID, Qtr, Year, ChronicIllness,
                       totDrugPres, totIllPres, totATCPres,
                       totDrugQtr, totIllQtr, totATCQtr,
                       totDrugYr, totIllYr, totATCYr, 
                       totChronQtr, totChronQtrPerc,
                       skewChronQtrPerc)])
feat.p2 = repNaN(feat.p2)
f2.1 = dcast(feat.p2, Patient_ID ~ Qtr, value.var = "totDrugQtr", fun.aggregate = mean, fill = 0)
f2.1[, skew :=  moments::skewness(as.numeric(.SD), na.rm = TRUE), .SDcols = as.character(unique(feat.p2$Qtr)), by = Patient_ID]; colnames(f2.1) = c("Patient_ID", paste0("f2.1_", colnames(f2.1[,-1,with = F])))
f2.2 = dcast(feat.p2, Patient_ID ~ Qtr, value.var = "totIllQtr", fun.aggregate = mean, fill = 0)
f2.2[, skew :=  moments::skewness(as.numeric(.SD), na.rm = TRUE), .SDcols = as.character(unique(feat.p2$Qtr)), by = Patient_ID]; colnames(f2.2) = c("Patient_ID", paste0("f2.2_", colnames(f2.2[,-1,with = F])))
f2.3 = dcast(feat.p2, Patient_ID ~ Qtr, value.var = "totATCQtr", fun.aggregate = mean, fill = 0)
f2.3[, skew :=  moments::skewness(as.numeric(.SD), na.rm = TRUE), .SDcols = as.character(unique(feat.p2$Qtr)), by = Patient_ID]; colnames(f2.3) = c("Patient_ID", paste0("f2.3_", colnames(f2.3[,-1,with = F])))
f2.4 = dcast(feat.p2, Patient_ID ~ ChronicIllness + Qtr, value.var = "totChronQtrPerc", fun.aggregate = mean, fill = 0); colnames(f2.4) = c("Patient_ID", paste0("f2.4_", colnames(f2.4[,-1,with = F])))
f2.5 = dcast(feat.p2, Patient_ID ~ ChronicIllness, value.var = "skewChronQtrPerc", fun.aggregate = mean, fill = 0); colnames(f2.5) = c("Patient_ID", paste0("f2.5_", colnames(f2.5[,-1,with = F])))
rm(feat.p2); gc()

mymerge = function(x,y) merge(x,y, all = T)
feat.p2 = Reduce(mymerge,list(f2.1, f2.2, f2.3, f2.4, f2.5))
save(feat.p2, file = "./feat_p2_v2.RData")



# 8. Last Drugs/Illness, Days to today ------------------------------------
cat("\nFeature Sets 8...")
x[, lstATC5Buy := as.numeric(lstTrans - max(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ATCLevel5Code)]
x[, ATC5Length := as.numeric(max(Dispense_Week, na.rm = T) - min(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ATCLevel5Code)]
# generating features
feat.p3 = unique(x[, .(Patient_ID, Drug_ID, ChronicIllness, ATCLevel3Code,ATCLevel5Code, lstATC5Buy, ATC5Length)])
feat.p3 = repNaN(feat.p3)
f3.5 = dcast(feat.p3, Patient_ID ~ ATCLevel5Code, value.var = "lstATC5Buy", fun.aggregate = mean, fill = NA); colnames(f3.5) = c("Patient_ID", paste0("f3.5_", colnames(f3.5[,-1,with = F])))
f3.6 = dcast(feat.p3, Patient_ID ~ ATCLevel5Code, value.var = "ATC5Length", fun.aggregate = mean, fill = 0); colnames(f3.6) = c("Patient_ID", paste0("f3.6_", colnames(f3.6[,-1,with = F])))
rm(feat.p3);gc()

mymerge = function(x,y) merge(x,y, all = T)
feat.p3 = Reduce(mymerge,list(f3.5,f3.6))
save(feat.p3, file = "./feat_p3_v2.RData")



# 14 Gender, age, dosage index (vs index) / ABS ---------------------------
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
f14.1 = dcast(patient, Patient_ID ~ patPostInit, fun = length); setnames(f14.1, c('Patient_ID', paste0('PatState_', colnames(f14.1)[-1])))
f14.4 = patient[,.(Patient_ID, patPostArea)]
rm(feat.p15);gc()

mymerge = function(x,y) merge(x,y, all = T)
feat.p4 = Reduce(mymerge,list(f14.1, f14.4))
save(feat.p4, file = "./feat_p4_v2.RData")



# Merge Final Features ----------------------------------------------------
load(file = './feat_p1_v2.RData')
load(file = './feat_p2_v2.RData')
mymerge = function(x,y) merge(x,y, all = T)
fnl.dat = Reduce(mymerge,list(feat.p1,feat.p2))
rm(feat.p1);rm(feat.p2);gc()
load(file = './feat_p3_v2.RData')
fnl.dat = Reduce(mymerge,list(fnl.dat,feat.p3))
rm(feat.p3);gc()
load(file = './feat_p4_v2.RData')
fnl.dat = Reduce(mymerge,list(fnl.dat,feat.p4))
rm(feat.p4);gc()
save(fnl.dat, file = "./feat_all_20170525_fix_extra.RData")
gc()
toRm = c()
for(i in 1:ncol(fnl.dat)){
    r = nrow(unique(fnl.dat[, i, with = F]))
    if(r<=1) toRm = c(toRm, i)
}
fnl.dat[, (toRm) := NULL, with = F]
save(fnl.dat, file = "./feat_all_20170525_fix_extra.RData")
repNaN = function(x, rep = NA){
    x[,lapply(.SD,function(x){ifelse(is.nan(x),rep,x)})]    
}
fnl.dat = repNaN(fnl.dat)
save(fnl.dat, file = "./feat_all_20170525_fix_extra.RData")
# NVZ
library(caret)
ncol(fnl.dat)
# toDelAll = c()
load(file = "./nzv.RData")
# nzv <- nearZeroVar(fnl.dat[,4001:4572, with = F], saveMetrics= TRUE)
toDel = rownames(nzv[nzv$zeroVar,])
toDelAll = c(toDelAll, toDel)
save(toDelAll, file = "./nzv.RData")
fnl.dat[, (toDelAll):=NULL]
save(fnl.dat, file = "./feat_all_20170525_fix_extra.RData")
# Scale & Center
# load(file = "./feat_all_20170525_fix_extra.RData")
cols <- names(fnl.dat)[-1]
idx = 4001:4507; gc()
fnl.dat[, (cols[idx]) := lapply(.SD, scale), .SDcols=cols[idx]]
save(fnl.dat, file = "./feat_all_scale_20170525_fix_extra.RData")

