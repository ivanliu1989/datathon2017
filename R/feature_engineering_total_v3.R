rm(list = ls()); gc()
library(data.table)
load("./data/meta.RData")
load(file = "./modelData/5_xgb_model.RData")

# x = trans[!(Dispense_Week >= as.Date("2016-01-01") & ChronicIllness == "Diabetes")]
x = trans[(Dispense_Week < as.Date("2016-01-01"))]
lstTrans = as.Date(max(x$Dispense_Week))
rm(trans); gc()
repNaN = function(x, rep = NA){
    x[,lapply(.SD,function(x){ifelse(is.nan(x),rep,x)})]    
}


# 0. New features ---------------------------------------------------------
setorder(x, Patient_ID, ChronicIllness, Dispense_Week)
x[, illPurchaseInterval := abs(as.numeric(Dispense_Week - shift(Dispense_Week, type = "lead", fill = lstTrans))/7), 
  by = .(Patient_ID, ChronicIllness)]
x[ChronicIllness == "Depression" & Patient_ID == 521970, .(illPurchaseInterval)]
setorder(x, Patient_ID, ATCLevel3Code, Dispense_Week)
x[, atcPurchaseInterval := abs(as.numeric(Dispense_Week - shift(Dispense_Week, type = "lead", fill = lstTrans))/7), 
  by = .(Patient_ID, ATCLevel3Code)]

gc()
x[, illExpDist := pexp(illPurchaseInterval, 1/mean(abs(illPurchaseInterval), na.rm = T)), by = .(Patient_ID, ChronicIllness)]
x[, illChgRatio := illPurchaseInterval/mean(illPurchaseInterval, na.rm = T), by = .(Patient_ID, ChronicIllness)]
x[, atcExpDist := pexp(atcPurchaseInterval, 1/mean(atcPurchaseInterval, na.rm = T)), by = .(Patient_ID, ATCLevel3Code)]
x[, atcChgRatio := atcPurchaseInterval/mean(illPurchaseInterval, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]

load(file = "./modelData/tmp_outcomes2016.RData")
y = unique(x[Patient_ID %in% tmp_outcomes2016, .(Patient_ID, ChronicIllness, ATCLevel3Code, illPurchaseInterval, atcPurchaseInterval)])
y[, avgPosIllIPI := mean(illPurchaseInterval, na.rm = T), by = .(Patient_ID, ChronicIllness)]
y[, avgPosATCIPI := mean(atcPurchaseInterval, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
y = unique(y[, .(Patient_ID, ChronicIllness, ATCLevel3Code, avgPosIllIPI, avgPosATCIPI)])
gc()
y[, avgPosIllIPI := median(avgPosIllIPI), by = ChronicIllness]
y[, avgPosATCIPI := median(avgPosATCIPI), by = ATCLevel3Code]
y.ill = unique(y[, .(ChronicIllness, avgPosIllIPI)])
y.atc = unique(y[, .(ATCLevel3Code, avgPosATCIPI)])
save(y.ill, y.atc, file = "./index_data.RData")

x[, latestIllPurchase := ifelse(Dispense_Week == max(Dispense_Week), 1, 0), by = .(Patient_ID, ChronicIllness)]
x[, latestATCPurchase := ifelse(Dispense_Week == max(Dispense_Week), 1, 0), by = .(Patient_ID, ATCLevel3Code)]
feat.ill = unique(x[latestIllPurchase == 1, .(Patient_ID, ChronicIllness, illExpDist, illChgRatio, illPurchaseInterval)])
feat.atc = unique(x[latestATCPurchase == 1, .(Patient_ID, ATCLevel3Code, atcExpDist, atcChgRatio, atcPurchaseInterval)])

feat.ill.fnl <- merge(feat.ill, y.ill, by = "ChronicIllness", all.x = TRUE)
feat.atc.fnl <- merge(feat.atc, y.atc, by = "ATCLevel3Code", all.x = TRUE)

feat.ill.fnl[, illIndex := illPurchaseInterval / avgPosIllIPI]
feat.ill.fnl[, illChgRatio := ifelse(is.infinite(illChgRatio),10000, illChgRatio)]
feat.atc.fnl[, atcIndex := atcPurchaseInterval / avgPosATCIPI]
feat.atc.fnl[, atcChgRatio := ifelse(is.infinite(atcChgRatio),10000, atcChgRatio)]
save(feat.ill.fnl, feat.atc.fnl, file = "./index_data.RData")
f0.1 = dcast(feat.ill.fnl, Patient_ID ~ ChronicIllness, value.var = "illIndex", fun.aggregate = mean, fill = 100); 
colnames(f0.1) = c("Patient_ID", paste0("f0.1_", colnames(f0.1[,-1,with = F])))
f0.2 = dcast(feat.ill.fnl, Patient_ID ~ ChronicIllness, value.var = "illExpDist", fun.aggregate = mean, fill = NA); 
colnames(f0.2) = c("Patient_ID", paste0("f0.2_", colnames(f0.2[,-1,with = F])))
f0.3 = dcast(feat.ill.fnl, Patient_ID ~ ChronicIllness, value.var = "illChgRatio", fun.aggregate = mean, fill = NA); 
colnames(f0.3) = c("Patient_ID", paste0("f0.3_", colnames(f0.3[,-1,with = F])))

f0.4 = dcast(feat.atc.fnl, Patient_ID ~ ATCLevel3Code, value.var = "atcIndex", fun.aggregate = mean, fill = 100); 
colnames(f0.4) = c("Patient_ID", paste0("f0.4_", colnames(f0.4[,-1,with = F])))
f0.5 = dcast(feat.atc.fnl, Patient_ID ~ ATCLevel3Code, value.var = "atcExpDist", fun.aggregate = mean, fill = NA); 
colnames(f0.5) = c("Patient_ID", paste0("f0.5_", colnames(f0.5[,-1,with = F])))
f0.6 = dcast(feat.atc.fnl, Patient_ID ~ ATCLevel3Code, value.var = "atcChgRatio", fun.aggregate = mean, fill = NA); 
colnames(f0.6) = c("Patient_ID", paste0("f0.6_", colnames(f0.6[,-1,with = F])))

mymerge = function(x,y) merge(x,y, all = T)
feat.p0 = Reduce(mymerge,list(f0.1,f0.2,f0.3,f0.4,f0.5,f0.6))
repNaN = function(x, rep = NA){
    x[,lapply(.SD,function(x){ifelse(is.nan(x),rep,x)})]    
}
feat.p0 = repNaN(feat.p0)
cols <- names(feat.p0)[-1]
feat.p0[, (cols) := lapply(.SD, scale), .SDcols=cols]
save(feat.p0, file = "./feat_p0_scaled.RData")


# 1. Kurt/Skew of drug qty -----------------------------------------------
cat("\nFeature Sets 1...")
library(moments)
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

# Scale & Center
# load(file = "./feat_all_20170525_fix_extra.RData")
cols <- names(fnl.dat)[-1]
idx = 2001:3189; gc() # 3189
fnl.dat[, (cols[idx]) := lapply(.SD, scale), .SDcols=cols[idx]]
save(fnl.dat, file = "./feat_all_scale_20170525_fix_extra.RData")


library(data.table)
load(file = "./modelData/feat_all_scale_20170525_fix_extra.RData.RData")
fnl.dat.all = copy(fnl.dat); rm(fnl.dat); gc()
load(file = "./modelData/feat_all_scale_20170525_fixed.RData")

fnl.dat = merge(fnl.dat, fnl.dat.all, by = "Patient_ID")


save(fnl.dat, file = "./modelData/feat_all_scale_20170525_fix_all.RData")




# Adding new features -----------------------------------------------------
rm(list = ls());gc()
load(file = "./modelData/feat_all_scale_20170525_fix_all.RData")
load(file = "./feat_p0_scaled.RData")
dim(fnl.dat); gc()
fnl.dat = merge(fnl.dat, feat.p0, by = "Patient_ID")
dim(fnl.dat)

save(fnl.dat, file = "./modelData/feat_all_scale_20170525_fix_all_extra.RData")
