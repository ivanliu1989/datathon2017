repNaN = function(x, rep = NA){
    x[,lapply(.SD,function(x){ifelse(is.nan(x),rep,x)})]    
}


# cores = 6
generateHistFeatures = function(txns, lstTrans = as.Date("2016-01-01"), cores = 6){
    
    setorder(txns, Patient_ID, Dispense_Week)
    # library('Matrix')
    # library(doParallel)
    # library(doMC)
    # registerDoMC(cores = cores)
    # pats = unique(txns$Patient_ID)
    # nparallel = round(length(pats)/5000)
    # pats = data.table(pats = pats, cuts = cut(pats, breaks = nparallel, labels = c(1:nparallel)))
    
    # res = foreach(i=as.numeric(unique(pats$cuts)), .combine = rbind) %dopar% {
    x = txns#[Patient_ID %in% pats[cuts == i, pats]]
    
    repNaN = function(x, rep = 0){
        x[,lapply(.SD,function(x){ifelse(is.nan(x),rep,x)})]    
    }
    # 1. Kurt/Skew of drug qty ------------------------------------------------
    cat("\nFeature Sets 1...")
    library(moments)
    x[, illSkew := moments::skewness(Dispense_Week, na.rm = T), by = .(Patient_ID, ChronicIllness)]
    x[, ATCSkew :=  moments::skewness(Dispense_Week, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
    x[, illHurt :=  moments::kurtosis(Dispense_Week, na.rm = T), by = .(Patient_ID, ChronicIllness)]
    x[, ATCHurt :=  moments::kurtosis(Dispense_Week, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
    
    x[, dosage := as.numeric((as.numeric(Dispense_Week - shift(Dispense_Week))/7)/(shift(RepeatsLeft_Qty)-RepeatsLeft_Qty)), by = Presc_Itm_ID]
    x[, dosageIll := sum(dosage, na.rm = T), by = .(Patient_ID, Dispense_Week, ChronicIllness)]
    x[, dosageATC := sum(dosage, na.rm = T), by = .(Patient_ID, Dispense_Week, ATCLevel3Code)]
    x[, dosageQtrIll := sum(dosage, na.rm = T), by = .(Patient_ID, Qtr, ChronicIllness)]
    x[, dosageQtrATC := sum(dosage, na.rm = T), by = .(Patient_ID, Qtr, ATCLevel3Code)]
    
    x[, dosageIllSkew :=  moments::skewness(dosageIll, na.rm = T), by = .(Patient_ID, ChronicIllness)]
    x[, dosageIllHurt :=  moments::kurtosis(dosageIll, na.rm = T), by = .(Patient_ID, ChronicIllness)]
    x[, dosageATCSkew :=  moments::skewness(dosageIll, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
    x[, dosageATCHurt :=  moments::kurtosis(dosageIll, na.rm = T), by = .(Patient_ID, ATCLevel3Code)]
    
    x[, dosageQtrATC5 := sum(dosage, na.rm = T), by = .(Patient_ID, Year, ATCLevel5Code)]
    ### Ingredient 
    
    # generating features
    feat.p1 = copy(unique(x[, .(Patient_ID, Qtr, Dispense_Week, Drug_ID, ChronicIllness, ATCLevel3Code,
                                illSkew, ATCSkew, illHurt, ATCHurt, dosageQtrIll, dosageQtrATC, 
                                dosageIllSkew, dosageIllHurt, dosageATCSkew, dosageATCHurt
                                ,dosageQtrATC5)]))
    feat.p1 = repNaN(feat.p1)
    feat.p1[is.na(ChronicIllness), ChronicIllness:="Others"]
    f1.1 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "illSkew", fun.aggregate = mean, fill = 0); colnames(f1.1) = c("Patient_ID", paste0("f1.1_", colnames(f1.1[,-1,with = F])))
    f1.3 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "ATCSkew", fun.aggregate = mean, fill = 0); colnames(f1.3) = c("Patient_ID", paste0("f1.3_", colnames(f1.3[,-1,with = F])))
    f1.4 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "illHurt", fun.aggregate = mean, fill = 0); colnames(f1.4) = c("Patient_ID", paste0("f1.4_", colnames(f1.4[,-1,with = F])))
    f1.6 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "ATCHurt", fun.aggregate = mean, fill = 0); colnames(f1.6) = c("Patient_ID", paste0("f1.6_", colnames(f1.6[,-1,with = F])))
    f1.7 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "dosageQtrIll", fun.aggregate = mean, fill = 0); colnames(f1.7) = c("Patient_ID", paste0("f1.7_", colnames(f1.7[,-1,with = F])))
    f1.8 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "dosageQtrATC", fun.aggregate = mean, fill = 0); colnames(f1.8) = c("Patient_ID", paste0("f1.8_", colnames(f1.8[,-1,with = F])))
    f1.9 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "dosageIllSkew", fun.aggregate = mean, fill = 0); colnames(f1.9) = c("Patient_ID", paste0("f1.9_", colnames(f1.9[,-1,with = F])))
    f1.10 = dcast(feat.p1, Patient_ID ~ ChronicIllness, value.var = "dosageIllHurt", fun.aggregate = mean, fill = 0); colnames(f1.10) = c("Patient_ID", paste0("f1.10_", colnames(f1.10[,-1,with = F])))
    f1.11 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "dosageATCSkew", fun.aggregate = mean, fill = 0); colnames(f1.11) = c("Patient_ID", paste0("f1.11_", colnames(f1.11[,-1,with = F])))
    f1.12 = dcast(feat.p1, Patient_ID ~ ATCLevel3Code, value.var = "dosageATCHurt", fun.aggregate = mean, fill = 0); colnames(f1.12) = c("Patient_ID", paste0("f1.12_", colnames(f1.12[,-1,with = F])))
    f1.13 = dcast(feat.p1, Patient_ID ~ ATCLevel5Code, value.var = "dosageQtrATC5", fun.aggregate = mean, fill = 0); colnames(f1.13) = c("Patient_ID", paste0("f1.13_", colnames(f1.13[,-1,with = F])))
    rm(feat.p1); gc()
    
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
    feat.p2 = copy(unique(x[, .(Patient_ID, Presc_ID, Qtr, Year, ChronicIllness,
                                totDrugPres, totIllPres, totATCPres,
                                totATC5Pres,
                                totDrugQtr, totIllQtr, totATCQtr,
                                totDrugYr, totIllYr, totATCYr, 
                                totChronQtr, totChronQtrPerc,
                                skewChronQtrPerc)]))
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
    
    # 8. Last Drugs/Illness, Days to today ------------------------------------
    cat("\nFeature Sets 8...")
    x[, lstIllBuy := as.numeric(lstTrans - max(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ChronicIllness)]
    x[, lstDrugBuy := as.numeric(lstTrans - max(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, Drug_ID)]
    x[, lstATCBuy := as.numeric(lstTrans - max(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ATCLevel3Code)]
    x[, lstATC5Buy := as.numeric(lstTrans - max(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ATCLevel5Code)]
    x[, illLength := as.numeric(max(Dispense_Week, na.rm = T) - min(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ChronicIllness)]
    x[, drugLength := as.numeric(max(Dispense_Week, na.rm = T) - min(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, Drug_ID)]
    x[, ATCLength := as.numeric(max(Dispense_Week, na.rm = T) - min(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ATCLevel3Code)]
    x[, ATC5Length := as.numeric(max(Dispense_Week, na.rm = T) - min(Dispense_Week, na.rm = T))/7, by = .(Patient_ID, ATCLevel5Code)]
    
    # generating features
    feat.p3 = copy(unique(x[, .(Patient_ID, Drug_ID, ChronicIllness, ATCLevel3Code,
                                lstIllBuy, lstDrugBuy, lstATCBuy,
                                lstATC5Buy, ATC5Length,
                                illLength, drugLength, ATCLength)]))
    feat.p3 = repNaN(feat.p3)
    f3.1 = dcast(feat.p3, Patient_ID ~ ChronicIllness, value.var = "lstIllBuy", fun.aggregate = mean, fill = NA); colnames(f3.1) = c("Patient_ID", paste0("f3.1_", colnames(f3.1[,-1,with = F])))
    f3.2 = dcast(feat.p3, Patient_ID ~ ATCLevel3Code, value.var = "lstATCBuy", fun.aggregate = mean, fill = NA); colnames(f3.2) = c("Patient_ID", paste0("f3.2_", colnames(f3.2[,-1,with = F])))
    f3.3 = dcast(feat.p3, Patient_ID ~ ChronicIllness, value.var = "illLength", fun.aggregate = mean, fill = 0); colnames(f3.3) = c("Patient_ID", paste0("f3.3_", colnames(f3.3[,-1,with = F])))
    f3.4 = dcast(feat.p3, Patient_ID ~ ATCLevel3Code, value.var = "ATCLength", fun.aggregate = mean, fill = 0); colnames(f3.4) = c("Patient_ID", paste0("f3.4_", colnames(f3.4[,-1,with = F])))
    f3.5 = dcast(feat.p3, Patient_ID ~ ATCLevel5Code, value.var = "lstATC5Buy", fun.aggregate = mean, fill = NA); colnames(f3.5) = c("Patient_ID", paste0("f3.5_", colnames(f3.5[,-1,with = F])))
    f3.6 = dcast(feat.p3, Patient_ID ~ ATCLevel5Code, value.var = "ATC5Length", fun.aggregate = mean, fill = 0); colnames(f3.6) = c("Patient_ID", paste0("f3.6_", colnames(f3.6[,-1,with = F])))
    rm(feat.p3);gc()
    
    # 13 Prescriber # / Prescriber distance / Postcode ------------------------
    # cat("\nFeature Sets 13...")
    # feat.p13 = unique(x[, .(Patient_ID, Prescriber_ID, Qtr, ChronicIllness)])
    # f13.1 = data.table::dcast(feat.p13,Patient_ID~Qtr+ChronicIllness,fun=length, fill = 0); setnames(f13.1, c('Patient_ID', paste0("f13.1_", colnames(f13.1[,-1,with = F]))))
    # f13.2 = data.table::dcast(x,Patient_ID~Qtr+ChronicIllness+IsDeferredScript,fun=length, fill = 0); setnames(f13.2, c('Patient_ID', paste0("f13.2_", colnames(f13.2[,-1,with = F]))))
    # rm(feat.p13);gc()
    
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
    f14.2 = dcast(patient, Patient_ID ~ ageCat, fun = length); setnames(f14.2, c('Patient_ID', paste0('PatAge_', colnames(f14.2)[-1])))
    f14.3 = dcast(patient, Patient_ID ~ gender, fun = length); setnames(f14.3, c('Patient_ID', paste0('PatGender_', colnames(f14.3)[-1])))
    f14.4 = patient[,.(Patient_ID, patPostArea)]
    feat.p15 = merge(x, store, by = 'Store_ID', all.x = TRUE)
    f15.1 = data.table::dcast(feat.p15,Patient_ID~IsBannerGroup,fun=length, fill = 0); setnames(f15.1, c('Patient_ID', paste0("f15.1_", colnames(f15.1[,-1,with = F]))))
    f15.2 = data.table::dcast(feat.p15,Patient_ID~StateCode,fun=length, fill = 0); setnames(f15.2, c('Patient_ID', paste0("f15.2_", colnames(f15.2[,-1,with = F]))))
    f15.3 = data.table::dcast(feat.p15,Patient_ID~SourceSystem_Code,fun=length, fill = 0); setnames(f15.3, c('Patient_ID', paste0("f15.3_", colnames(f15.3[,-1,with = F]))))
    rm(feat.p15);gc()
    
    # 9	Prices, reclain of different drugs, illness ---------------------------
    # cat("\nFeature Sets 9...")
    # feat.p9 = x[, .(Patient_ID, ChronicIllness, ATCLevel3Code, Qtr, GovernmentReclaim_Amt, PatientPrice_Amt)]
    # feat.p9[, reclaim_perc := GovernmentReclaim_Amt / (GovernmentReclaim_Amt+PatientPrice_Amt)]
    # feat.p9[is.na(reclaim_perc), reclaim_perc := 0]
    # f9.1 = dcast(feat.p9, Patient_ID ~ ChronicIllness + Qtr, fun = median); setnames(f9.1, c('Patient_ID', paste0('f9.1_', colnames(f9.1)[-1])))
    # rm(feat.p9);gc()
    
    # Final Clean and Merge ---------------------------------------------------
    cat("\nFinal Merging...")
    mymerge = function(x,y) merge(x,y, all = T)
    fnl.dat = Reduce(mymerge,list(f1.1, f1.3, f1.4, f1.6, f1.7, f1.8, f1.9, f1.10, f1.11, f1.12, 
                                  f1.13,
                                  f2.1, f2.2, f2.3, f2.4, f2.5,
                                  f3.1, f3.2, f3.3, f3.4,f3.5,f3.6,
                                  # f9.1,
                                  # f13.1, f13.2,
                                  f14.1, f14.2, f14.3, f14.4,
                                  f15.1, f15.2, f15.3))
    fnl.dat = repNaN(fnl.dat)
    cols <- names(fnl.dat)[-1]
    fnl.dat[, (cols) := lapply(.SD, scale), .SDcols=cols]
    toRm = c()
    for(i in 1:ncol(fnl.dat)){
        r = nrow(unique(fnl.dat[, i, with = F]))
        if(r<=1) toRm = c(toRm, i)
    }
    fnl.dat[, (toRm) := NULL, with = F]
    
    
        
}
