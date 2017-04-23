rm(list = ls()); gc()

trans = fread('./data/transactions_all.csv')
load("./data/meta.RData")

# 1. get a list of patients who were prescribed diabetes medicatio --------

tmp_outcomes2015 = unique(trans[as.Date(Prescription_Week) >= as.Date("2015-01-01") & 
                                    Drug_ID %in% unique(chron[ChronicIllness == "Diabetes", MasterProductID]), Patient_ID])

tmp_outcomes2014 = unique(trans[as.Date(Prescription_Week) < as.Date("2015-01-01") & 
                                    Drug_ID %in% unique(chron[ChronicIllness == "Diabetes", MasterProductID]), Patient_ID])


# 2. flag all patients as 1 or 0 ------------------------------------------
patient[, tgt2015 := ifelse(Patient_ID %in% tmp_outcomes2015, 1, 0)]
patient[, tgt2014 := ifelse(Patient_ID %in% tmp_outcomes2014, 0.99, 0.01)]


# 3. Evaluation
binaryClassifierEvaluation(patient$tgt2014, patient$tgt2015)

### 0.9329 / 0.9439
# 0.9155
# 0.9504






# 1. get a list of patients who were prescribed diabetes medicatio --------

tmp_outcomes2016 = unique(trans[as.Date(Prescription_Week) >= as.Date("2016-01-01") &
                                    Drug_ID %in% unique(chron[ChronicIllness == "Diabetes", MasterProductID]), Patient_ID])

tmp_outcomes2015 = unique(trans[as.Date(Prescription_Week) < as.Date("2016-01-01") & 
                                    Drug_ID %in% unique(chron[ChronicIllness == "Diabetes", MasterProductID]), Patient_ID])


# 2. flag all patients as 1 or 0 ------------------------------------------
patient[, tgt2016 := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]
patient[, tgt2015 := ifelse(Patient_ID %in% tmp_outcomes2015, 0.99, 0.01)]


# 3. Evaluation
binaryClassifierEvaluation(patient[Patient_ID <279201, tgt2015], patient[Patient_ID <279201, tgt2016])

### 0.94503
### 0.937 / 0.9346
# 0.9407
# 0.9333