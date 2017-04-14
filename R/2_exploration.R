rm(list = ls()); gc()
load("./data/meta.RData")
txns = fread('./data/transactions.csv')
test = fread("./datathon2017/data/diabetes_submission_example.csv")

ls()

dim(txns)
# [1] 59450785       19

names(txns) = paste0("txn.", names(txns))
names(store) = paste0("str.", names(store))
names(patient) = paste0("pat.", names(patient))
names(atc) = paste0("atc.", names(atc))
names(chron) = paste0("chr.", names(chron))
names(drug) = paste0("dru.", names(drug))


# Merge with Store
gc()
txns = merge(txns, store, by.x = 'txn.Store_ID', by.y = 'str.Store_ID', all.x = TRUE, all.y = FALSE)

# Merge with patient
gc()
txns = merge(txns, patient, by.x = 'txn.Patient_ID', by.y = 'pat.Patient_ID', all.x = TRUE, all.y = FALSE)

# Merge with Drug
gc()
txns = merge(txns, drug, by.x = 'txn.Drug_ID', by.y = 'dru.MasterProductID', all.x = TRUE, all.y = FALSE)
Drug_Code = copy(txns[txns$txn.Drug_Code != txns$dru.MasterProductCode, .(dru.MasterProductCode, txn.Drug_Code)])
unique(Drug_Code[dru.MasterProductCode == 'ZYPR11', txn.Drug_Code])
unique(Drug_Code[txn.Drug_Code == '1990132259|LY', dru.MasterProductCode])
unique(Drug_Code[txn.Drug_Code == 'DAAB6E37-F70C-42DC-9E29-2202B7D99EC0', dru.MasterProductCode])

# Merge with ATC
gc()
txns = merge(txns, atc, by.x = c('dru.ATCLevel1Code', 'dru.ATCLevel2Code', 'dru.ATCLevel3Code'
                                 ,'dru.ATCLevel4Code', 'dru.ATCLevel5Code'), 
             by.y = c('atc.ATCLevel1Code', 'atc.ATCLevel2Code', 'atc.ATCLevel3Code'
                      ,'atc.ATCLevel4Code', 'atc.ATCLevel5Code'), all.x = TRUE, all.y = FALSE)


# Merge with Chronic
gc()
txns = merge(txns, chron, by.x = 'txn.Drug_ID', by.y = 'chr.MasterProductID', all.x = TRUE, all.y = FALSE)

gc()
saveRDS(txns, file = './data/main.rds')


# Check diebete txns
txns.diabetes = copy(txns[chr.ChronicIllness == "Diabetes", .(txn.Patient_ID, txn.Dispense_Week)])
table(txns.diabetes$txn.Dispense_Week)
range(txns.diabetes[as.Date(txn.Dispense_Week) >= as.Date('2015-12-28') , txn.Patient_ID])



# Test set: 279201 - 558352
# Test periods: >= as.Date('2015-12-28') | 2016-01-03
# Known periods: 2010-12-26 to 2015-12-27
# Holdout periods: 2015-01-04 to 2015-12-27
# Train periods: 2010-12-26 to 2015-01-04

# Step 1: Text Mining
# Step 2: Meta data feature engineering
# Step 3: 3rd party data
# Step 4: Merge to master transaction table
# Step 5: Time slice based training