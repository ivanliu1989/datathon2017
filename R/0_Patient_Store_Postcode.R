load("./data/meta.RData")
patient
store
postcode = fread("./datathon2017/data/australian-postcodes.csv")
# postcodeDemo = fread("./datathon2017/data/PostcodeVariables.csv", na.strings = "")


# Postcode Feature Engineering --------------------------------------------
patient[,patPostInit:=as.numeric(substr(postcode, 1,1))]
patient[,patPostArea:=as.numeric(substr(postcode, 2,nchar(postcode)))]
store[, postcode := as.numeric(postcode)]
store[,stPostInit:=as.numeric(substr(postcode, 1,1))]
store[,stPostArea:=as.numeric(substr(postcode, 2,nchar(postcode)))]
patient = merge(patient, postcode[!duplicated(postcode), .(postcode, lat, lon)], by = 'postcode', all.x = TRUE)
setnames(patient, c('patPostcode', 'Patient_ID', 'gender', 'year_of_birth', 'patPostInit', 'patPostArea', 'patLat', 'patLon'))
patient[, gender := ifelse(gender == "F", 1, ifelse(gender == "M", 0, -1))]
patient[, age := ifelse(year_of_birth == 1900, NA, 2016 - year_of_birth)]
patient[, year_of_birth := NULL]
store = merge(store, postcode[!duplicated(postcode), .(postcode, lat, lon)], by = 'postcode', all.x = TRUE)
setnames(store, c('stPostcode', 'Store_ID', 'StateCode', 'IsBannerGroup', 'stPostInit', 'stPostArea', 'stLat', 'stLon'))




patient = merge(patient, unique(postcodeDemo[, -2, with = F]), by.x = 'patPostcode', by.y = 'postcode', all.x = TRUE)
# 
cols <- c('gender', 'age', 'patPostArea')
patient[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]
patient[, (cols) := lapply(.SD, scale), .SDcols=cols]




# Different Target --------------------------------------------------------
buy2015 = unique(txns[Dispense_Week < as.Date("2016-01-01") & ChronicIllness == "Diabetes", Patient_ID])
buy2016 = unique(txns[Dispense_Week >= as.Date("2016-01-01") & ChronicIllness == "Diabetes", Patient_ID])
allPat = unique(txns[, Patient_ID])
noBuy2016 = all2016[!all2016 %in% buy2016]

buy_nbuy = allPat[(allPat %in% buy2015) & (!(allPat %in% buy2016))] # 0.986408 / 0.9862 / 0.9777
buy_buy = allPat[(allPat %in% buy2015) & (allPat %in% buy2016)] # 0.996682 / 0.9967 / 0.978
nbuy_buy = allPat[(!(allPat %in% buy2015)) & (allPat %in% buy2016)]
nbuy_nbuy = allPat[(!(allPat %in% buy2015)) & (!(allPat %in% buy2016))]

val_res = val # 0.9753 / 0.9644
val_bn = val # 0.9289 / 0.788
val_bb = val # 0.9696 / 0.9638
val_nb = val # 0.9419 / 0.8028
val_nn = 1- val # 0.9698 / 0.0503

val_final = (val_res + (1-val_bn) + val_bb + val_nb + (1 - val_nn))/5
caret::confusionMatrix(ifelse(val_res >= 0.5, 1, 0),testBC$response)
pROC::roc(testBC$response, val_res) # 0.8998
