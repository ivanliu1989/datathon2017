# The task is to predict the probability that a patient will be dispensed a drug related to Diabetes post 2015.

myFiles <- list.files('./data/Transactions', full.names = T)
library(data.table)

dat.tot = lapply(myFiles, FUN = function(x){
    fread(x)
})

dat = rbindlist(dat.tot)
save(dat, file="./data/transactions.RData")
write.csv(dat, file="./data/transactions.csv", row.names = F, quote = F)

atc = fread("./data/Lookups/ATC_LookUp.txt")
chron = fread("./data/Lookups/ChronicIllness_LookUp.txt")
drug = fread("./data/Lookups/Drug_LookUp.txt")
patient = fread("./data/Lookups/patients.txt")
store = fread("./data/Lookups/stores.txt")
save(atc,chron,drug,patient,store, file="./data/meta.RData")

dat2 = fread('./data/transactions.csv')
identical(dat, dat2)



# Missing transactions ----------------------------------------------------
myFiles <- list.files('./data/MISSING_TRANSACTIONS/', full.names = T)
library(data.table)

dat.tot = lapply(myFiles, FUN = function(x){
    fread(x)
})
dat = rbindlist(dat.tot)
write.csv(dat, file="./data/missing_transactions.csv", row.names = F, quote = F)

dat2 = fread('./data/transactions.csv')

table(unique(dat$Patient_ID) %in% unique(dat2$Patient_ID))


dat2 = rbind(dat2, dat)
write.csv(dat2, file="./data/transactions_all.csv", row.names = F, quote = F)

unique(dat2)
