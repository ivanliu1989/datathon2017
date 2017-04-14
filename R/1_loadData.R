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
