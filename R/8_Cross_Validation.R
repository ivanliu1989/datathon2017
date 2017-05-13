# Cross Validation --------------------------------------------------------
trainIndex <- createFolds(trainBC$response, k = 5, list = FALSE)
head(trainIndex)

for(i in 1:5){
    
}