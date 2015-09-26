rm(list = ls())
# Read cleaned training and testing data 

#set working directory

setwd("/home/nilmadhab/Desktop/data/Practical-Machine-Learning/")


training <- read.table(file = "pml-training2.csv", 
                       header = TRUE, sep = ",", quote = "")
testing <- read.table(file = "pml-testing2.csv", 
                      header = TRUE, sep = ",", quote = "")
# Change the numeric type to integer type to make sure 
# the same data type in training data and testing data
training$magnet_dumbbell_z <- as.integer(training$magnet_dumbbell_z)
training$magnet_forearm_y <- as.integer(training$magnet_forearm_y)
training$magnet_forearm_z <- as.integer(training$magnet_forearm_z)
# Change the 
levels(testing$new_window) <- levels(training$new_window)

# Install randomForest package
# install.packages("randomForest")
library(randomForest)
# install.packages("caret")
library(caret)


set.seed(111)
# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 2)
# Perform the cross validation
cv <- train(classe ~ ., data = training, method = "rf", 
            trControl = fitControl)
cv$bestTune$mtry

library(rattle)
fancyRpartPlot(cv$finalModel)


RandomForest = randomForest(classe ~ ., data = training, 
                            mtry = cv$bestTune$mtry)
PredictForTrain = predict(RandomForest)
table(PredictForTrain, training$classe)

PredictForest = predict(RandomForest, newdata = testing)
PredictForest

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_", i ,".txt")
    write.table(x[i], file = filename, quote = FALSE,
                row.names = FALSE, col.names = FALSE)
  }
}
# Call the function
pml_write_files(PredictForest)

