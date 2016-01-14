library(caret)
library(rpart)
library(tree)
library(ggplot2)
library(randomForest)
library(e1071)

# Load whole dataset
seaflowDS <- read.csv("G:/Coursera/Specializations/DataScienceAtScale/2-PracticalPredictiveAnalyticsModelsAndMethods/Assignment-1/seaflow_21min.csv")

# Questions 2,3
summary(seaflowDS)

# Partition dataset, training & test
nr <- nrow(seaflowDS)
training <- seaflowDS[1:floor(nr/2), ]
test <- seaflowDS[(floor(nr/2)+1):nr, ]

# Question 4
mean(training$time)

# Question 5
qplot(pe, chl_small, data = seaflowDS, col = pop) + geom_point(size=1)

# Questions 6, 7, 8
fol <- formula(pop ~ fsc_small + fsc_perp + chl_small + pe + chl_big)
model <- rpart(fol, method="class", data=training)

test_pop <- test$pop
test_pop <- as.vector(test_pop)
predictedPop <- predict(model, test)

# Question 9
get_pop <- function(data){
  result <- vector("character", length=nrow(data))
  for(i in 1:nrow(data)) 
  {
    if(data[i,1] == 1)
    {
      result[i] = "crypto"
    }
    else if(data[i,2] == 1)
    {
      result[i] = "nano"
    }
    else if(data[i,3] == 1)
    {
      result[i] = "pico"
    }
    else if(data[i,4] == 1)
    {
      result[i] = "synecho"
    }
    else if(data[i,5] == 1)
    {
      result[i] = "ultra"
    }
  }
  return (result)
}
popPredicted <- get_pop(round(predictedPop))
x <- (test_pop == popPredicted)
sum(x)/length(x)

# Question 10
randomForestModel <- randomForest(fol, data=training)
rFpredictedPop <- predict(randomForestModel, test)
popRFPredicted <- as.vector(rFpredictedPop)
rfX <- (test_pop == popRFPredicted)
sum(rfX)/length(rfX)

# Question 11
importance(randomForestModel)

# Question 12
e1071Model <- svm(fol, data=training)
e1071predictedPop <- predict(e1071Model, test)
pope1071Predicted <- as.vector(e1071predictedPop)
e1071X <- (test_pop == pope1071Predicted)
sum(e1071X)/length(e1071X)

# Question 13
table(pred = popPredicted, true = test$pop)
table(pred = popRFPredicted, true = test$pop)
table(pred = pope1071Predicted, true = test$pop)



# Question 15
filteredDS <- seaflowDS[seaflowDS$file_id != 208, ]
filteredNR <- nrow(filteredDS)
trainingFilt <- seaflowDS[1:floor(filteredNR/2), ]
testFilt <- seaflowDS[(floor(filteredNR/2)+1):filteredNR, ]

modelFilt <- rpart(fol, method="class", data=trainingFilt)
test_popFilt <- testFilt$pop
test_popFilt <- as.vector(test_popFilt)
predictedPopFilt <- predict(modelFilt, testFilt)
popPredictedFilt <- get_pop(round(predictedPopFilt))
xFilt <- (test_popFilt == popPredictedFilt)
sum(xFilt)/length(xFilt)

randomForestModelFilt <- randomForest(fol, data=trainingFilt)
rFpredictedPopFilt <- predict(randomForestModelFilt, testFilt)
popRFPredictedFilt <- as.vector(rFpredictedPopFilt)
rfXFilt <- (test_popFilt == popRFPredictedFilt)
sum(rfXFilt)/length(rfXFilt)

e1071ModelFilt <- svm(fol, data=trainingFilt)
e1071predictedPopFilt <- predict(e1071ModelFilt, testFilt)
pope1071PredictedFilt <- as.vector(e1071predictedPopFilt)
e1071XFilt <- (test_popFilt == pope1071PredictedFilt)
sum(e1071XFilt)/length(e1071XFilt)
