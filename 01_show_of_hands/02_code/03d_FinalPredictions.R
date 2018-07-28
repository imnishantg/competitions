###### Loading Libraries ########
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggvis)
library(caret)
library(caTools)
library(mice)
library(mi)
library(VIM)
set.seed(100)




####### Final Predications #########
## Ways to be tested:
# 1) Average of the probabilities

## Models to be used
# 1) Logistic Regression
# 2) RPart (## Weak Classifier)
# 3) Random Forest
# 4) Gradient Boosting Machines
# 5) Support Vector Machine
# 6) LDA
# 7) QDA
# 8) Naive Bayes

#### Prediction Probabilities ####

pred.svmHappy4_Tr <- predict(svmHappy4, newdata = trainIV_4, probability = TRUE)
pred.svmHappy4_Te <- predict(svmHappy4, newdata = testIV_4, probability = TRUE)

## Prediction Probabilities for Training Datasets

logHappyTrain <- predict(LogisticHappy,newdata = trainIV_4, type="response")
rpartHappyTrain <- predict(rpartHappy4, newdata = trainIV_4, type="prob")[, 2]
rfHappyTrain <- predict(rfHappy4, newdata = trainIV_4, type = "prob")[, 2]
gbmHappyTrain <- predict(gbmHappy4, newdata = trainIV_4, n.tree = 2000, type = "response")
svmHappyTrain <- attr(pred.svmHappy4_Tr, "probabilities")[, 1]
ldaHappyTrain <- predict(ldaHappy4, newdata = trainIV_4)$posterior[, 2]
qdaHappyTrain <- predict(qdaHappy4, newdata = trainIV_4)$posterior[, 2]
nbHappyTrain <- predict(NBHappy4, newdata = testIV_4, type = "raw")[, 2]

## Prediction Probabilities for Testing Datasets

logHappyTest <- predict(LogisticHappy,newdata = testIV_4, type="response")
rpartHappyTest <- predict(rpartHappy4, newdata = testIV_4, type="prob")[, 2]
rfHappyTest <- predict(rfHappy4, newdata = testIV_4, type = "prob")[, 2]
gbmHappyTest <- predict(gbmHappy4, newdata = testIV_4, n.tree = 2000, type = "response")
svmHappyTest <- attr(pred.svmHappy4_Te, "probabilities")[, 1]
ldaHappyTest <- predict(ldaHappy4, newdata = testIV_4)$posterior[, 2]
qdaHappyTest <- predict(qdaHappy4, newdata = testIV_4)$posterior[, 2]
nbHappyTest <- predict(NBHappy4, newdata = testIV_4, type = "raw")[, 2]


#### Average Pred Prob #########

exist <- c(0,1)
finPar <- data.frame(expand.grid(exist, exist, exist, exist, exist, exist, exist, exist))
finPar$NoOfModels <- rowSums(finPar[, 1:8])
finPar$test.err <- NA
finPar$test.roc <- NA

colnames(finPar) <- c("Log", "rpart", "rf", "gbm", "svm", "lda", "qda", "nb", "NoOfModels", "test_err", "test_ROC")


### Creating Prediction Dataframe

predDF_Happy <- data.frame(Actual = testIV_4$Happy)
predDF_Happy$Log <- logHappyTest
predDF_Happy$rpart <- rpartHappyTest
predDF_Happy$rf <- rfHappyTest
predDF_Happy$gbm <- gbmHappyTest
predDF_Happy$svm <- svmHappyTest
predDF_Happy$lda <- ldaHappyTest
predDF_Happy$qda <- qdaHappyTest
predDF_Happy$nb <- nbHappyTest
predDF_Happy$PredProb <- NA


for(i in 2:nrow(finPar)){
  
  #Calculating the average prediction probability
  predDF_Happy$PredProb <- apply(predDF_Happy[, 2:9], 1, function(x) sum(x*(finPar[i, 1:8]))/finPar$NoOfModels[i])
  
  # Calculate Test Error
  finPar$test_err[i] <- mean(as.numeric(as.character(round(predDF_Happy$PredProb, 0))) != as.numeric(as.character(predDF_Happy$Actual)))
  
  
  # Calculate Test AUC
  pred = prediction(predDF_Happy$PredProb, predDF_Happy$Actual)
  finPar$test_ROC[i] <- as.numeric(performance(pred, "auc")@y.values)
  
  
  
  cat(i, " ")
}


View(finPar)






#### For Submission ######

TestIV_4 <- Test_4 %>% select(one_of(FinalVarImp))

ForSubmission <- predict(gbmHappy4, newdata = TestIV_4, n.tree = 2000, type = "response")




