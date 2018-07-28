######## Loading Libraries ########
library(randomForest)
library(gbm)
library(ROCR)





##### Random Forest #####
rfHappy4 = randomForest(Happy ~ ., data = trainIV_4, ntree = 1000, mtry = 2)
rfHappy4
plot(rfHappy4, log="y")
varImpPlot(rfHappy4)


###### For RandomForest Prediction

pred.rf_Happy4 <- predict(rfHappy4, newdata = testIV_4, type = "prob")
pred.rf_Happy4_NEW <- ifelse(pred.rf_Happy4[, 2] > 0.5, 1, 0)
table(as.character(pred.rf_Happy4_NEW), as.character(testIV_4$Happy))
mean(as.numeric(as.character(pred.rf_Happy4_NEW)) == as.numeric(as.character(testIV_4$Happy)))

## AUC: 0.7322768
predRF = prediction(pred.rf_Happy4[, 2], testIV_4$Happy)
as.numeric(performance(predRF, "auc")@y.values)
perfRF = performance(predRF, "tpr", "fpr")


plot(perfRF, colorize = T, main = "ROC: Random Forest Model", sub = "AUC: 0.7322768", type = "o", lwd = 3); abline(a = 0, b = 1, lwd = 2)

acc.perf.rf = performance(predRF, measure = "acc")
plot(acc.perf.rf, lwd = 3, main = "Random Forest: Accuracy vs Cutoff", sub = "Accuracy Maximum at: 49.9%")

AccuracyRF <- data.frame(Cutoff = acc.perf.rf@x.values, Accuracy = acc.perf.rf@y.values)
colnames(AccuracyRF) <- c("CutOff", "Accuracy")




###### Cross Validation for Random Forest ########

# Tuning Parameters: ntree, mtry, 

numtree = seq(1000, 2500, 250)
mTry = seq(2, 16, 2)

paramRF <- expand.grid(numtree, mTry)
paramRF$train.roc <- NA
paramRF$test.roc <- NA
paramRF$oob.err <- NA
paramRF$test.err <- NA
paramRF$train.err <- NA

colnames(paramRF) <- c("NTree", "MTRY", "Train_ROC", "Test_ROC", "OOB Error", "Test Error", "Train Error")

for(i in 1:nrow(paramRF)) {
  
  # Creating Boosting Model
  rfHappy4CV <- randomForest(Happy ~ ., data = trainIV_4, ntree = paramRF[i, 1], mtry = paramRF[i, 2])
  
  # Predicting on train and test data
  pred.rf_Train <- predict(rfHappy4CV, newdata = trainIV_4, type = "prob")
  pred.rf_Test <- predict(rfHappy4CV, newdata = testIV_4, type = "prob")
  
  # Calculating ROC for Training Set
  predROC_Train = prediction(pred.rf_Train[, 2], trainIV_4$Happy)
  paramRF[i, 3] <- as.numeric(performance(predROC_Train, "auc")@y.values)
  
  # Calculating ROC for Test Set  
  predROC_Test = prediction(pred.rf_Test[, 2], testIV_4$Happy)
  paramRF[i, 4] <- as.numeric(performance(predROC_Test, "auc")@y.values)
  
  # OOB Error
  paramRF[i, 5] <- rfHappy4$err.rate[paramRF[i, 1]]
  
  # Training and Test Error
  paramRF[i, 6] <- mean(as.numeric(as.character(testIV_4$Happy)) != round(pred.rf_Test[, 2], 0))
  paramRF[i, 7] <- mean(as.numeric(as.character(trainIV_4$Happy)) != round(pred.rf_Train[, 2], 0))
  
  cat(i, " ")
}

View(paramRF)


##### GBM ########
gbmHappy4 = gbm(as.numeric(as.character(Happy)) ~ ., data = trainIV_4, distribution = "bernoulli", n.trees = 2000, shrinkage = 0.007, interaction.depth = 1)
summary(gbmHappy4)
VarImpGBM_4 <- summary(gbmHappy4)
VarImpGBM_4$var <- as.character(VarImpGBM_4$var)
gbmHappy4

## error rate
predGBM_test <- predict(gbmHappy4, newdata = testIV_4, n.tree = 2000, type = "response")
mean(round(predGBM_test, 0) != as.numeric(as.character(testIV_4$Happy)))


### ROC for GBM: AUC - 0.7628277 with ntree = 2000, shrinkage = 0.007, and depth= 1
predGBM = prediction(predGBM_test, testIV_4$Happy)
as.numeric(performance(predGBM, "auc")@y.values)
perfGBM = performance(predGBM, "tpr", "fpr")
plot(perfGBM)

plot(perfGBM, colorize = T, main = "ROC: GBM Model", sub = "AUC: 0.7628277", type = "o", lwd = 3); abline(a = 0, b = 1, lwd = 2)

acc.perf.gbm = performance(predGBM, measure = "acc")
plot(acc.perf.gbm, lwd = 3, main = "GBM: Accuracy vs Cutoff", sub = "Accuracy Maximum at: 51.8%")


AccuracyGBM <- data.frame(Cutoff = acc.perf.gbm@x.values, Accuracy = acc.perf.gbm@y.values)
colnames(AccuracyGBM) <- c("CutOff", "Accuracy")





### PLay around with ROCR ####
class(predGBM)
slotNames(predGBM)

perfGBM = performance(predGBM, "tpr", "fpr")
plot(perfGBM); abline(a = 0, b = 1)

cost.perf = performance(pred, "cost")

cost.perf = performance(pred, "cost")
plot(cost.perf)

acc.perf = performance(predGBM, measure = "acc")
plot(acc.perf)
AccuracyGBM <- data.frame(Cutoff = acc.perf@x.values, Accuracy = acc.perf@y.values)
colnames(AccuracyGBM) <- c("CutOff", "Accuracy")










#### CV for GBM #####
###### Setting the Hyperparameters #########

ntrees = seq(250, 5000, 250)
shrink = seq(0.001, 0.01, 0.001)
depth = c(1)

paramGBM <- expand.grid(ntrees, shrink, depth)
paramGBM$train.err <- NA
paramGBM$test.err <- NA
paramGBM$train.roc <- NA
paramGBM$test.roc <- NA

colnames(paramGBM) <- c("NTree", "Shrink", "Depth", "Train_Error", "Test_Error", "Train_ROC", "Test_ROC")


for(i in 1:nrow(paramGBM)) {
  
  # Creating Boosting Model
  gbmHappy4CV = gbm(as.numeric(as.character(Happy)) ~ ., data = trainIV_4, distribution = "bernoulli", n.trees = paramGBM[i, 1], shrinkage = paramGBM[i, 2], interaction.depth = paramGBM[i, 3])
  
  # Predicting on train and test data
  predGBM_Train <- predict(gbmHappy4CV, newdata = trainIV_4, n.tree = paramGBM[i, 1], type = "response")
  predGBM_Test <- predict(gbmHappy4CV, newdata = testIV_4, n.tree = paramGBM[i, 1], type = "response")
  
  # Calculate the training and test error rate
  paramGBM[i, 4] <- mean(round(predGBM_Train, 0) != as.numeric(as.character(trainIV_4$Happy)))
  paramGBM[i, 5] <- mean(round(predGBM_Test, 0) != as.numeric(as.character(testIV_4$Happy)))
  
  
  # Calculate the training and test AUC values
  predGBM_Tr = prediction(predGBM_Train, trainIV_4$Happy)
  paramGBM[i, 6] <- as.numeric(performance(predGBM_Tr, "auc")@y.values)
  
  predGBM_Te = prediction(predGBM_Test, testIV_4$Happy)
  paramGBM[i, 7] <- as.numeric(performance(predGBM_Te, "auc")@y.values)
  
  
  cat(i, " ")
}

View(paramGBM)

paramGBM %>% 
  select(NTree, Shrink, Test_ROC) %>% 
  spread(Shrink, Test_ROC)







