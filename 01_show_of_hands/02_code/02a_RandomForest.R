######## Loading Libraries ########
library(randomForest)
library(gbm)
library(ROCR)




###### Important Variables #####

VarImp <- data.frame(RF2 = VarImp_rf2, RF4 = VarImp_rf4, GBM4 = VarImpGBM_4$var)
VarImp$RF2 <- as.character(VarImp$RF2)
VarImp$RF4 <- as.character(VarImp$RF4)
VarImp$GBM4 <- as.character(VarImp$GBM4)
str(VarImp)

FinalVarImp <- intersect(intersect(VarImp$RF2[1:20], VarImp$RF4[1:20]), VarImp$GBM4[1:20])


###### RandomForest Models #####


## For Train_2
rfHappy2 = randomForest(Happy ~ . -UserID -YOB, data = train_2, na.action = na.roughfix)
rfHappy2

plot(rfHappy2, log="y")
varImpPlot(rfHappy2)


rfHappy2_varImp <- data.frame(rownames(rfHappy2$importance), rfHappy2$importance)
colnames(rfHappy2_varImp) <- c("Variables", "Importance")
# Top 25 Important variables -- Use them in the second round
VarImp_rf2 <- as.character(arrange(rfHappy2_varImp, desc(Importance))[, 1])







## For Train_4

train4ForRF <- train_4 %>% select(-YOB, -UserID)
test4ForRF <- test_4 %>% select(-YOB, -UserID)

rfHappy4 = randomForest(Happy ~ ., data = train4ForRF, ntree = 1500, mtry = 20)
rfHappy4
plot(rfHappy4, log="y")
varImpPlot(rfHappy4)


rfHappy4_varImp <- data.frame(rownames(rfHappy4$importance), rfHappy4$importance)
colnames(rfHappy4_varImp) <- c("Variables", "Importance")
# Top 25 Important variables -- Use them in the second round
VarImp_rf4 <- as.character(arrange(rfHappy4_varImp, desc(Importance))[, 1])


###### For RandomForest Prediction

pred.rf_Happy4 <- predict(rfHappy4, newdata = test4ForRF, type = "prob")
pred.rf_Happy4_NEW <- ifelse(pred.rf_Happy4[, 2] > 0.5, 1, 0)
table(as.character(pred.rf_Happy4_NEW), as.character(test4ForRF$Happy))
mean(as.numeric(as.character(pred.rf_Happy4_NEW)) == as.numeric(as.character(test4ForRF$Happy)))

pred = prediction(pred.rf_Happy4[, 2], test4ForRF$Happy)
as.numeric(performance(pred, "auc")@y.values)
perf = performance(pred, "tpr", "fpr")
plot(perf)



## OOB Error
rfHappy4$err.rate
pred = predict(rfHappy4, newdata = test4ForRF, type = "prob")
## Test Error
mean(as.numeric(as.character(test4ForRF$Happy)) != round(pred[, 2], 0))


###### Cross Validation for RandomForest ######

oob.err = double(30)
test.err = double(30)
for (mtry in 1:30) {
  rf_fit_DS1 = randomForest(isDS1 ~ . -PID -PID_State -isDO -isP, data = train, mtry = mtry, ntree = 350)
  oob.err[mtry] = rf_fit_DS1$err.rate[350]
  pred = predict(rf_fit_DS1, test)
  test.err[mtry] = mean(test$isDS1 != pred)
  cat(mtry, " ")
}

###### Second Type of Coss Validation for Random Forest ########

# Tuning Parameters: ntree, mtry, 

numtree = seq(250, 2500, 250)
mTry = seq(5, 80, 5)

paramRF <- expand.grid(numtree, mTry)
paramRF$train.roc <- NA
paramRF$test.roc <- NA
paramRF$oob.err <- NA
paramRF$test.err <- NA
paramRF$train.err <- NA

colnames(paramRF) <- c("NTree", "MTRY", "Train_ROC", "Test_ROC", "OOB Error", "Test Error", "Train Error")

for(i in 1:nrow(paramRF)) {
  
  # Creating Boosting Model
  rfHappy4CV <- randomForest(Happy ~ ., data = train4ForRF, ntree = paramRF[i, 1], mtry = paramRF[i, 2])
  
  # Predicting on train and test data
  pred.rf_Train <- predict(rfHappy4CV, newdata = train4ForRF, type = "prob")
  pred.rf_Test <- predict(rfHappy4CV, newdata = test4ForRF, type = "prob")
  
  # Calculating ROC for Training Set
  predROC_Train = prediction(pred.rf_Train[, 2], train4ForRF$Happy)
  paramRF[i, 3] <- as.numeric(performance(predROC_Train, "auc")@y.values)
  
  # Calculating ROC for Test Set  
  predROC_Test = prediction(pred.rf_Test[, 2], test4ForRF$Happy)
  paramRF[i, 4] <- as.numeric(performance(predROC_Test, "auc")@y.values)
  
  # OOB Error
  paramRF[i, 5] <- rfHappy4$err.rate[paramRF[i, 1]]
  
  # Training and Test Error
  paramRF[i, 6] <- mean(as.numeric(as.character(test4ForRF$Happy)) != round(pred.rf_Test[, 2], 0))
  paramRF[i, 7] <- mean(as.numeric(as.character(train4ForRF$Happy)) != round(pred.rf_Train[, 2], 0))
  
  cat(i, " ")
}

View(paramRF)




## Imputed randomForest ######

trainForRF <- select(train_2, -YOB, -UserID)

trainForRF.imputed <- rfImpute(Happy ~ ., data= trainForRF)
rfHappy = randomForest(Happy ~ ., data = trainForRF.imputed)
rfHappy
plot(rfHappy, log="y")
varImpPlot(rfHappy)



### Another RandomForest



rfHappy = randomForest(Happy ~ votes + Age + EducationLevel + Income + Party + Q118237 + Q101162 + Q107869 + Q119334 + Q102906, data = train_2, na.action = na.roughfix)
rfHappy
plot(rfHappy, log="y")
varImpPlot(rfHappy)

#### Prediction ####

pred.rfHappy <- predict(rfHappy2, newdata = test_2, type = "prob")
pred.rfHappy_10 <- ifelse(pred.rfHappy[, 2] > 0.5, 1, 0)
table(as.factor(pred.rfHappy_10), test_2$Happy)
mean(as.character(pred.rfHappy_10) == as.character(test_2$Happy))

######## ROC Curves #####
rfHappy4ROC = predict(rfHappy4, newdata = test_4, type = "prob")
rfHappy4ROC

## AUC: 0.7465089
pred = prediction(rfHappy4ROC[,2], test_4$Happy)
as.numeric(performance(pred, "auc")@y.values)
perf = performance(pred, "tpr", "fpr")
plot(perf)


######## For Cross-Validation ########
# Not running... some bug
trainForRF_4 <- select(train4ForRF, -UserID, -YOB)
rf.grid <- expand.grid(.mtry=seq(5, 50, 5), .ntree = seq(100,1000,100))
rf.control <- trainControl(method="cv", number=10, classProbs = TRUE, summaryFunction = twoClassSummary, verboseIter=TRUE)

rfHappyCV_4 <- train(train4ForRF$Happy ~., 
             data=train4ForRF, method="rf", 
             metric = "ROC",
             trControl=rf.control,
             tuneLength=5)















####### Gradient Boosting Machines ######

gbmHappy2 = gbm(as.factor(as.numeric(as.character(Happy))) ~ . -UserID -YOB, data = train_2, distribution = "bernoulli", n.trees = 1000, shrinkage = 0.1, interaction.depth = 2)
summary(gbmHappy2)
gbmHappy2 <-train(Happy ~ . -UserID -YOB, data = train_2, method="gbm", distribution="bernoulli")
summary(gbmHappy2)
pred.gbmHappy2 <- predict(gbmHappy2, newdata = train_2, type = "prob")



### Important Variables #####

VarImpGBM_2 <- summary(gbmHappy2)




### For Train_4 ###

gbmHappy4 = gbm(as.numeric(as.character(Happy)) ~ . -UserID -YOB, data = train_4, distribution = "bernoulli", n.trees = 1500, shrinkage = 0.006, interaction.depth = 2)
summary(gbmHappy4)
VarImpGBM_4 <- summary(gbmHappy4)
VarImpGBM_4$var <- as.character(VarImpGBM_4$var)
gbmHappy4

## error rate
predGBM_Train <- predict(gbmHappy4, newdata = test_4, n.tree = 1500, type = "response")
mean(round(predGBM_Train, 0) != as.numeric(as.character(test_4$Happy)))


### ROC for GBM
pred = prediction(predGBM_Train, test_4$Happy)
as.numeric(performance(pred, "auc")@y.values)
perf = performance(pred, "tpr", "fpr")
plot(perf)



#### CV for GBM #####
###### Setting the Hyperparameters #########

ntrees = seq(500, 1500, 250)
shrink = seq(0.001, 0.03, 0.005)
depth = c(2, 3, 4)

paramGBM <- expand.grid(ntrees, shrink, depth)
paramGBM$train.err <- NA
paramGBM$test.err <- NA
paramGBM$train.roc <- NA
paramGBM$test.roc <- NA

colnames(paramGBM) <- c("NTree", "Shrink", "Depth", "Train_Error", "Test_Error", "Train_ROC", "Test_ROC")


for(i in 1:nrow(paramGBM)) {
  
  # Creating Boosting Model
  gbmHappy4CV = gbm(as.numeric(as.character(Happy)) ~ . -UserID -YOB, data = train_4, distribution = "bernoulli", n.trees = paramGBM[i, 1], shrinkage = paramGBM[i, 2], interaction.depth = paramGBM[i, 3])
  
  # Predicting on train and test data
  predGBM_Train <- predict(gbmHappy4CV, newdata = train_4, n.tree = paramGBM[i, 1], type = "response")
  predGBM_Test <- predict(gbmHappy4CV, newdata = test_4, n.tree = paramGBM[i, 1], type = "response")
  
  # Calculate the training and test error rate
  paramGBM[i, 4] <- mean(round(predGBM_Train, 0) != as.numeric(as.character(train_4$Happy)))
  paramGBM[i, 5] <- mean(round(predGBM_Test, 0) != as.numeric(as.character(test_4$Happy)))
  
  
  # Calculate the training and test AUC values
  predGBM_Tr = prediction(predGBM_Train, train_4$Happy)
  paramGBM[i, 6] <- as.numeric(performance(predGBM_Tr, "auc")@y.values)
  
  predGBM_Te = prediction(predGBM_Test, test_4$Happy)
  paramGBM[i, 7] <- as.numeric(performance(predGBM_Te, "auc")@y.values)
  
  
  cat(i, " ")
}

View(paramGBM)









