#### Loading Libraries #####
library(e1071)
library(MASS)



##### Trying out SVM ######

train4Forsvm <- select(train_4, -UserID, -YOB)
test4Forsvm <- select(test_4, -UserID, -YOB)

svmHappy4 = svm(Happy ~ ., data = train4Forsvm, probability = TRUE, type="C-classification", gamma = 0.006, cost = 10)
print(svmHappy4)

pred.svmHappy4 <- predict(svmHappy4, newdata = test4Forsvm, probability = TRUE)
table(pred.svmHappy4, test_4$Happy)
mean(pred.svmHappy4 != test_4$Happy)

## To get the probabilities
attr(pred.svmHappy4, "probabilities")[1:10, ]

### ROC for SVM
pred = prediction(attr(pred.svmHappy4, "probabilities")[, 1], test4Forsvm$Happy)
as.numeric(performance(pred, "auc")@y.values)
perf = performance(pred, "tpr", "fpr")
plot(perf)



##### Cross Validation fo SVM #######

# The Kernel: 'Radial'
# The Tuning Parameters: Gamma (0.0016), cost (1.0)



gamma = seq(0.0006, 0.0021, 0.0005)
cost = seq(10, 50, 5)


paramSVM <- expand.grid(gamma, cost)
paramSVM$train.err <- NA
paramSVM$test.err <- NA
paramSVM$train.roc <- NA
paramSVM$test.roc <- NA

colnames(paramSVM) <- c("Gamma", "Cost", "Train_Error", "Test_Error", "Train_ROC", "Test_ROC")


for(i in 1:nrow(paramSVM)) {
  
  # Creating Boosting Model
  svmHappy4CV = svm(Happy ~ ., data = train4Forsvm, probability = TRUE, type="C-classification", gamma = paramSVM[i, 1], cost = paramSVM[i, 2])
  
  # Predicting on train and test data
  predSVM_Train <- predict(svmHappy4CV, newdata = train4Forsvm, probability = TRUE)
  predSVM_Test <- predict(svmHappy4CV, newdata = test4Forsvm, probability = TRUE)
  
  # Calculate the training and test error rate
  paramSVM[i, 3] <- mean(predSVM_Train != train4Forsvm$Happy)
  paramSVM[i, 4] <- mean(predSVM_Test != test4Forsvm$Happy)
  
  
  # Calculate the training and test AUC values
  predSVM_Tr = prediction(attr(predSVM_Train, "probabilities")[, 1], train4Forsvm$Happy)
  paramSVM[i, 5] <- as.numeric(performance(predSVM_Tr, "auc")@y.values)
  
  predSVM_Te = prediction(attr(predSVM_Test, "probabilities")[, 1], test4Forsvm$Happy)
  paramSVM[i, 6] <- as.numeric(performance(predSVM_Te, "auc")@y.values)
  
  
  cat(i, " ")
}

View(paramSVM)







###### LDA #######
Maybe after PCA

ldaHappy4 = lda(Happy ~ ., data = train4Forsvm)
print(svmHappy4)

pred.svmHappy4 <- predict(svmHappy4, newdata = test4Forsvm, probability = TRUE)
table(pred.svmHappy4, test_4$Happy)
mean(pred.svmHappy4 != test_4$Happy)
















