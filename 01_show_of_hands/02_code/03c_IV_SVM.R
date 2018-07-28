#### Loading Libraries #####
library(e1071)
library(MASS)
library(caret)


##### Trying out SVM ######

svmHappy4 = svm(Happy ~ ., data = trainIV_4, probability = TRUE, type="C-classification", gamma = 0.004, cost = 3.5)
print(svmHappy4)

pred.svmHappy4 <- predict(svmHappy4, newdata = testIV_4, probability = TRUE)
table(pred.svmHappy4, testIV_4$Happy)
mean(pred.svmHappy4 != testIV_4$Happy)

## To get the probabilities
attr(pred.svmHappy4, "probabilities")[1:10, ]

### ROC for SVM: AUC - 0.7575604 with gamma = 0.004, cost = 3.5
predSVM = prediction(attr(pred.svmHappy4, "probabilities")[, 1], testIV_4$Happy)
as.numeric(performance(predSVM, "auc")@y.values)
perfSVM = performance(predSVM, "tpr", "fpr")
plot(perfSVM)




plot(perfSVM, colorize = T, main = "ROC: SVM Model", sub = "AUC: 0.7575604", type = "o", lwd = 3); abline(a = 0, b = 1, lwd = 2)

acc.perf.svm = performance(predSVM, measure = "acc")
plot(acc.perf.svm, lwd = 3, main = "SVM: Accuracy vs Cutoff", sub = "Accuracy Maximum at: 52.2%")


AccuracySVM <- data.frame(Cutoff = acc.perf.svm@x.values, Accuracy = acc.perf.svm@y.values)
colnames(AccuracySVM) <- c("CutOff", "Accuracy")






##### Cross Validation of SVM #######

# The Kernel: 'Radial'
# The Tuning Parameters: Gamma (0.0016), cost (1.0)



gamma = seq(0.001, 0.01, 0.001)
cost = seq(2.5, 4.5, 0.5)


paramSVM <- expand.grid(gamma, cost)
paramSVM$train.err <- NA
paramSVM$test.err <- NA
paramSVM$train.roc <- NA
paramSVM$test.roc <- NA

colnames(paramSVM) <- c("Gamma", "Cost", "Train_Error", "Test_Error", "Train_ROC", "Test_ROC")


for(i in 1:nrow(paramSVM)) {
  
  # Creating Boosting Model
  svmHappy4CV = svm(Happy ~ ., data = trainIV_4, probability = TRUE, type="C-classification", gamma = paramSVM[i, 1], cost = paramSVM[i, 2])
  
  # Predicting on train and test data
  predSVM_Train <- predict(svmHappy4CV, newdata = trainIV_4, probability = TRUE)
  predSVM_Test <- predict(svmHappy4CV, newdata = testIV_4, probability = TRUE)
  
  # Calculate the training and test error rate
  paramSVM[i, 3] <- mean(predSVM_Train != trainIV_4$Happy)
  paramSVM[i, 4] <- mean(predSVM_Test != testIV_4$Happy)
  
  
  # Calculate the training and test AUC values
  predSVM_Tr = prediction(attr(predSVM_Train, "probabilities")[, 1], trainIV_4$Happy)
  paramSVM[i, 5] <- as.numeric(performance(predSVM_Tr, "auc")@y.values)
  
  predSVM_Te = prediction(attr(predSVM_Test, "probabilities")[, 1], testIV_4$Happy)
  paramSVM[i, 6] <- as.numeric(performance(predSVM_Te, "auc")@y.values)
  
  
  cat(i, " ")
}

View(paramSVM)

select(paramSVM, Gamma, Cost, Test_ROC) %>% spread(Cost, Test_ROC)


######## LDA #####


ldaHappy4 = lda(Happy ~ ., data = trainIV_4)
print(ldaHappy4)
plot(ldaHappy4)
plot(ldaHappy4, dimen=1, type="both")


pred.ldaHappy4 <- predict(ldaHappy4, newdata = testIV_4)
table(pred.ldaHappy4$class, testIV_4$Happy)
mean(pred.ldaHappy4$class != testIV_4$Happy)


### ROC for LDA: AUC - 0.7531964
predLDA = prediction(pred.ldaHappy4$posterior[, 2], testIV_4$Happy)
as.numeric(performance(predLDA, "auc")@y.values)
perfLDA = performance(predLDA, "tpr", "fpr")
plot(perfLDA)


plot(perfLDA, colorize = T, main = "ROC: LDA Model", sub = "AUC: 0.7531964", type = "o", lwd = 3); abline(a = 0, b = 1, lwd = 2)

acc.perf.lda = performance(predLDA, measure = "acc")
plot(acc.perf.lda, lwd = 3, main = "LDA: Accuracy vs Cutoff", sub = "Accuracy Maximum at: 54.5%")


AccuracyLDA <- data.frame(Cutoff = acc.perf.lda@x.values, Accuracy = acc.perf.lda@y.values)
colnames(AccuracyLDA) <- c("CutOff", "Accuracy")
View(AccuracyLDA)






### Using Caret 

ldaFit <- train(Happy ~ ., method='lda', preProcess=c('scale', 'center'), data=trainIV_4)
confusionMatrix(testIV_4$Happy, predict(ldaFit, testIV_4))


### QDA #####

qdaHappy4 = qda(Happy ~ ., data = trainIV_4)
print(qdaHappy4)


pred.qdaHappy4 <- predict(qdaHappy4, newdata = testIV_4)
table(pred.qdaHappy4$class, testIV_4$Happy)
mean(pred.qdaHappy4$class != testIV_4$Happy)


### ROC for qda: AUC - 0.6938282
pred = prediction(pred.qdaHappy4$posterior[, 2], testIV_4$Happy)
as.numeric(performance(pred, "auc")@y.values)
perf = performance(pred, "tpr", "fpr")
plot(perf)










qdaFit <- train(Happy ~ ., method='qda', preProcess=c('scale', 'center'), data=trainIV_4)
confusionMatrix(testIV_4$Happy, predict(ldaFit, testIV_4))


##### Naive Bayes #########


NBHappy4 = naiveBayes(Happy ~ ., data = trainIV_4)
print(NBHappy4)

pred.NBHappy4 <- predict(NBHappy4, newdata = testIV_4, type = "raw")
table(round(pred.NBHappy4[, 2], 0), testIV_4$Happy)
mean(as.character(round(pred.NBHappy4[, 2], 0)) != as.character(testIV_4$Happy))

## To get the probabilities
pred.NBHappy4[, 2]

### ROC for NB: AUC - 0.7507126
predNB = prediction(pred.NBHappy4[, 2], testIV_4$Happy)
as.numeric(performance(predNB, "auc")@y.values)
perfNB = performance(predNB, "tpr", "fpr")
plot(perfNB)





plot(perfNB, colorize = T, main = "ROC: Naive Bayes Model", sub = "AUC: 0.7507126", type = "o", lwd = 3); abline(a = 0, b = 1, lwd = 2)

acc.perf.nb = performance(predNB, measure = "acc")
plot(acc.perf.nb, lwd = 3, main = "Naive Bayes: Accuracy vs Cutoff", sub = "Accuracy Maximum at: 50.3%")


AccuracyNB <- data.frame(Cutoff = acc.perf.nb@x.values, Accuracy = acc.perf.nb@y.values)
colnames(AccuracyNB) <- c("CutOff", "Accuracy")








