##### Loading Libraries #######
library(rpart)
library(rpart.plot)
library(tree)
library(caret)
library(e1071)


FinalVarImp <- c(FinalVarImp, "Happy")

###### New Datasets #######
trainIV_4 <- train_4 %>% select(one_of(FinalVarImp))
testIV_4 <- test_4 %>% select(one_of(FinalVarImp))


####### Logistic Regression ######## Logistic looks good... include the model #######
LogisticHappy <- glm(Happy ~ ., data = trainIV_4, family = "binomial")
summary(LogisticHappy)

LogHappy_train <- predict(LogisticHappy,data = trainIV_4, type="response")
LogHappy_train_10 <- as.character(ifelse(LogHappy_train > 0.5, 1, 0))
table(LogHappy_train_10,as.character(trainIV_4$Happy))
mean(as.character(LogHappy_train_10) == as.character(trainIV_4$Happy))

## AUC: 0.7526703
logHappy4ROC = predict(LogisticHappy, newdata = testIV_4, type = "response")
logHappy4ROC

predLOG = prediction(logHappy4ROC, testIV_4$Happy)
as.numeric(performance(predLOG, "auc")@y.values)
perfLOG = performance(predLOG, "tpr", "fpr")
plot(perfLOG)




plot(perfLOG, colorize = T, main = "ROC: Logistic Regression Model", sub = "AUC: 0.7526703", type = "o", lwd = 3); abline(a = 0, b = 1, lwd = 2)

acc.perf.log = performance(predLOG, measure = "acc")
plot(acc.perf.log, lwd = 3, main = "Logistic Regression: Accuracy vs Cutoff", sub = "Accuracy Maximum at: 53.4%")

AccuracyLOG <- data.frame(Cutoff = acc.perf.log@x.values, Accuracy = acc.perf.log@y.values)
colnames(AccuracyLOG) <- c("CutOff", "Accuracy")











#### RPart #### Not so Good #####
rpartHappy4 <- rpart(Happy ~ ., data = trainIV_4, minbucket = 40, cp = 0.001)

plot(rpartHappy4); text(rpartHappy4, cex = 0.6, minlength = 5, pretty = TRUE)

prp(rpartHappy4, type = 0, varlen = 0, cex = 0.5, faclen = 5, compress = TRUE, ycompress = TRUE, box.col = "yellow")



rpartHappy_train <- predict(rpartHappy4,data = trainIV_4, type="prob")
rpartHappy_train_10 <- as.character(ifelse(rpartHappy_train[, 2] > 0.5, 1, 0))
table(rpartHappy_train_10,as.character(trainIV_4$Happy))
mean(as.numeric(rpartHappy_train_10) == as.numeric(as.character(trainIV_4$Happy)))

## AUC: 0.6922704
rpartHappy4ROC = predict(rpartHappy4, newdata = testIV_4, type = "prob")
rpartHappy4ROC

predrpart = prediction(rpartHappy4ROC[, 2], testIV_4$Happy)
as.numeric(performance(predrpart, "auc")@y.values)
perfrpart = performance(predrpart, "tpr", "fpr")
plot(perfrpart, colorize = T, main = "ROC: RPart Model", sub = "AUC: 0.6922704", type = "o", lwd = 3); abline(a = 0, b = 1, lwd = 2)

acc.perf.rpart = performance(predrpart, measure = "acc")
plot(acc.perf.rpart,  type = "o", lwd = 3, main = "RPart: Accuracy vs Cutoff", sub = "Accuracy Maximum at: 46.1%")



##### Using this code for rpart CV #######

minBucket = seq(5, 100, 5)
cpGrid = seq(0.0001,0.01,0.0005)

paramRPart <- expand.grid(minBucket, cpGrid)
paramRPart$train.roc <- NA
paramRPart$test.roc <- NA
colnames(paramRPart) <- c("minBucket", "cp", "Train_ROC", "Test_ROC")

for(i in 1:nrow(paramRPart)) {
  
  # Creating Boosting Model
  rpartHappy4CV <- rpart(Happy ~ ., data = trainIV_4, minbucket=paramRPart[i, 1], cp = paramRPart[i, 2])
  # Predicting on train and test data
  pred.rpart_Train <- predict(rpartHappy4CV, newdata = trainIV_4, type="prob")
  pred.rpart_Test <- predict(rpartHappy4CV, newdata = testIV_4, type="prob")
  
  # Calculating ROC for Training Set
  predROC_Train = prediction(pred.rpart_Train[, 2], trainIV_4$Happy)
  paramRPart[i, 3] <- as.numeric(performance(predROC_Train, "auc")@y.values)
  
  # Calculating ROC for Test Set  
  predROC_Test = prediction(pred.rpart_Test[, 2], testIV_4$Happy)
  paramRPart[i, 4] <- as.numeric(performance(predROC_Test, "auc")@y.values)
  
  cat(i, " ")
}

View(paramRPart)

select(paramRPart, minBucket, cp, Test_ROC) %>% spread(cp, Test_ROC)


######### Trees #####

treeHappy4 = tree(Happy ~ ., data = trainIV_4)
summary(treeHappy4)
plot(treeHappy4, main = "Decision Tree for Happiness")
text(treeHappy4, pretty = 0, cex = 0.7)















