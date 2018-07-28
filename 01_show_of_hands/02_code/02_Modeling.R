##### Loading Libraries #######
library(rpart)
library(rpart.plot)
library(tree)
library(caret)
library(e1071)




######### Classification Models #######

split = sample.split(Train_2$Happy, SplitRatio = 0.7)
train_2 = subset(Train_2, split==TRUE)
test_2 = subset(Train_2, split==FALSE)

split = sample.split(Train_4$Happy, SplitRatio = 0.7)
train_4 = subset(Train_4, split==TRUE)
test_4 = subset(Train_4, split==FALSE)


trainForrpart <- select(train_4, -YOB, -UserID)
testForrpart <- select(test_4, -YOB, -UserID)

#### Logistic Regression ###
## Logistic regression does not work if there are missing values in the data

LogisticHappy <- glm(Happy ~ ., data = trainForrpart, family = "binomial")
summary(LogisticHappy)

LogHappy_train <- predict(LogisticHappy,data = trainForrpart, type="response")
LogHappy_train_10 <- as.character(ifelse(LogHappy_train > 0.5, 1, 0))
table(LogHappy_train_10,as.character(trainForrpart$Happy))
mean(as.character(LogHappy_train_10) == as.character(trainForrpart$Happy))

## AUC: 0.7345127
logHappy4ROC = predict(LogisticHappy, newdata = testForrpart, type = "response")
logHappy4ROC

pred = prediction(logHappy4ROC, testForrpart$Happy)
as.numeric(performance(pred, "auc")@y.values)
perf = performance(pred, "tpr", "fpr")
plot(perf)




##### RPart Models ########

rpartHappy4 <- rpart(Happy ~ ., data = trainForrpart, minbucket=50, cp = 0.06)

plot(rpartHappy4)
text(rpartHappy4, cex = 0.7)
prp(rpartHappy4)



rpartHappy_train <- predict(rpartHappy4,data = trainForrpart, type="prob")
rpartHappy_train_10 <- as.character(ifelse(rpartHappy_train[, 2] > 0.5, 1, 0))
table(rpartHappy_train_10,as.character(trainForrpart$Happy))
mean(as.numeric(rpartHappy_train_10) == as.numeric(as.character(trainForrpart$Happy)))

## AUC: 0.6554981
rpartHappy4ROC = predict(rpartHappy4, newdata = testForrpart, type = "prob")
rpartHappy4ROC

pred = prediction(rpartHappy4ROC[, 2], testForrpart$Happy)
as.numeric(performance(pred, "auc")@y.values)
perf = performance(pred, "tpr", "fpr")
plot(perf)










####### Cross Validation for RPart #####
# The following code is not working...
tr.control = trainControl(method = "cv", classProbs = TRUE)
cp.grid = expand.grid(.cp = (0:10)*0.001)

rpartHappyCV = train(trainForrpart$Happy ~ ., data = trainForrpart, method = "rpart", metric = "ROC", trControl = tr.control, tuneGrid = cp.grid)

# Extract tree
best.tree = rpartHappyCV$finalModel
prp(best.tree)
### Important Variables ####
VarImprprtCV <- data.frame(names(best.tree$variable.importance), best.tree$variable.importance)[1:25, ]


##### Using this code for rpart CV #######

minBucket = seq(5, 50, 5)
cpGrid = seq(0.001,0.1,0.005)

paramRPart <- expand.grid(minBucket, cpGrid)
paramRPart$train.roc <- NA
paramRPart$test.roc <- NA
colnames(paramRPart) <- c("minBucket", "cp", "Train_ROC", "Test_ROC")

for(i in 1:nrow(paramRPart)) {
  
  # Creating Boosting Model
  rpartHappy4CV <- rpart(Happy ~ ., data = trainForrpart, minbucket=paramRPart[i, 1], cp = paramRPart[i, 2])
  # Predicting on train and test data
  pred.rpart_Train <- predict(rpartHappy4CV, newdata = trainForrpart, type="prob")
  pred.rpart_Test <- predict(rpartHappy4CV, newdata = testForrpart, type="prob")
  
  # Calculating ROC for Training Set
  predROC_Train = prediction(pred.rpart_Train[, 2], trainForrpart$Happy)
  paramRPart[i, 3] <- as.numeric(performance(predROC_Train, "auc")@y.values)
  
  # Calculating ROC for Test Set  
  predROC_Test = prediction(pred.rpart_Test[, 2], testForrpart$Happy)
  paramRPart[i, 4] <- as.numeric(performance(predROC_Test, "auc")@y.values)
  
  cat(i, " ")
}



##### Prediction of the best tree after cross - validation #####

pred.treeCVHappy4 = predict(best.tree, trainForrpart)
pred.treeCVHappy4_10 <- ifelse(pred.treeCVHappy4[, 2] > 0.5, 1, 0)

table(pred.treeHappy2_10,as.character(test_2$Happy))
mean(as.character(pred.treeHappy2_10) == as.numeric(as.character(test_2$Happy)))







######### Tree Models #####


## For Train 2
# Misclassification Error -> 0.28
treeHappy2 = tree(Happy ~ . -UserID -YOB, data = train_2)
summary(treeHappy2)
plot(treeHappy2, main = "Decision Tree for Happiness")
text(treeHappy2, pretty = 0, cex = 0.7)


## For Train 4 - PATHETIC!!!
treeHappy4 = tree(Happy ~ . -UserID -YOB, data = train_4)
summary(treeHappy4)
plot(treeHappy4, main = "Decision Tree for Happiness")
text(treeHappy4, pretty = 0, cex = 0.7)




######## For Tree Predictions ####

# For P
pred.treeHappy2 = predict(treeHappy2, test_2)
pred.treeHappy2_10 <- ifelse(pred.treeHappy2[, 2] > 0.5, 1, 0)

table(pred.treeHappy2_10,as.character(test_2$Happy))
mean(as.character(pred.treeHappy2_10) == as.numeric(as.character(test_2$Happy)))











tree_P = tree(Happy ~ . -UserID -YOB, data = train_2)
summary(tree_P)
plot(tree_P, main = "Decision Tree (for Persistent Patients)")
text(tree_P, pretty = 0, cex = 0.7)

tree.pred_P = predict(tree_P, test_2)
tree.pred_P_NEW <- ifelse(tree.pred_P[, 2] > 0.5, 1, 0)
table(as.factor(tree.pred_P_NEW), test_2$Happy)
mean(tree.pred_P_NEW == test_2$Happy)











