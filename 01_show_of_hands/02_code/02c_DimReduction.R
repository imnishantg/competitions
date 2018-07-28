########## Loading Libraries ##########
library(FactoMineR)



######### Multiple Correspondence Analysis ##########

remVars <- c("UserID", "YOB", "Happy")

trainMCA <- train_4
trainMCA$Age <- as.numeric(scale(trainMCA$Age))
trainMCA$votes <- as.numeric(scale(trainMCA$votes))
trainMCA <- select(trainMCA, -UserID, -YOB)

testMCA <- test_4
testMCA$Age <- as.numeric(scale(testMCA$Age))
testMCA$votes <- as.numeric(scale(testMCA$votes))
testMCA <- select(testMCA, -UserID, -YOB)

try1 <- unlist(lapply(train_4, class))
try1 <- data.frame(Variable = names(try1), Class = try1)

qualVars <- setdiff(which(try1$Class == "factor"), c(1, 2, 110)) # Removing UserID, YOB, and Happy
quantVars <- (which(try1$Class == "numeric"))


mcaHappy <- MCA(train_4, quanti.sup = quantVars, quali.sup = qualVars, graph = TRUE)















