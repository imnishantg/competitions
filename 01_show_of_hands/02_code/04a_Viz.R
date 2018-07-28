######## Viasualizations #######
library(ggplot2)
library(ggvis)



####### Missing Values ########
## Personal Attributes: Age, Gender, HouseholdStatus, EducationLevel, Party, Income



MIValmat <- 1*as.matrix(is.na(sohTest))
image(x = 1:nrow(MIValmat), 
      y = 1:ncol(MIValmat), 
      z = MIValmat,
      breaks = c(min(MIValmat), 0, 1, max(MIValmat)), 
      col = c("white", "violetred4", "white"), 
      main = "Missing Values in Test Dataset", 
      xlab = "Rows", ylab = "Columns",
      sub = "Percentage of Values Missing: 27.7%")




dummy <- as.matrix(as.numeric(is.na(ShowofHands)))
x <- col(dummy)[dummy==1]
y <- row(dummy)[dummy==1]
par(bg="black", fg="white", col.axis="white", col.lab="white", col.sub="white", col.main="white", pch=22)
plot(x, y, type="n")
points(x,y)





# 1) MI in Income
# a) wrt Age
MI_income_wrtAge <- ggplot(Train_1, aes(x = Income, y = Age, color = as.factor(is.na(Income)))) + geom_jitter() 
MI_income_wrtAge <- MI_income_wrtAge + geom_violin(trim = FALSE)
MI_income_wrtAge + ggtitle("MIssing Values in 'Income' Variable, wrt to Age") + coord_flip()


# b) wrt Age and Education
MI_income_wrtAge <- ggplot(Train_1, aes(x = Income, y = Age, color = as.factor(is.na(Income)))) + geom_jitter() 
MI_income_wrtAge <- MI_income_wrtAge
MI_income_wrtAge + ggtitle("MIssing Values in 'Income' Variable, wrt to Age") + coord_flip() + facet_wrap(~ EducationLevel)



# 2) MI in Household Status
# a) wrt Age
MI_income_wrtAge <- ggplot(Train_1, aes(x = HouseholdStatus, y = Age, color = as.factor(is.na(HouseholdStatus)))) + geom_jitter() 
MI_income_wrtAge <- MI_income_wrtAge + geom_violin(trim = FALSE)
MI_income_wrtAge + ggtitle("Missing Values in 'HouseholdStatus' Variable, wrt to Age") + coord_flip()


# a) wrt Age
MI_income_wrtAge <- ggplot(Train_1, aes(x = HouseholdStatus, y = Age, color = as.factor(is.na(HouseholdStatus)))) + geom_jitter() 
MI_income_wrtAge <- MI_income_wrtAge + geom_violin(trim = FALSE)
MI_income_wrtAge + ggtitle("Missing Values in 'HouseholdStatus' Variable, wrt to Age") + coord_flip() + facet_wrap(~ Income)



# 3) MI in Age
ggplot(Train_1, aes(x = EducationLevel, y = HouseholdStatus, color = as.factor(is.na(Age)))) + geom_jitter()
MI_Age



## Missing Values' Proportion in each column

ggplot(MissVal, aes(x =  reorder(Variables, NoNAs), y = PropNAs, fill = PropNAs)) + geom_bar(stat = "identity") + coord_flip()


p2 <- ggplot(MissVal, aes(x = reorder(Variables, PropNAs), y = PropNAs, fill = as.factor(floor(20*PropNAs)), ymax = 1, label=paste0(100*round(PropNAs, 3), "%")))
p2 <- p2 + geom_bar(width = 1, stat = "identity", color = "black", position = position_dodge(width = 1))
p2 <- p2 + ggtitle("Proportion of Missing Values in Training Dataset") + ylab("Proportion of Missing Value") + xlab("Variable")
p2 + geom_text(aes(label=paste0(100*round(PropNAs, 3), "%")), position=position_dodge(width=0.9), hjust = -0.5, size = 3) + coord_flip()









###### Visualizations for All Variables Models #########

#### Random Forest: Variable's Strength


p2 <- ggplot(rfHappy4_varImp[1:25, ], aes(x = reorder(Variables, Importance), y = Importance, fill = -Importance, label = round(Importance, 1))) 
p2 <- p2 + geom_bar(width = 1, stat = "identity", color = "black", position = position_dodge(width = 1)) 
p2 <- p2 + ggtitle("Important Variables, acc. to Random Forest") + ylab("Variable Importance") + xlab("Variable")
p2 + geom_text(aes(label=round(Importance, 1)), position=position_dodge(width=0.9), hjust = -0.5, size = 3) + coord_flip()





dfgbmIV <- summary(gbmHappy4)

p2 <- ggplot(dfgbmIV[1:25, ], aes(x = reorder(var, rel.inf), y = rel.inf, fill = -rel.inf, label = round(rel.inf, 1))) 
p2 <- p2 + geom_bar(width = 1, stat = "identity", color = "black", position = position_dodge(width = 1)) 
p2 <- p2 + ggtitle("Important Variables, acc. to Gradient Boosting Model") + ylab("Variable Importance") + xlab("Variable")
p2 + geom_text(aes(label=round(rel.inf, 1)), position=position_dodge(width=0.9), hjust = -0.5, size = 3) + coord_flip()















###### Visualizations for Important Variables Models #########

#### Random Forest: Variable's Strength

dfrfIV <- data.frame(Variable = rownames(rfHappy4$importance), MeanDecreaseGini = rfHappy4$importance)

p2 <- ggplot(dfrfIV, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini, fill = -MeanDecreaseGini, label = round(MeanDecreaseGini, 1))) 
p2 <- p2 + geom_bar(width = 1, stat = "identity", color = "black", position = position_dodge(width = 1)) 
p2 <- p2 + ggtitle("Important Variables, acc. to Random Forest") + ylab("Variable Importance") + xlab("Variable")
p2 + geom_text(aes(label=round(MeanDecreaseGini, 1)), position=position_dodge(width=0.9), hjust = -0.5, size = 3) + coord_flip()


#### GBM: Variable's Strength

dfgbmIV <- summary(gbmHappy4)

p2 <- ggplot(dfgbmIV, aes(x = reorder(var, rel.inf), y = rel.inf, fill = -rel.inf, label = round(rel.inf, 1))) 
p2 <- p2 + geom_bar(width = 1, stat = "identity", color = "black", position = position_dodge(width = 1)) 
p2 <- p2 + ggtitle("Important Variables, acc. to Gradient Boosting Model") + ylab("Variable Importance") + xlab("Variable")
p2 + geom_text(aes(label=round(rel.inf, 1)), position=position_dodge(width=0.9), hjust = -0.5, size = 3) + coord_flip()





###### Multiple ROC Curves #####

ROCCurves_RF <- data.frame(rf_fpr = perfRF@x.values, rf_tpr = perfRF@y.values)
colnames(ROCCurves_RF) <- c("FPR", "TPR")

ROCCurves_GBM <- data.frame(gbm_fpr = perfGBM@x.values, gbm_tpr = perfGBM@y.values)
colnames(ROCCurves_GBM) <- c("FPR", "TPR")

ROCCurves_SVM <- data.frame(svm_fpr = perfSVM@x.values, svm_tpr = perfSVM@y.values)
colnames(ROCCurves_SVM) <- c("FPR", "TPR")

ROCCurves_NB <- data.frame(nb_fpr = perfNB@x.values, nb_tpr = perfNB@y.values)
colnames(ROCCurves_NB) <- c("FPR", "TPR")

ROCCurves_LOG <- data.frame(log_fpr = perfLOG@x.values, log_tpr = perfLOG@y.values)
colnames(ROCCurves_LOG) <- c("FPR", "TPR")



ROCCurves <- ggplot() + geom_line(data = ROCCurves_RF, aes(x = FPR, y = TPR), color = "springgreen4", size = 1)
ROCCurves <- ROCCurves + geom_line(data = ROCCurves_GBM, aes(x = FPR, y = TPR), color = "black", size = 1)
ROCCurves <- ROCCurves + geom_line(data = ROCCurves_SVM, aes(x = FPR, y = TPR), color = "yellow4", size = 1)
ROCCurves <- ROCCurves + geom_line(data = ROCCurves_NB, aes(x = FPR, y = TPR), color = "deeppink4", size = 1)
ROCCurves <- ROCCurves + geom_line(data = ROCCurves_LOG, aes(x = FPR, y = TPR), color = "royalblue4", size = 1)
ROCCurves <- ROCCurves + ggtitle("ROC Curves: Best 5 Models") + ylab("True Positive Rate") + xlab("False Positive Rate")
ROCCurves






Ques <- mutate(QuesBank, Question.ID = as.character(paste0("Q",Question.ID)))


questionsImp <- FVI_DF %>% 
  rename(Question.ID = Variables)  %>% 
  left_join(Ques, by = "Question.ID")













