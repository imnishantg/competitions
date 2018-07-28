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


###### Reading the Datasets #######
sohTest <- read.csv("./Kaggle/ShowOfHands/test.csv", header = TRUE, na.strings = c(NA, ""), stringsAsFactors= FALSE)
ShowofHands <- read.csv("./Kaggle/ShowOfHands/train.csv", header = TRUE, na.strings = c(NA, ""), stringsAsFactors= FALSE)
QuesBank <- read.table("./Kaggle/ShowOfHands/QuestionBank.txt", header= TRUE, sep = "\t", stringsAsFactors= FALSE)

str(ShowofHands)
str(QuesBank)


##### Following are the way in which the train and the test data can be pre-processed

# 1) Combine the train and test data -> create new features and 'Keep the Question's NAs as NAs' -> isolate the train and test data -> filter the train data
# 2) Combine the train and test data -> create new features and 'Turn the Questions' NAs as 'Skipped'' -> isolate the train and test data -> filter the train data
# 3) Combine the train and test data -> create new features and 'Perform Multiple Imputation on Questions' Replies' -> isolate the train and test data -> filter the train data


###### Preprocessing the Dataset #######

# 1) Combine the train and test data -> create new features and 'Keep the Question's NAs as NAs' -> isolate the train and test data -> filter the train data
ShowofHands <- ShowofHands[, c(1:7, 9:110, 8)]
colnames(ShowofHands)
ShowofHands$FROM <- "Train"

sohTest$Happy <- NA
sohTest$FROM <- "Test"

## Rbind the data sets
SOHMain <- bind_rows(ShowofHands, sohTest)

## Relevel the factor Variables 
SOHMain$Income = factor(SOHMain$Income, levels= c("under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000", "$100,001 - $150,000", "over $150,000"))
SOHMain$HouseholdStatus = factor(SOHMain$HouseholdStatus, levels = c("Domestic Partners (no kids)", "Domestic Partners (w/kids)", "Married (no kids)", "Married (w/kids)", "Single (no kids)", "Single (w/kids)"))
SOHMain$EducationLevel = factor(SOHMain$EducationLevel, levels= c("Current K-12", "High School Diploma", "Current Undergraduate", "Associate's Degree", "Bachelor's Degree", "Master's Degree", "Doctoral Degree"))

## Creating New Variables ###

SOHMain$Age <- 2013 - as.numeric(as.character(SOHMain$YOB))
SOHMain$Kids = ifelse(is.na(SOHMain$HouseholdStatus) == TRUE, NA,ifelse(SOHMain$HouseholdStatus %in% c("Domestic Partners (w/kids)", "Married (w/kids)", "Single (w/kids)"), "Yes", "No"))



###### Datasets No.1: Demographics and Questions, Both have NAs ######

Train_1 <- filter(SOHMain, FROM == "Train") %>% select(-FROM)
Test_1 <- filter(SOHMain, FROM == "Test") %>% select(-FROM)




###### Datasets No. 2: Demographics have imputed values and Questions have NAs ######
SOHMain_MIDemo <- SOHMain
MIVars <- c("Gender", "Income", "HouseholdStatus", "EducationLevel", "Party", "Age", "Kids")

#### K Nearest Neighbour Imputation using all the other variables
# try1 <-kNN(as.matrix(SOHMain_MIDemo), variable = MIVars)
# SOHMain_MIDemog <- try1[, 1:113]

#### K Nearest Neighbour Imputation using only those variables
try2 <-kNN(as.matrix(SOHMain_MIDemo), variable = MIVars, dist_var = MIVars)
SOHMain_MIDemog2 <- try2[, 1:113]
rm(try1, try2)

SOHMain_MIDemog2 <- as.data.frame(SOHMain_MIDemog2)
SOHMain_MIDemog2$Age <- as.numeric(as.character(SOHMain_MIDemog2$Age))
SOHMain_MIDemog2$votes <- as.numeric(as.character(SOHMain_MIDemog2$votes))
SOHMain_MIDemog2$Income = factor(SOHMain_MIDemog2$Income, levels= c("under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000", "$100,001 - $150,000", "over $150,000"))
SOHMain_MIDemog2$HouseholdStatus = factor(SOHMain_MIDemog2$HouseholdStatus, levels = c("Domestic Partners (no kids)", "Domestic Partners (w/kids)", "Married (no kids)", "Married (w/kids)", "Single (no kids)", "Single (w/kids)"))
SOHMain_MIDemog2$EducationLevel = factor(SOHMain_MIDemog2$EducationLevel, levels= c("Current K-12", "High School Diploma", "Current Undergraduate", "Associate's Degree", "Bachelor's Degree", "Master's Degree", "Doctoral Degree"))



### Using 'SOHMain_MIDemog2' as the final Dataset for this round ###
Train_2 <- filter(SOHMain_MIDemog2, FROM == "Train") %>% select(-FROM)
Test_2 <- filter(SOHMain_MIDemog2, FROM == "Test") %>% select(-FROM)





###### Datasets No. 3: Demographics and Questions have imputed values ######

remVars <- setdiff(colnames(SOHMain_MIDemo), c("FROM", "UserID", "YOB", "Happy"))
## IMPORTANT: This one takes a lot of time
# try3 <-kNN(as.matrix(SOHMain_MIDemo), dist_var = remVars)
# SOHMain_MIDemog3 <- try3[, 1:113]





###### Datasets No. 4: Demographics have imputed values and Questions have 'Skipped' ######

SOHMain_MIDemog4 <- SOHMain_MIDemog2
for(i in grep("^Q", colnames(SOHMain_MIDemog4))){
  
  SOHMain_MIDemog4[, i] <- ifelse(is.na(SOHMain_MIDemog4[,i]) == TRUE, "Skipped", SOHMain_MIDemog4[,i])
  SOHMain_MIDemog4[, i] <- as.factor(SOHMain_MIDemog4[, i])
}



SOHMain_MIDemog4 <- as.data.frame(SOHMain_MIDemog4)
SOHMain_MIDemog4$Age <- as.numeric(as.character(SOHMain_MIDemog4$Age))
SOHMain_MIDemog4$votes <- as.numeric(as.character(SOHMain_MIDemog4$votes))
SOHMain_MIDemog4$Income = factor(SOHMain_MIDemog4$Income, levels= c("under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000", "$100,001 - $150,000", "over $150,000"))
SOHMain_MIDemog4$HouseholdStatus = factor(SOHMain_MIDemog4$HouseholdStatus, levels = c("Domestic Partners (no kids)", "Domestic Partners (w/kids)", "Married (no kids)", "Married (w/kids)", "Single (no kids)", "Single (w/kids)"))
SOHMain_MIDemog4$EducationLevel = factor(SOHMain_MIDemog4$EducationLevel, levels= c("Current K-12", "High School Diploma", "Current Undergraduate", "Associate's Degree", "Bachelor's Degree", "Master's Degree", "Doctoral Degree"))

Train_4 <- filter(SOHMain_MIDemog4, FROM == "Train") %>% select(-FROM)
Test_4 <- filter(SOHMain_MIDemog4, FROM == "Test") %>% select(-FROM)






####### Filtering the Data ####

Train_2 <- filter(Train_2, Age > 13, Age < 81)
Train_4 <- filter(Train_4, Age > 13, Age < 81)






####### Missing Values ###########

MissVal <- data.frame(Variables = colnames(ShowofHands))
MissVal$NoNAs <- NA


for(i in 1:ncol(ShowofHands)){
  
  MissVal$NoNAs[i] <- sum(is.na(ShowofHands[, i]) == TRUE)
  
}


MissVal$PropNAs <- MissVal$NoNAs/nrow(ShowofHands)

View(MissVal)

FVI_DF <- data.frame(Variables = FinalVarImp)
FVI_DF$Variables <- as.character(FVI_DF$Variables)

FVI_NAs <- left_join(FVI_DF, MissVal, by = "Variables")








