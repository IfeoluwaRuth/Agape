############# Importing libraries #########################
library(rpart)
library(rpart.plot)
library(tidyverse)
############## Data viewing ########################
data1 <- read.csv("~/Personal_PROJECT/data1.csv")
#View(data1)
names(data1) ##### Or use the command: {labels(data1)}
summary(data1$Age)
class(data1)
class(data1$Survived)
data1$Survived = as.factor(data1$Survived)
##########$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
class(data1$Pclass)
data1$Pclass = as.factor(data1$Pclass)
############^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
class(data1$Embarked)
data1$Embarked = as.factor(data1$Embarked)
levels(data1$Embarked)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4
class(data1$Sex)
data1$sex = as.factor(data1$Sex)
levels(data1$sex)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
class(data1$SibSp)
data1$SibSp = as.factor(data1$SibSp)
levels(data1$SibSp)
#$$%^&*(((((())))))%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data1$Parch = as.factor(data1$Parch)
levels(data1$Parch)
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4
###################################
########## Data cleaning #####################
anyNA(data1) # Checking for missing values#######
data1$Age = replace_na(data1$Age, 29)
is.na(data1)
data1 = data1 %>% select(!c(Name, Ticket)) ##### Removing variables Name and Ticket
head(data1)
#### Shuffling the dataset for proper randomness ####################
set.seed(1234)
S_data = sample(1:nrow(data1))
head(S_data)
############################$ Create train and test dataset #$$$$$$$$$$$$$$$$$$$$
Create_Train_Test = function(data1, size = 0.7, train = TRUE){
  no_row = nrow(data1)
  T_row = size * no_row
  Train_sample = 1: T_row
  if (train ==TRUE){
    return
    (data1[Train_sample, ])
  } else {
    return (data1[- Train_sample, ])
  }
}
################# CHECKING %%%%%%%%%%%%%%%%%%%
Train_Data = Create_Train_Test(data1, 0.7, train = TRUE)
Test_Data = Create_Train_Test(data1, 0.7, train = FALSE)
dim(Train_Data)
dim(Test_Data)
dim(data1)
prop.table(table(Train_Data$Survived))
prop.table(table(Test_Data$Survived))
###################### Building the Models #$$$$$$$$$$$$$$$$$$$$
Fit_model = rpart(Survived~.,data = Train_Data,
                  method = "class") # method can also be "anova" in case of regression tree;"poisson" and "exp" for survival analysis
rpart.plot(Fit_model, type = 2, extra = 106)
summary(Fit_model)
#################$ Making Predictions #$$$$$$$$$$$$
Predict_test = predict(Fit_model, Test_Data, type = "class") ## predicting the target class of the test dataset
Tab_matrix = table(Test_Data$Survived,Predict_test)
Tab_matrix ### creating a table that classified the target class.
#### This can also be called the the confusion matrix matrix which is used to measure the performance of the model. 
### Accuracy check-up
accuracy_test = sum (diag(Tab_matrix)/sum(Tab_matrix))
print(paste("Accuracy test for the model is:", accuracy_test))
