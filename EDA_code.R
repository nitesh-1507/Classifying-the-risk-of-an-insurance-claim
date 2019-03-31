#The guidelines to execute the code
#1) The codes are in order so execution should be line by line
#2) Skipping any line could hamper the execution of code
#3) Firstly libraries should be called
#4) Preprocessed dataset is to be imported next
#5) Post all this just the line by line execution will give the desired error rate


library(ISLR)
library(MASS)
mydata <-read.csv("C:\\Users\\gaura\\Desktop\\613\\TAMU ISEN 613\\ASSIGNMENT\\PROJECT\\submitted\\train_EDA_project.csv", na.strings = '?')
mydata<-na.omit(mydata)        


# changing some predictors to categorical variables.
mydata$ID <- as.factor(mydata$ID)
mydata$Household_ID <- as.factor(mydata$Household_ID)
mydata$Calendar_Year <- as.factor(mydata$Calendar_Year)
mydata$Model_Year <- as.factor(mydata$Model_Year)
mydata$OrdCat <- as.factor(mydata$OrdCat)
mydata$C_Claim <- as.factor(mydata$C_Claim)
attach(mydata)
str(mydata)


# Data split
library(caTools)
set.seed(1)
train= sample.split(1:99908, SplitRatio = 0.7)              # dividing into 70/30 ratio
data.train= subset(mydata,subset = train )                   # training data
data.test = subset(mydata, subset= !train)                   # test data



# QDA Model
qda_model <- qda( C_Claim ~ Cat3 + Cat6 + NVVar2 + NVVar3 + Var1 + Var2 + Var3 + Var4+ Var5 + Var6+ Var7 + Var8, data= data.train)
qda_pred <- predict(qda_model, data.test)
qda_class <- qda_pred$class
table( data.test$C_Claim, qda_class)
mean(qda_class== data.test$C_Claim)
qda_posterior_prob<-qda_pred$posterior


qda_pred_with_threshold<- rep(0,29973)
qda_pred_with_threshold[qda_posterior_prob[,2]> 0.012] = 1
table(Actual_Values= data.test$C_Claim, Predicted_Values= qda_pred_with_threshold)
mean(data.test$C_Claim!=qda_pred_with_threshold)

# checking on the test data 

test.final <- read.csv("C:\\Users\\gaura\\Desktop\\613\\TAMU ISEN 613\\ASSIGNMENT\\PROJECT\\submitted\\test_set_final.csv", na.strings = '?')


test.final$Calendar_Year <- as.factor(test.final$Calendar_Year)
test.final$Model_Year <- as.factor(test.final$Model_Year)
test.final$OrdCat <- as.factor(test.final$OrdCat)
test.final$C_Claim <- as.factor(test.final$C_Claim)

# qda_model <- qda( C_Claim ~ Cat3 + Cat6 + NVVar2 + NVVar3 + Var1 + Var2 + Var3 + Var4+ Var5 + Var6+ Var7 + Var8, data= data.train)   we already have this model 
qda_pred <- predict(qda_model, test.final)
qda_class <- qda_pred$class


qda_posterior_prob<-qda_pred$posterior
qda_pred_with_threshold<- rep(0,50000)
qda_pred_with_threshold[qda_posterior_prob[,2]> 0.012] = 1
table(Actual_Values= test.final$C_Claim, Predicted_Values= qda_pred_with_threshold)
mean(test.final$C_Claim==qda_pred_with_threshold)

# improved model

# since the criteria is only accuracy and not the true positive rate we will restore the threshold back to 0.5 which was made 0.012 to improve true positive rate.
qda_pred_with_threshold<- rep(0,50000)
qda_pred_with_threshold[qda_posterior_prob[,2]> 0.5] = 1
table(Actual_Values= test.final$C_Claim, Predicted_Values= qda_pred_with_threshold)
mean(test.final$C_Claim==qda_pred_with_threshold)

