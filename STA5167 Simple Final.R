#Loading the Libraries
library(caTools)
library(pROC)
library(ROCR) 
library(car)
library(MASS)
library(survey)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

#Creating the Data Set
STA_5167_Heart_Disease_Data <- read_excel("C:/Users/Lucas Nolting/Downloads/STA 5167 Heart Disease Data.xlsx")
sex            <- as.factor(STA_5167_Heart_Disease_Data$sex)
cp             <- STA_5167_Heart_Disease_Data$cp
targetFactor   <- as.factor(STA_5167_Heart_Disease_Data$target)
target         <- STA_5167_Heart_Disease_Data$target
age            <- STA_5167_Heart_Disease_Data$age
trestbps       <- STA_5167_Heart_Disease_Data$trestbps

Heart_Data<-data.frame(target, targetFactor, sex, cp, age, trestbps)
#Modifying the "Chest Pain" Variable
Heart_Data$cp <- as.factor(ifelse(Heart_Data$cp >=1,1,0)) 

#Bar Chart of Categorical Variables
ggplot(data=Heart_Data, aes(x=cp))+
  geom_bar(aes(fill = targetFactor))+
  labs(title = "Distribution of Chest Pain \n and Heart Disease")+
  theme_bw()

ggplot(data=Heart_Data, aes(x=sex))+
  geom_bar(aes(fill = targetFactor))+
  labs(title = "Distribution of Sex \n and Heart Disease")+
  theme_bw()

#Plots of Numerical Variables
ggplot(data= Heart_Data,aes(x=age,y=target))+
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "quasibinomial"))+
  labs(title = "Heart Disease vs Age")+
  theme_bw()

ggplot(data= Heart_Data,aes(x=trestbps,y=target))+
  geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "binomial"))+
  labs(title = "Heart Disease vs \n Resting Blood Pressure")+
  theme_bw()

#Scatter Plot Matrix 
pairs(~target+age+trestbps, data = Heart_Data)

#
#   
# Below Modeling Training Begins
#
#

#Splitting into Testing and Training Data Sets
set.seed(12345)
smp_size <- floor(0.8 * nrow(Heart_Data))
train_ind <- sample(seq_len(nrow(Heart_Data)), size = smp_size)
train <- Heart_Data[train_ind, ]
test <- Heart_Data[-train_ind, ]

#Fitting the Logistic Regression Model 
model<-(glm(target~trestbps+age+sex+cp, 
            data = train, family = binomial))
summary(model)

#Model Selection
stepAIC(model, direction = "both")

#Model Selection
stepAIC(model, direction = "both")

#Testing for Outliers using Cook`s D
cooksD <- (cooks.distance(model))
Influence <- as.data.frame(cooksD)

# Plot Cook's Distance with a horizontal line at 4/n to see which observations
#exceed this threshold
n <- nrow(train)
ggplot(data = Influence, aes(x = seq(1,length(cooksD)), y = cooksD))+
  ggtitle("Plot of Cook`s D Values")+
  geom_point(col = "cyan", alpha = 0.75)+
  geom_hline(yintercept = 4/n, col = "black")+
  labs(x = "Observation",  y = "Cook`s D")+
  theme_bw()


#Removing Outliers
influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])
length(influential_obs)
outliers_removed <- train[-influential_obs, ]

#Refit the Model Without Outliers
model1<-(glm(target~trestbps+age+sex+cp, 
             data = outliers_removed, family = binomial))
summary(model1)

#Testing for Independence
Dev_Res <- resid(model, type = "deviance")
Independence <- as.data.frame(Dev_Res)
ggplot(data = Independence, aes(x = seq(1,length(Dev_Res)), y = Dev_Res))+
  ggtitle("Plot of Deviance Residuals")+
  geom_point(col = "navy", shape = "*", size = 4)+
  labs(x = "Observation",  y = "Deviance Residual")+
  theme_bw()

#Testing for Multicollinearity
car::vif(model, type = 'terms')

#Testing for Linearity with Continuous Variables
ContinuousModel <- glm(target~trestbps+age, 
                       data = train, family = binomial)
ContinuousProb <- predict(ContinuousModel, train, type = "response")
logit = log(ContinuousProb/(1-ContinuousProb))

boxTidwell(logit,train$trestbps)
boxTidwell(logit,train$age)

pairs(~logit+trestbps+age, data = train)



#
#
#Model Testing Begins
#
#

#Predicting the Probability of Having Heart Disease
TestProb <- predict(model, test, type = "response")

#ROC Curve
predicted.data <- data.frame(
  probability.of.hd=TestProb,
  target=test$targetFactor)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.hd, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

multiclass.roc(predicted.data$target, predicted.data$probability.of.hd, plot=TRUE, percent = TRUE,
               legacy.axes = TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage",
               print.auc = TRUE)

#Finding Optimal Cutoff
roc.info <- roc(predicted.data$target, predicted.data$probability.of.hd, legacy.axes=TRUE)

roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive percentage
  thresholds=roc.info$thresholds)

roc.df[roc.df$tpp > 75 & roc.df$tpp < 85,]

#Using Cutoff to Find Accuracy
predicted_classes <- ifelse(TestProb >0.5, 1, 0)

#Constructing a Confusion Matrix
ConfMatrix<-table(test$targetFactor, predicted_classes)
ConfMatrix

#Calculating Model Accuracy
missing_classerr <- mean(predicted_classes != test$target)
print(paste('Accuracy =', 1 - missing_classerr))

#Final Model Probability Graph
ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color = target), alpha=1, shape=4, stroke=2) +
  labs(title = "Patients Predicted Probability \n of Having Heart Disease", 
       x = "Index", y = "Predicted probability of having heart disease")+
  theme_bw()
  

#Distribution of Accuracy and True Positive Rate when Sample Size = 100
Accuracy <- (c(rep(0,1000)))
TruePositive <- (c(rep(0,1000)))
set.seed(12345)
for (i in 1:1000)
{
  Test_ind <- sample(seq_len(nrow(test)), size = 100)
  Test2 <-test[Test_ind,]
  TestProb2 <- predict(model, Test2, type = "response")
  predicted_classes2 <- ifelse(TestProb2 >0.5, 1, 0)
  ConfMatrix <- table(Test2$targetFactor, predicted_classes2)
  Accuracy[i] <- 1 - mean(predicted_classes2 != Test2$target)
  TruePositive[i] <- ConfMatrix[2,2]/(ConfMatrix[2,2]+ConfMatrix[2,1])
}
ggplot()+
  geom_histogram(aes(x=Accuracy), bins = 20, color = "black", fill = "lightblue")+
  labs(title = "Distribution of Model Accuracy")+
  theme_bw()
ggplot()+
  geom_histogram(aes(x=TruePositive), bins = 20, color = "black", fill = "lightblue")+
  labs(title = "Distribution of Model \n True Positive Rate")+
  theme_bw()








