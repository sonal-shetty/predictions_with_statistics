#Question 1
#Consider a relational dataset and specify your input and output variables
#For this task, I am going to use the Heart attack analysis and prediction dataset from Kaggle
#Reference link for the dataset : https://www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset
#Output variable is the variable 'output' which predicts whether a person is likely to suffer from a heart attack or not
#0 indicates less chance of heart attack
#1 indicates more chance of heart attack
#Input variables are 'age', 'sex', 'cp', 'trtbps', 'chol', 'fbs', 'restecg', 'thalachh', 'exng', 'oldpeak', 'slp', 'caa', 'thall'
#The target variable is categorical so I will be using Logistic regression for this task
library(readr)
library(MASS)
heart <- read_csv("Statistics CA 2/heart.csv")
head(heart)
n=nrow(heart);n

#Splitting the model into an 80-20 split to predict the target variable
indexes = sample(n,n*(80/100)) # the ratio of trainset is 80% and testset is 20%
trainset = heart[indexes,] #242 rows 
testset = heart[-indexes,]

#Since target variable is categorical, using binomial as the family for prediction
logr=glm(output~.,family='binomial', data=heart)

#Specify the significant variables on the output variable at the level of 𝛼=0.05
summary(logr)
pvalue = summary(logr)
pvalue=summary(logr)$coefficients[,4]; pvalue
round(pvalue,3)
#Based on the coefficients of the model, variables sex, cp,thalachh, exng, oldpeak, caa and thall have p.value less than 0.05
#Since p.value < alpha, H0 is rejected and these variables are considered significant to the target variable 'output'

#Estimating the parameters of the model
coef(logr)

#Predicting the output of the test dataset using the trained model
pred=predict(logr, testset, type='response')
final_pred=rep(0,length(pred))
final_pred[pred>0.5]=1 #probability of heart attack prediction being 1, if pred < 0.5 then output is 0
final_pred

#Functional form of the optimal predictive model
step.model <- stepAIC(logr, direction = "both", trace=FALSE)
summary(step.model)

pred1=predict(step.model, testset, type='response')
final_pred1 = rep(0,length(pred1))
final_pred1[pred1>0.5]=1
final_pred1

#Computing the confusion matrix
cm=table(testset$output, final_pred);cm

#Computing the accuracy of the model
acc=mean(testset$output==final_pred);acc

#Computing recall and precision for the model
re=cm[2,2]/(cm[2,1]+cm[2,2]); re

precision = cm[2,2]/(cm[1,2]+cm[2,2]); precision

#Final prediction from both full and reduced model are same although there is a slight difference in AIC with the reduced model and so the reduced model would be slightly optimal

#Question2
#For this assignment, I am using the ADANIPORTS stock market dataset from Kaggle for time-series prediction
D3 <- read_csv("Statistics CA 2/ADANIPORTS.csv")
#Using variable %Deliverable for time-series analysis, Following step converts the dataframe into an appropriate value for modelling
#The frequency at which the dataframe sample is taken is monthly so the frequency here is given as 30
X <- ts(D3[,15], start = c(2007,11), end = c(2021,04), frequency = 30)
plot(X)
abline(reg=lm(X~time(X)), col="blue")
#Looking at the plot, we can see that %Deliverable is not stationary in mean and variance
#using diff() and log() respectively to make the mean and variance stationary
Y = diff(log(X))
plot(Y)
abline(reg=lm(Y~time(Y)), col="blue") #Plot to see the difference
#b
acf(Y) 
#The order of q is 2 since 2 initial lags are outside of the bounds for MA
pacf(Y) 
#The order of p is 2, there is an exponential decay in the partial correlation
#d is 1 as we applied the diff once
library(forecast)
auto.fit<-auto.arima(X, seasonal=T)
auto.fit
#Value for AIC is -622.78 and ARIMA values suggested is ARIMA(1,1,1)
auto.fit<-auto.arima(X, seasonal=F)
auto.fit
#With Seasonal as F, value for AIC and model suggestion remains the same therefore we can use either of the models
#d
auto.fcast <- forecast(auto.fit, h=10)
plot(auto.fcast)
#We are able to forecast and plot the values for 10 steps ahead with the original time series