#Prediction of quality of wine

#The quality of wine depends on on a no. of factor such as Taste, Fixed 
#acidity, Volatile acidity, pH etc

#Given to you a dataset containing 50,000 samples of wine from a winery.
#Draw a prediction of quality of a sample based upon the given info.

#Load the data
wine<-read.csv("C:\\Users\\User\\Documents\\Data Science\\R for data science\\WineQT.csv")

#View the data
View(wine)

#No . of row & columns in the dataset
nrow(wine)
ncol(wine)

#First 6 rows of the data
wine_first6<-head(wine)

#View first 6 rows of the data
View(wine_first6)

#Lasst 6 rows of the data
wine_last6<-tail(wine)

#View last 6 rows of the data
View(wine_last6)

#Extract quality column from the data
Quality<-wine$quality

#Observe quality for 3
Quality[Quality==3]
which(Quality==3)

#Viewing the freq of Quality
table(Quality)  

#Viewing the Quality through bar diagram
barplot(table(Quality))  

#We set the quality is Good for > 5 , Bad for < 5 , Average for = 5

#Replace the Quality as "good" for > 5 
wine$taste<-replace(Quality,Quality>5,"Good") 

#Replace the Quality as "bad" for > 5 
wine$taste<-replace(wine$taste,wine$taste<5,"Bad")

#Replace the Quality as "average" for > 5 
wine$taste<-replace(wine$taste,wine$taste==5,"Average")
wine$taste<-as.factor(wine$taste)

#Frequency
table(wine$taste)

#Perform the model
set.seed(1)  #For a fixed random sample
samp<-sample(nrow(wine),0.8*nrow(wine))
samp
train<-wine[samp,]  #wine based on sample
train
test<-wine[-samp,]
test

#Necessary library
install.packages("randomForest")
library(randomForest)

#Fit the data
model<-randomForest(taste ~.- quality,data=train)  
model

#Predict the data
Predicted_taste<-predict(model,newdata=test)  

#Observe data
Observed_taste<-test$taste

#Frequency
table(Predicted_taste,Observed_taste)
table(Predicted_taste)
table(Observed_taste)


#Check accuracy of the model
Accuracy_model<-mean(Predicted_taste==Observed_taste)
Accuracy_model


#Here accuracy of the model is 0.7510917.So we can say that the model is
#appropriate.














x<-1:5
y<-c("T","F")
y<-sample(y,5,replace=T)
y
data<-data.frame(No.=x,response=y)
data
nrow(data)
sample(nrow(data),0.7*nrow(data))






