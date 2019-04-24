stp<-TestBandData1$Steps
cl<-TestBandData1$Calories_Burned
tsp<-TestBandData1$Total_slp
wk<-TestBandData1$Weekend
sleept<-TestBandData1$Deep_sleep

sleept<-as.factor(TestBandData1$Deep_sleep)
sleept
#creating data frame for model
df<-data.frame(stp,cl,tsp,wk,sleept)
dim(df)

TestBandData1
df$class=Sleep_type
head(df)

Sleep_type<-cut(TestBandData1$Deep_sleep, breaks=2,labels=c("A","B"),right = FALSE)
Sleep_type
df$class=Sleep_type
head(df)

#creating sample for model
sam<-sample(2,nrow(df),replace=TRUE,prob=c(0.7,0.3))

#taining data set
train<-df[sam==1,]

#testing data set 
test<-df[sam==2,]

#loading the library
library(e1071)
library(caret)

#model construction using naive bayes
model<-naiveBayes(class~stp+cl+tsp+wk,data=train)
model

#predictive model for testing data set
pred<-predict(model,test)
pred


#confusion matrix for test
confusionMatrix(pred,test$class,dnn=list("Predicted","Actual"))

cm=table(test$class,pred)

#confusion matrix for train
#confusionMatrix(pred, train$sleept,dnn=list("Predicted","Actual"))

#confusion matrix for test
#confusionMatrix(pred,test$class,dnn=list("Predicted","Actual"))

TP=cm[1,1]
FN=cm[1,2]
FP=cm[2,1]
TN=cm[2,2]
accuracy=(TP+TN)/(TP+TN+FP+FN)
precision=TP/(TP+FP)
Recall= TP/(TP+FN)
print(accuracy*100)
print(precision*100)
print(Recall*100)