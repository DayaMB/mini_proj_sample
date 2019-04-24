TestBandData<-data.frame(Project_data)

#removing na values
TestBandData<-Project_data[complete.cases(Project_data),]

#add weekend variable
TestBandData$Weekend <- grepl("Sun.+",weekdays(TestBandData$Date))

#add total sleep Variable
TestBandData$Total_slp<-(TestBandData$Deep_sleep+TestBandData$Light_sleep)

#adding new col with class labels 

df$class=Sleep_type
head(df)
Sleep_type = c() 
for(i in 1:length(TestBandData$Deep_sleep))
{
  if(TestBandData$Deep_sleep[i]<=90)
  {
    Sleep_type = c(Sleep_type,"Not_enough_sleep")
  }
  else
  {
    Sleep_type = c(Sleep_type,"Good_sleep")
  }
}
#TestBandData=cbind(TestBandData,Sleep_type)

#tot_sleep
boxplot(TestBandData$Total_slp)
TestBandData$Total_slp[which(TestBandData$Total_slp>550)]
boxplot(TestBandData$Total_slp,plot=FALSE)$out
outliers<-boxplot(TestBandData$Total_slp,plot=FALSE)$out
print(outliers)
TestBandData[which(TestBandData$Total_slp %in% outliers),]
TestBandData1<-TestBandData[-which(TestBandData$Total_slp %in% outliers),]
boxplot(TestBandData1$Total_slp)

#Before and after rmvng outliers
boxplot(TestBandData$Total_slp)
boxplot(TestBandData1$Total_slp)
#Bivariate analysis
cor(TestBandData1$Steps,TestBandData1$Total_slp)
cor(TestBandData1$Calories_Burned,TestBandData1$Total_slp)



#male df
Test_male<-data.frame(TestBandData1[which(TestBandData1$Gender=='M'),])
Test_m_we<-data.frame(male[which(Test_male$Weekend=='TRUE'),]) # sun
Test_m_wd<-data.frame(male[which(Test_male$Weekend=='FALSE'),])# other days

#female df
Test_female<-data.frame(TestBandData1[which(TestBandData1$Gender=='F'),])
Test_f_we<-data.frame(female[which(female$Weekend=='TRUE'),])#sun
Test_f_wd<-data.frame(female[which(female$Weekend=='FALSE'),])#other days


#When you perform a hypothesis test in 
#statistics, a p-value helps you determine the significance of your results. ... 

#H0: mean total sleep on weekend = mean total sleep on weekdays (if p value is greater than 0.05 accept the null hypothesis)
t.test(Test_male$Total_slp~Test_male$Weekend)

t.test(Test_female$Total_slp~Test_female$Weekend)

#five num summary
datasummary1 <-data.frame(TestBandData1[c(2:5,10)])  
summary(datasummary1)

