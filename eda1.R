setwd("C:/Users/HP/OneDrive/workkk")
print(summary(train))
#sm binning.eda(train$age, rounding = 3, pbar = 1)
library(ggplot2)
# frequency distributions and univariate charts
#ggplot(train, aes(age)) +geom_histogram(binwidth = ((max(train$age)-min(train$age))/10),fill="blue",color="black")+geom_freqpoly(bins=10)
ggplot(train, aes(age)) +geom_histogram(binwidth = ((max(train$age)-min(train$age))/10),fill="blue",color="black")
ggplot(train, aes(bmi),na.rm=TRUE) +geom_histogram(binwidth = ((max(train$bmi)-min(train$bmi))/10),fill="blue",color="black")
ggplot(train, aes(avg_glucose_level)) +geom_histogram(binwidth = ((max(train$avg_glucose_level)-min(train$avg_glucose_level))/10),fill="blue",color="black")
#categorical bars
ggplot(train, aes(x = gender)) + geom_bar(fill="blue" )
ggplot(train, aes(x = hypertension)) + geom_bar(fill="blue" )
ggplot(train, aes(x = heart_disease)) + geom_bar(fill="blue" )
ggplot(train, aes(x = ever_married)) + geom_bar(fill="blue" )
ggplot(train, aes(x = work_type)) + geom_bar(fill="blue" )
ggplot(train, aes(x = Residence_type)) + geom_bar(fill="blue" )
ggplot(train, aes(x = smoking_status)) + geom_bar(fill="blue" )
#hist(train$age)
#hist(train$bmi)
#hist(train$avg_glucose_level)
#make these prop tables better 
print (table(train$gender)/434)
print (table(train$smoking_status)/434)
print (table(train$Residence_type)/434)
print (table(train$hypertension)/434)
print (table(train$heart_disease)/434)
print (table(train$ever_married)/434)
print (table(train$work_type)/434)
print (table(train$stroke)/434)
##prop.table(table(train$gender))
####bivariate
# bivariate charts...event rate etc..
#bivariate  prop.tables
prop.table(table(train$gender,(train$stroke)))*100
prop.table(table(train$hypertension,(train$stroke)))*100
prop.table(table(train$heart_disease,(train$stroke)))*100
prop.table(table(train$ever_married,(train$stroke)))*100
prop.table(table(train$work_type,(train$stroke)))*100
prop.table(table(train$Residence_type,(train$stroke)))*100
prop.table(table(train$smoking_status,(train$stroke)))*100

#distribution tables 
prop.table(table(cut_number(train$age,n=10),train$stroke))
prop.table(table(cut_number(train$bmi,n=10),train$stroke))
prop.table(table(cut_number(train$avg_glucose_level,n=10),train$stroke))
# tables to data frames for charts
dfage <- data.frame(prop.table(table(cut_number(train$bmi,n=10),train$stroke)))
dfbmi <-data.frame(prop.table(table(cut_number(train$avg_glucose_level,n=10),train$stroke)))
dfavgl <-data.frame(prop.table(table(cut_number(train$bmi,n=10),train$stroke)))
# subsetting the non event rows
dfage<-dfage[-c(1,2,3,4,5,6,7,8,9,10),]
dfbmi<-dfbmi[-c(1,2,3,4,5,6,7,8,9,10),]
dfavgl<-dfavgl[-c(1,2,3,4,5,6,7,8,9,10),]
#plotting 

ggplot(dfage, aes(x=Var1, y=Freq*100)) +geom_bar(stat="identity",fill="blue")
ggplot(dfbmi, aes(x=Var1, y=Freq*100)) +geom_bar(stat="identity",fill="blue")
ggplot(dfavgl, aes(x=Var1, y=Freq*100)) +geom_bar(stat="identity",fill="blue")

# missing value
train$bmi[is.na(train$bmi)] = median(train$bmi, na.rm = TRUE)
sum(is.na(train$bmi))
print(sum(is.na(train$bmi)))
train$smoking_status[train$smoking_status==""] = "unknown"
#outliers and box plots
ggplot(train,aes(y=age))+geom_boxplot()
ggplot(train,aes(y=bmi))+geom_boxplot()
ggplot(train,aes(y=avg_glucose_level))+geom_boxplot()
#calculating ll an ul and iq

# correlations
numdf<- data.frame(train$age,train$bmi,train$avg_glucose_level)
co1<- cor(numdf)
print(co1)

#anova?
catdf <-data.frame(train$gender,train$hypertension,train$heart_disease,train$ever_married)
# chi?
ch1<- chisq.test(catdf)
print (ch1)


###WOE

library(Information)
train$gender<- factor(train$gender)
train$smoking_status<- factor(train$smoking_status)
train$ever_married<- factor(train$ever_married)
train$Residence_type<- factor(train$Residence_type)
train$work_type<- factor(train$work_type)

IV <- create_infotables(data=train, y="stroke", bins=10, parallel=FALSE)
print(IV)
summary(IV)
IV_Value = data.frame(IV$Summary)
print(IV$Tables$age, row.names=FALSE)
plot_infotables(IV, IV$Summary$Variable[1:9], same_scale=FALSE)
# encoding woe categorical 

