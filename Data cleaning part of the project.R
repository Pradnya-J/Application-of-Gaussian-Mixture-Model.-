#importing the data
Data=read.csv("D:\\MSc(ASA)\\Sem1\\prOJECT\\Data Set\\credit_risk.csv")
summary(Data)
View(Data)

#changing the missing value rows with mean
library(dplyr)
which(is.na(Data))
mean.data=Data %>% group_by(Data$Intent) %>% summarise(Mean=mean(Data$Rate, na.rm = T)) 
Emp=Data %>% group_by(Home,Class) %>% summarise(Max=max(Data$Emp_length, na.rm=T),.groups = "keep")
#changing the na column with the mean
nrow(Data)
Data$Rate=ifelse(is.na(Data$Rate),11,Data$Rate)
Data$Emp_length=ifelse(is.na(Data$Emp_length)&(Data$Class==0),54,
                       ifelse(is.na(Data$Emp_length)&(Data$Class==1),46.25,Data$Emp_length))

#checking for Na values
which(is.na(Data))


#looking at the frequency of default and not default/good or bad customer 
table(Data$Default)

#Converting the target column default into Good or Bad
Data$Default=ifelse(Data$Default=="Y","Bad","Good")

#removing the character columns
Data=subset(Data,select=-c(Home,Intent))


#Scaling the income and amount column
Vec1=((Data$Amount)-min(Data$Amount))/(max(Data$Amount)-min(Data$Amount))
Vec2=((Data$Income)-min(Data$Income))/(max(Data$Income)-min(Data$Income))
Vec3=((Data$Percent_income)-min(Data$Percent_income))/(max(Data$Percent_income)-min(Data$Percent_income))
Data=cbind(Data,Vec1,Vec2,Vec3)
View(Data)
Data=Data[,-c(1,3,5,7,8)]
Data=Data[,c(1,2,3,5,6,7,8,4)]


#Changing the column names to scaled amount and scaled income
colnames(Data)[c(5,6,7)]=c("Amount","Income","Income_percent")
summary(Data)

#there is a problem with the age column we have to fix it
View(table(Data$Age))
which(Data$Age>100)
Data$Age[c(82,184,576,748,32298)]=c(44,44,23,23,44)
Data$Age=round(Data$Age,0)
Data$Emp_length=round(Data$Emp_length,0)

#our target column is default column and rest are features

#now we will split the dagta into train and test
x=sort(sample(nrow(Data),nrow(Data)*0.7))
Train_Data=Data[x,]
Test_Data=Data[-x,]

Data$Default
#we have to see the ratio of default to non default
library(imbalance)
nrow(subset(Data,Data$Default=="Good"))/nrow(Data)
imbalanceRatio(Data,classAttr = "Default")

#this means that we have to bring a balance ratio of 35-75 and therefore we have to oversample the data
Train_Data=oversample(Train_Data,ratio=0.35,method="SMOTE",classAttr = "Default")
imbalanceRatio(Train_Data,classAttr = "Default")
table(Train_Data$Default)

#now our data is fairly balanced and we can finally use the dataset for analysis

#exporting the clean dataset into csv and saving it
write.csv(Data,file="D:\\MSc(ASA)\\Sem1\\prOJECT\\Data Set\\credit_risk_clean.csv")
write.csv(Train_Data,file="D:\\MSc(ASA)\\Sem1\\prOJECT\\Data Set\\Train_data.csv")
write.csv(Test_Data,file="D:\\MSc(ASA)\\Sem1\\prOJECT\\Data Set\\Test_data.csv")

#Train data cleaning
Train_Data$Age=round(Train_Data$Age,0)
Train_Data$Emp_length=round(Train_Data$Emp_length,0)

View(Train_Data)
Train_Data$Age=round(Train_Data$Age,0)
Train_Data$Emp_length=round(Train_Data$Emp_length,0)
Train_Data$Emp_length=ifelse(Train_Data$Emp_length==1,mean(Train_Data$Emp_length),Train_Data$Emp_length)


