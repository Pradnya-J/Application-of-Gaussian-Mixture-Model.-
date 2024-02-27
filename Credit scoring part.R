#importing the data
Credit_score=read.csv("D:\\MSc(ASA)\\Sem1\\prOJECT\\Data Set\\credit_risk.csv")
View(Cred_Score)

#changing the missing value rows with mean
library(dplyr)
mean.data=Credit_score %>% group_by(Credit_score$Intent) %>% summarise(Mean=mean(Credit_score$Rate, na.rm = T)) 
Emp=Credit_score %>% group_by(Home,Class) %>% summarise(Max=max(Credit_score$Emp_length, na.rm=T),.groups = "keep")
#changing the na column with the mean
nrow(Data)
Credit_score$Rate=ifelse(is.na(Credit_score$Rate),11,Credit_score$Rate)
Credit_score$Emp_length=ifelse(is.na(Credit_score$Emp_length)&(Credit_score$Class==0),54,
                       ifelse(is.na(Credit_score$Emp_length)&(Credit_score$Class==1),46.25,Credit_score$Emp_length))

summary(Credit_score$Cred_length)

#Row binding
Cred_Score=rbind(Creditscore1,Creditscore1.0,Creditscore1.1,Creditscore1.2,Creditscore1.3,Creditscore1.4,
      Creditscore2,Creditscore2.0,Creditscore2.1,Creditscore2.2,Creditscore2.3,Creditscore2.4,
      Creditscore3,Creditscore3.0,Creditscore3.1,Creditscore3.2,Creditscore3.3,Creditscore3.4)
Cred_Score1=subset(Cred_Score,select=c(Id,RecoveryR))

#Joining it with the main table
New_Cred=Credit_score

######Intent 1
Intent1=subset(New_Cred,New_Cred$Intent=="DEBTCONSOLIDATION")
Intent2=subset(New_Cred,New_Cred$Intent=="PERSONAL")
Intent3=subset(New_Cred,New_Cred$Intent=="VENTURE")
Intent4=subset(New_Cred,New_Cred$Intent=="EDUCATION")
Intent5=subset(New_Cred,New_Cred$Intent=="HOMEIMPROVEMENT")
Intent6=subset(New_Cred,New_Cred$Intent=="MEDICAL")

par(mfrow=c(2,3))

ABC=function(X){
  X_P=length(which(X$Default=="Y"))/nrow(X)
  X$SI=NA
  X$Expected_return=NA
  for(i in 1:nrow(X)){
    S.I=(X$Amount[i]*X$Rate[i]*X$Cred_length[i]/(12*100))
    X$SI[i]=(X$Amount[i]+S.I)
  }
  
  for(i in 1:nrow(X)){
    ExpRet=(X$SI[i]*X_P*0.8)+(X$SI[i]*(1-X_P))
    X$Expected_return[i]=ExpRet
  }
  X$Profit=X$Expected_return-X$Amount
  Profit6=sum(X$Profit)
  hist(X$Profit,main = paste("Histogram of",X$Intent[1]),xlab="Profit")
  return(X)
  
}
Intent1=ABC(Intent1)
Intent2=ABC(Intent2)
Intent3=ABC(Intent3)
Intent4=ABC(Intent4)
Intent5=ABC(Intent5)
Intent6=ABC(Intent6)
par(mfrow=c(1,1))

profitDf=data.frame(Category=c("DEBTCONSOLIDATION","PERSONAL","VENTURE","EDUCATION","HOMEIMPROVEMENT","MEDICAL"),Profits=c(sum(Intent1$Profit),sum(Intent2$Profit),sum(Intent3$Profit),sum(Intent4$Profit),sum(Intent5$Profit),sum(Intent6$Profit)))
Category=data.frame(Category=c("DEBTCONSOLIDATION"=859886.5,"PERSONAL"=1148443.9,"VENTURE"=1148443.9,"EDUCATION"=841113.4,"HOMEIMPROVEMENT"=867121.0,"MEDICAL"=1145454.6))
barplot(profitDf[,2],
        ylimit=c(0,10*10^8),
        col = c("red","blue","green","orange","violet","purple"),
        xlab = "Profit",
        ylab="Amount in Rs",
        names.arg = c("DEBTCONSOLIDATION","PERSONAL","VENTURE","EDUCATION","HOMEIMPROVEMENT","MEDICAL"),
        space=0.5)

sum(Intent6$Profit)-sum(Intent6$Profit)


Cor=cor(Train_Data[,1:7],method = "kendall")


library(corrplot)
corrplot(Cor)
