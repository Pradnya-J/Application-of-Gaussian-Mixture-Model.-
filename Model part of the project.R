set.seed(50)
library(mclust)
library(ggplot2)

K=seq(2,10,1)
AIC=numeric()
BIC=numeric()
dummy_data=Train_Data
View(dummy_data[,c(1:8)])

for(i in K){
  kmeans_model=Mclust(dummy_data[,1:7],centers=i)
  AIC=c(AIC,AIC(kmeans_model))
  BIC=c(BIC,BIC(kmeans_model))
}
unique(dummy_data)

df=data.frame(Components = K, AIC = AIC, BIC = BIC)
# Find the optimal number of components based on minimum AIC and BIC

optimal_components_aic=K[which.min(AIC)];optimal_components_aic
optimal_components_bic=K[which.min(BIC)];optimal_components_bic
View(df)



# making the Mclust 
Gmm_model=Mclust(dummy_data[,c(1:7)],2)
table(dummy_data$Default,Gmm_model$classification)
summary(Gmm_model)


?density

#to find the maximum of a row
Probability_Dataframe$Max=pmax(Probability_Dataframe$V1,Probability_Dataframe$V2)
View(Probability_Dataframe)
#making an empty column to get the max value column
Column_for_cluster=numeric()
Probability_Dataframe=Probability_Dataframe[,-4]
Column_for_cluster=ifelse(Probability_Dataframe$V1==Probability_Dataframe$Max,append(Column_for_cluster,1),
                          ifelse(Probability_Dataframe$V2==Probability_Dataframe$Max,append(Column_for_cluster,2),0))


#Creating a dataframe column for it and binding it with the original dataframe
data1=data.frame(Column1=Column_for_cluster)
Probability_Dataframe=cbind(Probability_Dataframe,data1$Column1)
View(Probability_Dataframe)
View(dummy_data)
#adding the cluster column to dummy data column
dummy_data=cbind(dummy_data,data1$Column1)
colnames(dummy_data)[10]="Cluster Number"

#Dividing the dataframe according to the clusters
C_1=subset(dummy_data,dummy_data$`Cluster Number`==1)
C_2=subset(dummy_data,dummy_data$`Cluster Number`==2)
View(C_1)
#Looking at the default and non-default
table(C_1$Default)
table(C_2$Default)

#Calculating probability
Probability_good=function(x,y){
  Probability=x/y
}
P1=Probability_good(length(which(C_1$Default=="Good")),nrow(C_1));P1
P2=Probability_good(length(which(C_2$Default=="Good")),nrow(C_2));P2

#calculating Ratio
R1=length(which(C_1$Default=="Good"))/length(which(C_1$Default=="Bad"))
R2=length(which(C_2$Default=="Good"))/length(which(C_2$Default=="Bad"))

#Calculating Weight
W1=nrow(C_1)/nrow(dummy_data)
W2=nrow(C_2)/nrow(dummy_data)
sum(W1,W2)

#to check the test data
Dummy_test=Test_Data
predictions=predict.Mclust(Gmm_model,newdata = Test_Data[,1:7]);
predictions$classification
Test_Dataframe=data.frame(predictions$z)
View(Test_Dataframe)
predictions$z

Test_Dataframe$Max=pmax(Test_Dataframe$X1,Test_Dataframe$X2)

#making an empty column to get the max value column
Column_for_cluster=numeric()

Column_for_cluster=ifelse(Test_Dataframe$X1==Test_Dataframe$Max,append(Column_for_cluster,1),
                          ifelse(Test_Dataframe$X2==Test_Dataframe$Max,append(Column_for_cluster,2),0))

#Creating a dataframe column for it and binding it with the original dataframe
data1.0=data.frame(Column1=Column_for_cluster)
Test_Dataframe=cbind(Test_Dataframe,data1.0$Column1)

#adding the cluster column to dummy data column
Dummy_test=cbind(Dummy_test,data1.0$Column1)
colnames(Dummy_test)[10]="Cluster Number"
Dummy_test=Dummy_test[,-10]
View(Dummy_test)

#Dividing the dataframe according to the clusters
T_1=subset(Dummy_test,Dummy_test$`Cluster Number`==1)
T_2=subset(Dummy_test,Dummy_test$`Cluster Number`==2)
View(T_2)

#Looking at the default and non-default
table(T_1$Default)
table(T_2$Default)

PT1=Probability_good(length(which(T_1$Default=="Good")),nrow(T_1))
PT2=Probability_good(length(which(T_2$Default=="Good")),nrow(T_2))

#calculating Ratio
RT1=length(which(T_1$Default=="Good"))/length(which(T_1$Default=="Bad"))
RT2=length(which(T_2$Default=="Good"))/length(which(T_2$Default=="Bad"))

#Calculating Weight
WT1=nrow(T_1)/nrow(Dummy_test)
WT2=nrow(T_2)/nrow(Dummy_test)
sum(W1,W2)

t1=as.data.frame(table(C_1$Default))
t2=as.data.frame(table(C_2$Default))

#calculating Probabilities based on test dataset
PT1=Probability_good(length(which(T_1$Default=="Good")),nrow(T_1))
PT2=Probability_good(length(which(T_2$Default=="Good")),nrow(T_2))

#calculating Ratio
RT1=length(which(T_1$Default=="Good"))/length(which(T_1$Default=="Bad"))
RT2=length(which(T_2$Default=="Good"))/length(which(T_2$Default=="Bad"))

#Calculating Weight
WT1=nrow(T_1)/nrow(Dummy_test)
WT2=nrow(T_2)/nrow(Dummy_test)
sum(W1,W2)

t1=as.data.frame(table(C_1$Default))
t2=as.data.frame(table(C_2$Default))

Cluster1_PD=data.frame(Category=c("Good","Bad","Good","Bad","Good","Bad","Good","Bad"),Probability_of_default=c(P1,1-P1,P2,1-P2,PT1,1-PT1,PT1,1-PT2))

library(dplyr)
Master_table=cbind(t1,t2$Freq,t3$Freq,t4$Freq);Master_table



ggplot(Master_table,aes(x=factor(Var1),y=Freq))+
  geom_col(color='black',fill='cyan3')+
  xlab('Response')


C_1D=subset(C_1,C_1$Default=="Bad")
C_2D=subset(C_2,C_2$Default=="Bad")
View(C_1D)
T_1D=subset(T_1,T_1$Default=="Bad")
T_2D=subset(T_2,T_2$Default=="Bad")


Eloss=function(P1,Amount){
  loss=(1-P1)*Amount*(1-0.5)
  return(loss)
}

#for Train
C_1D$ElossR=NA
C_2D$ElossR=NA

set.seed(1)
for(i in 1:nrow(C_1D)){
  C_1D$ElossR[i]=Eloss(P1,rnorm(1,1000,100))
}
set.seed(1)
for(i in 1:nrow(C_2D)){
  C_2D$ElossR[i]=Eloss(P2,rnorm(1,1000,100))
}
e1=sum(C_1D$ElossR)
e2=sum(C_2D$ElossR)
Total_exploss=sum(C_1D$ElossR)+sum(C_2D$ElossR)
Total_earning=nrow(C_1)*1000-Total_exploss
View(C_2D)

#For test
T_1D$ExplossR=NA
T_2D$ExplossR=NA

View(T_1D)
set.seed(1)
for(i in 1:nrow(T_1D)){
  T_1D$ExplossR[i]=Eloss(PT1,rnorm(1,1000,100))
}
set.seed(1)
for(i in 1:nrow(T_2D)){
  T_2D$ExplossR[i]=Eloss(PT2,rnorm(1,1000,100))
}

et1=sum(T_1D$ElossR)
et2=sum(T_2D$ElossR)

View(T_1D)

##Random forest for factors
# Loading package 
library(caTools) 
library(randomForest) 

# Fitting Random Forest to the train dataset 
View(Train_Data)
set.seed(120)  # Setting seed 
trainmodified=Train_Data

trainmodified$Default=ifelse(trainmodified$Default=="Good",0,1)
classifier_RF = randomForest(x = trainmodified[-8], 
                             y = trainmodified$Default, 
                             ntree = 1000) 

varImpPlot(classifier_RF) 

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = Train_Data[-10]) 


?predict()
classifier_RF = randomForest(factor(Default~.),Train_Data) 
classifier_RF$mtry


#Visualization Part
#plotting the AIC and BIC Values for optimum number of clusters
ggplot(df,  aes(x = Components)) +
  geom_line(aes(y = AIC, color = "AIC")) +
  geom_line(aes(y = BIC, color = "BIC")) +
  labs(title = "AIC and BIC for Different Numbers of Components",
       x = "Number of Components",
       y = "AIC / BIC") +
  scale_color_manual(values = c("AIC" = "blue", "BIC" = "red")) +
  theme_minimal()

par(mfrow=c(1,2))
barplot(Cluster1_PD[1:2,2],space=0.5,width=1,xlab="Train Cluster 1",ylab="probability",names.arg =c("Good","Bad"),col =c("green","red") )
barplot(Cluster1_PD[3:4,2],space=0.5,xlab="Train Cluster 2",ylab="probability",names.arg =c("Good","Bad"),col =c("green","red"))
barplot(Cluster1_PD[5:6,2],space=0.5,width=0.1,xlab="Test Cluster 1",ylab="probability",names.arg =c("Good","Bad"),col =c("green","red"))
barplot(Cluster1_PD[7:8,2],space=0.5,width=0.1,xlab="Test Cluster 2",ylab="probability",names.arg =c("Good","Bad"),col =c("green","red"))


#Expected Loss from main data and model data
probtrain=subset(Train_Data,Train_Data$Default=="Bad")
probtest=subset(Test_Data,Test_Data$Default=="Bad")

View(probtrain)
View(probtest)
prob=nrow(probtrain)/nrow(Train_Data)
prob1=nrow(probtest)/nrow(Test_Data)

set.seed(1)
ExplossTrain1=Eloss(1-prob,rnorm(1,1000,100))*nrow(probtrain)
set.seed(1)
ExplossTest1=Eloss(1-prob1,rnorm(1,1000,100))*nrow(probtest)

e1=sum(C_1D$ElossR)+sum(C_2D$ElossR)
et1=sum(T_1D$ExplossR)+sum(T_2D$ExplossR)

T_1D$ExplossR=NA
T_2D$ExplossR=NA

View(T_1D)
set.seed(1)
for(i in 1:nrow(T_1D)){
  T_1D$ExplossR[i]=Eloss(PT1,rnorm(1,1000,100))
}
set.seed(1)
for(i in 1:nrow(T_2D)){
  T_2D$ExplossR[i]=Eloss(PT2,rnorm(1,1000,100))
}


set.seed(1)
sum=NA
for(i in 1:nrow(T_2)){
  a=rnorm(1,1000,100)
  sum[i]=a
}

nrow(Train_Data)

sum(sum)
Loss_Dataframe=data.frame(Exp_lossTrain=e1,ExplossTest=et1,ActualLossTrain=ExplossTrain1,ActualLossTest=ExplossTest1,ErrorPercTrain=(e1-ExplossTrain1)/(e1)*100,ErrorPercTest=(et1-ExplossTest1)/(et1)*100)


#Model Accuracy
install.packages("caret")
library("caret")

Train_Data$Default_Probability=NA

for(i in 1:nrow(Train_Data)){
  Train_Data$Default_Probability[i]=sum(Gmm_model$z[i,]*c(1-P1,1-P2))
}

Test_Data=Test_Data[,-10]

for(i in 1:nrow(Test_Data)){
  Test_Data$DefaultProbability[i]=sum(GMM_test$z[i,]*c(1-PT1,1-PT2))
}

confusionMatrix(as.factor(ifelse(predictions$classification==1,'Good','Bad')),as.factor(Test_Data$Default))
ifelse(predictions$classification==1,"Good","Bad")
table(ifelse(predictions$classification==1,"Good","Bad"))
table(Test_Data$Default)
