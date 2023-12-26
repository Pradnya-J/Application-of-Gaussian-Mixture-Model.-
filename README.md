# Application-of-Gaussian-Mixture-Model.-
In the modern times, statistics has become an essential field of study. While looking at the applications of statistics in the field of finance, this project brings an insightful analysis on a given banking data set to forecast the probability of default using Gaussian Mixture Model.

1 Introduction
In the modern times, statistics has become an essential field of study. The diverse application of statistics in field of biology, physics, commerce which in-
cludes business, banking institutions and many more has been very essential for the constant development of these fields. While looking at the applications of
statistics in the field of finance, this project brings an insightful analysis on a given banking data-set to forecast the probability of default using Gaussian Mixture Model. Further using these probabilities we have also calculated expected loss and profit.

2 Problem Statement

Given a particular data-set, calculate the probability of loan default for individual customer using Gaussian Mixture Model and calculate the expected loss and gain by using those probabilities. Finding out the Factor importance and comparing our model with Support Vector Mechine

3 Theory
The Gaussian Mixture Model (GMM) stands as a fundamental and versatile statistical technique with a widespread application in the domain of finance. Its adaptability extends to various financial tasks, including risk assessment, fraud detection, credit scoring, and market risk management. Within the context of speaker recognition, GMMs play a pivotal role during training, generating prototypes to represent the feature space as a mixture of Gaussian distributions
[4]. GMMs, renowned for their utility in clustering applications, provide a probabilistic association between data samples and clusters, unlike the binary classification approach of k-means clustering [1]. In the realm of consumer loan management, GMMs serve as a powerful tool for estimating the probability of default for individual customers. These models group customers into clusters based on feature similarities, enabling the assessment of bankruptcy probabilities within each cluster. This approach, outlined by Hamidreza Arian in ”Forecasting Probability of Default for Consumer Loan Management with Gaussian Mixture Models” (2020), relies on GMMs' ability to discern the ratio of good and bad customers within each cluster [1]. Notably, GMMs employ the Expectation-Maximization (EM) algorithm to determine both the optimal number of clusters and the parameters of each cluster [2]. Expanding beyond finance, GMMs have also found applicability in stock market prediction and data generation. These models offer distinct advantages, particularly in modeling stock returns. With a sufficient number of compo-
nents and careful fitting to training data using the Expectation–Maximization algorithm, GMMs can project future outcomes under various scenarios. Importantly, GMMs do not impose normality assumptions on return distributions when the number of components exceeds two [3].
    In various domains, especially those involving data from multiple populations, Gaussian mixtures have been extensively employed as models. Speaker recognition systems, a prominent example, often utilize GMMs due to their capacity to represent diverse feature distributions. GMMs excel in shaping smooth estimations for arbitrarily shaped densities, with each component density potentially representing a distinct hidden class. This adaptability and ability to represent feature distributions have solidified GMMs’ role in biometric systems, as detailed in Singh and colleagues’ work on ”Gaussian Mixture Model: A Mod-
eling Technique for Speaker Recognition and its Component” [4].
    Moreover, GMMs have demonstrated their utility in modeling wind parameters. The GMM’s flexibility shines as it accurately estimates the 50-year wind
parameter contour and aligns well with established standards like IEC 61400-1. This flexibility extends across various sectors of wind measurement data, showing promise in modeling the joint distribution of wind parameters. Notably, GMMs exhibit lower estimation error for marginal and conditional distributions
compared to copula methods, as highlighted in Zhang and colleagues’ research on ”Gaussian mixture model for extreme wind turbulence estimation” [5].

    In summary, Gaussian Mixture Models have proven to be versatile and powerful tools in various fields, including finance, speaker recognition, stock market prediction, and wind parameter modeling. Their ability to adapt to complex data distributions, estimate probabilities, and model diverse populations makes them a valuable asset in a wide range of applications. These models, often parameterized using the Expectation-Maximization algorithm, have significantly contributed to the advancement of research and practical solutions in these domains.

## Code ##
Data Cleaning
#im p o r ti n g the data
Data=re ad . c s v ( ”D: \ \MSc(ASA)\ \Sem1\\prOJECT\\Data Se t \\ c r e d i t r i s k . c s v ” )
summary ( Data )
View ( Data )
#ch an gin g the mi s si n g v al u e rows with mean
l i b r a r y ( d pl y r )
which ( i s . na ( Data ) )
mean . data=Data %>% group by ( D a t a$In ten t ) %>%
summarise (Mean=mean ( Data$Rate , na . rm = T) )
Emp=Data %>%
group by (Home, Cl a s s ) %>%
summarise (Max=max( Data$Emp length , na . rm=T ) , . g r oup s = ” keep ” )
#ch an gin g the na column with the mean
nrow ( Data )
Data$Rate=i f e l s e ( i s . na ( Data$Rate ) , 1 1 , Data$Rate )
Data$Emp length=i f e l s e ( i s . na ( Data$Emp length )&( Data$Class ==0) ,54 ,

i f e l s e ( i s . na ( Data$Emp length )
&(Data$Class ==1) ,46.25 , Data$Emp length ) )

#c h e c ki n g f o r Na v al u e s
which ( i s . na ( Data ) )
#l o o ki n g a t the f r e q u e n c y o f d e f a u l t and not d e f a u l t / good o r
#bad customer
t a b l e ( D a t a$De f aul t )
#C onve r tin g the t a r g e t column d e f a u l t i n t o Good o r Bad
D a t a$De f aul t=i f e l s e ( D a t a$De f aul t==”Y” , ”Bad ” , ”Good ” )
#removing the c h a r a c t e r columns
Data=s u b s e t ( Data , s e l e c t=−c (Home, I n t e n t ) )
#S c ali n g the income and amount column
Vec1=((Data$Amount)−min ( Data$Amount ) ) / (max( Data$Amount)−min ( Data$Amount ) )
Vec2=(( Data$Income)−min ( Data$Income ) ) / (max( Data$Income)−min ( Data$Income ) )
Vec3=(( D a ta$Pe rcen t income )−min ( Da ta$Pe rcen t income ) ) /
(max( Da ta$Pe rcen t income )−min ( Da ta$Pe rcen t income ) )
Data=cbind ( Data , Vec1 , Vec2 , Vec3 )
10

View ( Data )
Data=Data[ , − c ( 1 , 3 , 5 , 7 , 8 ) ]
Data=Data [ , c ( 1 , 2 , 3 , 5 , 6 , 7 , 8 , 4 ) ]
#Changing the column names t o s c a l e d amount and s c a l e d income
colnames ( Data ) [ c ( 5 , 6 , 7 )]= c ( ”Amount ” , ” Income ” , ” I n c om e p e r c e n t ” )
summary ( Data )
#t h e r e i s a problem with the age column we have t o f i x i t
View ( t a b l e ( Data$Age ) )
which ( Data$Age>100)
Data$Age [ c ( 8 2 , 1 8 4 , 5 7 6 , 7 4 8 , 3 2 2 9 8 )]= c ( 4 4 , 4 4 , 2 3 , 2 3 , 4 4 )
Data$Age=round ( Data$Age , 0 )
Data$Emp length=round ( Data$Emp length , 0 )
#our t a r g e t column i s d e f a u l t column and r e s t a r e f e a t u r e s
#now we w i l l s p l i t the dagta i n t o t r a i n and t e s t
x=s o r t ( sample ( nrow ( Data ) , nrow ( Data ) ∗ 0 . 7 ) )
Train Data=Data [ x , ]
Test Data=Data[−x , ]
#we have t o s e e the r a t i o o f d e f a u l t t o non d e f a u l t
l i b r a r y ( imb al ance )
nrow ( s u b s e t ( Data , D a t a$De f aul t==”Good ” ) ) / nrow ( Data )
imb al anceR a ti o ( Data , c l a s s A t t r = ” D e f a ul t ” )
#t h i s means t h a t we have t o b ri n g a
#b al a n c e r a t i o o f 35−75 and t h e r e f o r e we have t o ove r s ample the data
Train Data=ove r s ample ( Train Data , r a t i o =0.35 , method=”SMOTE” , c l a s s A t t r = ” D e f a ul t ” )
imb al anceR a ti o ( Train Data , c l a s s A t t r = ” D e f a ul t ” )
t a b l e ( T r ain D a t a$D e f a ul t )
#now our data i s f a i r l y b al anced and we can f i n a l l y u se
#Train data cl e a ni n g
Train Data$Age=round ( Train Data$Age , 0 )
Train Data$Emp length=round ( Train Data$Emp length , 0 )
View ( Train Data )
Train Data$Age=round ( Train Data$Age , 0 )
Train Data$Emp length=round ( Train Data$Emp length , 0 )
Train Data$Emp length=i f e l s e ( Train Data$Emp length==1,
mean ( Train Data$Emp length ) , Train Data$Emp length )
Model and Expected loss and Profits

11

s e t . s e e d ( 5 0 )
l i b r a r y ( mclu s t )
l i b r a r y ( g g pl o t 2 )
K=se q ( 2 , 1 0 , 1 )
AIC=numeric ( )
BIC=numeric ( )
dummy data=Train Data
View ( dummy data [ , c ( 1 : 8 ) ] )
f o r ( i i n K){
kmeans model=Mclust ( dummy data [ , 1 : 7 ] , c e n t e r s=i )
AIC=c (AIC , AIC ( kmeans model ) )
BIC=c (BIC , BIC ( kmeans model ) )
}
unique ( dummy data )
d f=data . frame ( Components = K, AIC = AIC , BIC = BIC )
# Find the op tim al number o f components based on minimum AIC and BIC
o p tim al c om p o n e n t s ai c=K[ which . min (AIC ) ] ; o p tim al c om p o n e n t s ai c
o p tim al c om p o n e n t s bi c=K[ which . min (BIC ) ] ; o p tim al c om p o n e n t s bi c
View ( d f )

# making the Mclust
Gmm model=Mclust ( dummy data [ , c ( 1 : 7 ) ] , 2 )
t a b l e ( dummy data$Default , Gmm m o d el$cl a s si fi c a ti o n )
summary (Gmm model )
? d e n si t y
#t o f i n d the maximum o f a row
P r ob abili ty D a t a f r ame$M ax=pmax ( P r ob abili ty D a t a f r ame$V 1 ,
P r ob abili t y D a t a f r ame$V 2 )

View ( P r o b a bili t y D a t a f r am e )
#making an empty column t o g e t the max v al u e column
C ol um n f o r cl u s t e r=numeric ( )
P r o b a bili t y D a t a f r am e=P r o b a bili t y D a t a f r am e [ , −4]
C ol um n f o r cl u s t e r=
i f e l s e ( P r ob abili t y D a t a f r ame$V 1==P robabili ty Da ta f rame$Max ,
append ( C ol um n f o r cl u s t e r , 1 ) ,
i f e l s e

( P r ob abili t y D a t a f r ame$V 2==P robabili ty Da ta f rame$Max ,

12

append ( C ol um n f o r cl u s t e r , 2 ) , 0 ) )

#C r e a ti n g a da ta f rame column f o r i t and bi n di n g i t with the o r i g i n a l da ta f rame
data1=data . frame ( Column1=C ol um n f o r cl u s t e r )
P r o b a bili t y D a t a f r am e=cbind ( P r o b a bili t y D a t a f r am e , data1$Column1 )
View ( P r o b a bili t y D a t a f r am e )
View ( dummy data )
#adding the c l u s t e r column t o dummy data column
dummy data=cbind ( dummy data , data1$Column1 )
colnames ( dummy data )[ 1 0]= ” Cl u s t e r Number”
#Di vi di n g the da ta f rame a c c o r di n g t o the c l u s t e r s
C 1=s u b s e t ( dummy data , dummy data$ ‘ Cl u s t e r Number‘==1)
C 2=s u b s e t ( dummy data , dummy data$ ‘ Cl u s t e r Number‘==2)
View ( C 1 )
#Looking a t the d e f a u l t and non−d e f a u l t
t a b l e ( C 1$De f aul t )
t a b l e ( C 2$De f aul t )
#C al c ul a ti n g p r o b a b i l i t y
P r o b a bili t y g o o d=f u n c ti o n ( x , y ){
P r o b a b i l i t y=x/y
}
P1=P r o b a bili t y g o o d ( l e n g t h ( which ( C 1$De f aul t==”Good ” ) ) , nrow ( C 1 ) ) ; P1
P2=P r o b a bili t y g o o d ( l e n g t h ( which ( C 2$De f aul t==”Good ” ) ) , nrow ( C 2 ) ) ; P2
#c a l c u l a t i n g Ra ti o
R1=l e n g t h ( which ( C 1$De f aul t==”Good ” ) ) / l e n g t h ( which ( C 1$De f aul t==”Bad ” ) )
R2=l e n g t h ( which ( C 2$De f aul t==”Good ” ) ) / l e n g t h ( which ( C 2$De f aul t==”Bad ” ) )
#C al c ul a ti n g Weight
W1=nrow ( C 1 ) / nrow ( dummy data )
W2=nrow ( C 2 ) / nrow ( dummy data )
sum (W1,W2)
#t o check the t e s t data
Dummy test=Test Data
p r e d i c t i o n s=p r e d i c t . Mclust (Gmm model , newdata = Test Data [ , 1 : 7 ] ) ;
p r e d i c t i o n s $ c l a s s i f i c a t i o n
Tes t Da ta frame=data . frame ( p r e d i c t i o n s $ z )
View ( Tes t Da ta frame )
p r e d i c t i o n s $ z
Test Dataframe$Max=pmax ( Test Data frame$X1 , Test Data frame$X2 )

13

#making an empty column t o g e t the max v al u e column
C ol um n f o r cl u s t e r=numeric ( )
C ol um n f o r cl u s t e r=i f e l s e ( Test Data frame$X1==Test Dataframe$Max ,

append ( C ol um n f o r cl u s t e r , 1 ) ,
i f e l s e ( Test Data frame$X2==Test Dataframe$Max ,
append ( C ol um n f o r cl u s t e r , 2 ) , 0 ) )

#C r e a ti n g a da ta f rame column f o r i t and bi n di n g i t with the o r i g i n a l da ta f rame
data1 .0= data . frame ( Column1=C ol um n f o r cl u s t e r )
Tes t Da ta frame=cbind ( Test Data frame , data1 . 0 $Column1 )
#adding the c l u s t e r column t o dummy data column
Dummy test=cbind ( Dummy test , data1 . 0 $Column1 )
colnames ( Dummy test )[ 1 0]= ” Cl u s t e r Number”
Dummy test=Dummy test [ , −1 0]
View ( Dummy test )
#Di vi di n g the da ta f rame a c c o r di n g t o the c l u s t e r s
T 1=s u b s e t ( Dummy test , Dummy test$ ‘ Cl u s t e r Number‘==1)
T 2=s u b s e t ( Dummy test , Dummy test$ ‘ Cl u s t e r Number‘==2)
View ( T 2 )
#Looking a t the d e f a u l t and non−d e f a u l t
t a b l e ( T 1$De f aul t )
t a b l e ( T 2$De f aul t )
PT1=P r o b a bili t y g o o d ( l e n g t h ( which ( T 1$De f aul t==”Good ” ) ) , nrow ( T 1 ) )
PT2=P r o b a bili t y g o o d ( l e n g t h ( which ( T 2$De f aul t==”Good ” ) ) , nrow ( T 2 ) )
#c a l c u l a t i n g Ra ti o
RT1=l e n g t h ( which ( T 1$De f aul t==”Good ” ) ) / l e n g t h ( which ( T 1$De f aul t==”Bad ” ) )
RT2=l e n g t h ( which ( T 2$De f aul t==”Good ” ) ) / l e n g t h ( which ( T 2$De f aul t==”Bad ” ) )
#C al c ul a ti n g Weight
WT1=nrow ( T 1 ) / nrow ( Dummy test )
WT2=nrow ( T 2 ) / nrow ( Dummy test )
sum (W1,W2)
t 1=a s . data . frame ( t a b l e ( C 1$De f aul t ) )
t 2=a s . data . frame ( t a b l e ( C 2$De f aul t ) )
#c a l c u l a t i n g P r o b a b i l i t i e s based on t e s t d a t a s e t
PT1=P r o b a bili t y g o o d ( l e n g t h ( which ( T 1$De f aul t==”Good ” ) ) , nrow ( T 1 ) )
PT2=P r o b a bili t y g o o d ( l e n g t h ( which ( T 2$De f aul t==”Good ” ) ) , nrow ( T 2 ) )

14

#c a l c u l a t i n g Ra ti o
RT1=l e n g t h ( which ( T 1$De f aul t==”Good ” ) ) / l e n g t h ( which ( T 1$De f aul t==”Bad ” ) )
RT2=l e n g t h ( which ( T 2$De f aul t==”Good ” ) ) / l e n g t h ( which ( T 2$De f aul t==”Bad ” ) )
#C al c ul a ti n g Weight
WT1=nrow ( T 1 ) / nrow ( Dummy test )
WT2=nrow ( T 2 ) / nrow ( Dummy test )
sum (W1,W2)
t 1=a s . data . frame ( t a b l e ( C 1$De f aul t ) )
t 2=a s . data . frame ( t a b l e ( C 2$De f aul t ) )
Cluster1 PD=data . frame ( Category=c ( ”Good ” , ”Bad ” ,
”Good ” , ”Bad ” , ”Good ” ,
”Bad ” , ”Good ” , ”Bad ” ) ,
P r o b a b i l i t y o f d e f a u l t=c (P1,1−P1 ,
P2,1−P2 , PT1,
1−PT1, PT1,1−PT2 ) )

l i b r a r y ( d pl y r )
M a s t e r t a bl e=cbind ( t1 , t2$Freq , t3$Freq , t4$F req ) ; M a s t e r t a bl e

g g pl o t ( M a s t e r t a bl e , a e s ( x=f a c t o r ( Var1 ) , y=Freq ))+
ge om c ol ( c o l o r =’ bl ack ’ , f i l l =’cyan3 ’)+
xl ab ( ’ Response ’ )
C 1D=s u b s e t ( C 1 , C 1$De f aul t==”Bad ” )
C 2D=s u b s e t ( C 2 , C 2$De f aul t==”Bad ” )
View (C 1D )
T 1D=s u b s e t ( T 1 , T 1$De f aul t==”Bad ” )
T 2D=s u b s e t ( T 2 , T 2$De f aul t==”Bad ” )
El o s s=f u n c ti o n (P1 , Amount ){
l o s s =(1−P1 )∗Amount∗(1 −0.5 )
r e t u r n ( l o s s )
}
#f o r Train
C 1D$ElossR=NA
C 2D$ElossR=NA
s e t . s e e d ( 1 )

15

f o r ( i i n 1 : nrow (C 1D ) ) {
C 1D$ElossR [ i ]= El o s s (P1 , rnorm ( 1 , 1 0 0 0 , 1 0 0 ) )
}
s e t . s e e d ( 1 )
f o r ( i i n 1 : nrow (C 2D ) ) {
C 2D$ElossR [ i ]= El o s s (P2 , rnorm ( 1 , 1 0 0 0 , 1 0 0 ) )
}
e1=sum ( C 1D$ElossR )
e2=sum ( C 2D$ElossR )
T o t a l e x p l o s s=sum ( C 1D$ElossR)+sum ( C 2D$ElossR )
T o t al e a r ni n g=nrow ( C 1 )∗1000− T o t a l e x p l o s s
View (C 2D )
#For t e s t
T 1D$ExplossR=NA
T 2D$ExplossR=NA
View (T 1D )
s e t . s e e d ( 1 )
f o r ( i i n 1 : nrow (T 1D ) ) {
T 1D$ExplossR [ i ]= El o s s (PT1, rnorm ( 1 , 1 0 0 0 , 1 0 0 ) )
}
s e t . s e e d ( 1 )
f o r ( i i n 1 : nrow (T 2D ) ) {
T 2D$ExplossR [ i ]= El o s s (PT2, rnorm ( 1 , 1 0 0 0 , 1 0 0 ) )
}
e t 1=sum ( T 1D$ElossR )
e t 2=sum ( T 2D$ElossR )
View (T 1D )
##Random f o r e s t f o r f a c t o r s
# Loading package
l i b r a r y ( c aT o ol s )
l i b r a r y ( randomForest )
# F i t t i n g Random F o r e s t t o the t r a i n d a t a s e t
View ( Train Data )
s e t . s e e d ( 1 2 0 ) # S e t ti n g s e e d
t r ai nm o di fi e d=Train Data
t r ai nm o di fi e d $D e f a ul t=i f e l s e ( t r ai nm o di fi e d $D e f a ul t==”Good ” , 0 , 1 )
c l a s s i f i e r R F = randomForest ( x = t r ai nm o di fi e d [ −8] ,
y = t r ai nm o di fi e d $D e f a ul t ,
n t r e e = 1 0 0 0 )
16

varImpPlot ( c l a s s i f i e r R F )
# P r e di c ti n g the Test s e t r e s u l t s
y p red = p r e d i c t ( c l a s s i f i e r R F , newdata = Train Data [ −1 0] )
? p r e d i c t ( )
c l a s s i f i e r R F = randomForest ( f a c t o r ( D e f a ul t  ̃ . ) , Train Data )
c l a s s i f i e r R F$ m t r y
#V i s u a l i z a t i o n Part
#p l o t t i n g the AIC and BIC Values f o r optimum number o f c l u s t e r s
g g pl o t ( d f , a e s ( x = Components ) ) +
g e om li n e ( a e s ( y = AIC , c o l o r = ”AIC ” ) ) +
g e om li n e ( a e s ( y = BIC , c o l o r = ”BIC ” ) ) +
l a b s ( t i t l e = ”AIC and BIC f o r D i f f e r e n t Numbers o f Components ” ,
x = ”Number o f Components ” ,
y = ”AIC / BIC ” ) +
s c a l e c o l o r m a n u a l ( v al u e s = c ( ”AIC” = ” bl u e ” , ”BIC” = ” red ” ) ) +
theme minimal ( )
par ( mfrow=c ( 1 , 2 ) )
b a r pl o t ( Cluster1 PD [ 1 : 2 , 2 ] , sp a ce =0.5 , width =1,
xl ab=”Train Cl u s t e r 1 ” , yl ab=” p r o b a b i l i t y ” ,
names . a r g =c ( ”Good ” , ”Bad ” ) , c o l =c ( ” g re e n ” , ” red ” ) )
b a r pl o t ( Cluster1 PD [ 3 : 4 , 2 ] , sp a ce =0.5 ,
xl ab=”Train Cl u s t e r 2 ” , yl ab=” p r o b a b i l i t y ” ,
names . a r g =c ( ”Good ” , ”Bad ” ) , c o l =c ( ” g re e n ” , ” red ” ) )
b a r pl o t ( Cluster1 PD [ 5 : 6 , 2 ] , sp a ce =0.5 , width =0.1 ,
xl ab=”Test Cl u s t e r 1 ” , yl ab=” p r o b a b i l i t y ” ,
names . a r g =c ( ”Good ” , ”Bad ” ) , c o l =c ( ” g re e n ” , ” red ” ) )
b a r pl o t ( Cluster1 PD [ 7 : 8 , 2 ] , sp a ce =0.5 ,
width =0.1 , xl ab=”Test Cl u s t e r 2 ” , yl ab=” p r o b a b i l i t y ” ,
names . a r g =c ( ”Good ” , ”Bad ” ) , c o l =c ( ” g re e n ” , ” red ” ) )
#Expected Loss from main data and model data
p r o b t r ai n=s u b s e t ( Train Data , T r ain D a t a$D e f a ul t==”Bad ” )
p r o b t e s t=s u b s e t ( Test Data , T e s t D a t a$D e f a ul t==”Bad ” )
View ( p r o b t r ai n )
View ( p r o b t e s t )
prob=nrow ( p r o b t r ai n ) / nrow ( Train Data )
prob1=nrow ( p r o b t e s t ) / nrow ( Test Data )
17

s e t . s e e d ( 1 )
E xpl o s sT r ain 1=El o s s (1−prob , rnorm ( 1 , 1 0 0 0 , 1 0 0 ) ) ∗ nrow ( p r o b t r ai n )
s e t . s e e d ( 1 )
E xpl o s sTe s t 1=El o s s (1−prob1 , rnorm ( 1 , 1 0 0 0 , 1 0 0 ) ) ∗ nrow ( p r o b t e s t )
e1=sum ( C 1D$ElossR)+sum ( C 2D$ElossR )
e t 1=sum ( T 1D$ExplossR)+sum ( T 2D$ExplossR )
T 1D$ExplossR=NA
T 2D$ExplossR=NA
View (T 1D )
s e t . s e e d ( 1 )
f o r ( i i n 1 : nrow (T 1D ) ) {
T 1D$ExplossR [ i ]= El o s s (PT1, rnorm ( 1 , 1 0 0 0 , 1 0 0 ) )
}
s e t . s e e d ( 1 )
f o r ( i i n 1 : nrow (T 2D ) ) {
T 2D$ExplossR [ i ]= El o s s (PT2, rnorm ( 1 , 1 0 0 0 , 1 0 0 ) )
}
s e t . s e e d ( 1 )
sum=NA
f o r ( i i n 1 : nrow ( T 2 ) ) {
a=rnorm ( 1 , 1 0 0 0 , 1 0 0 )
sum [ i ]=a
}
nrow ( Train Data )
sum ( sum )
L o s s D a t a f r ame=data . frame ( E x p l o s sT r ai n=e1 ,
E xpl o s sTe s t=et1 ,
Ac tu alL o s sT r ain=Expl o s sT r ain 1 ,
Ac tu alL o s sTe s t=Expl o s sTe s t 1 ,
E r r o rPe rcT r ain=(e1−E xpl o s sT r ain 1 ) / ( e1 ) ∗1 0 0 ,
E r r o rPe rcTe s t=(et1−E xpl o s sTe s t 1 ) / ( e t 1 ) ∗1 0 0 )

#Model Accuracy
i n s t a l l . p ack a ge s ( ” c a r e t ” )
l i b r a r y ( ” c a r e t ” )
T r ai n D a t a $D e f a ul t P r o b a bili t y=NA
18

f o r ( i i n 1 : nrow ( Train Data ) ) {
T r ai n D a t a $D e f a ul t P r o b a bili t y [ i ]=sum (Gmm model$z [ i , ] ∗ c (1−P1,1−P2 ) )
}
Test Data=Test Data [ , −9]
f o r ( i i n 1 : nrow ( Test Data ) ) {
T e s t D a t a $D e f a ul t P r o b a bili t y [ i ]=sum ( GMM test$z [ i , ] ∗ c (1−PT1,1−PT2 ) )
}
c o n f u si o nM a t ri x ( a s . f a c t o r ( i f e l s e ( p r e d i c t i o n s $ c l a s s i f i c a t i o n ==1,

’Good ’ , ’ Bad ’ ) ) ,
a s . f a c t o r ( T e s t D a t a$D e f a ul t ) )
i f e l s e ( p r e d i c t i o n s $ c l a s s i f i c a t i o n ==1,”Good ” , ”Bad ” )
t a b l e ( i f e l s e ( p r e d i c t i o n s $ c l a s s i f i c a t i o n ==1,”Good ” , ”Bad ” ) )
t a b l e ( T e s t D a t a$D e f a ul t )
#im p o r ti n g the data
C r e d i t s c o r e=re ad . c s v ( ”D: \ \MSc(ASA)\ \Sem1\\prOJECT\\Data Se t \\

c r e d i t r i s k . c s v ” )

View ( C red Sc o re )
#ch an gin g the mi s si n g v al u e rows with mean
l i b r a r y ( d pl y r )
mean . data=C r e d i t s c o r e %>% group by ( C r e d i t s c o r e $ I n t e n t ) %>%
summarise (Mean=mean ( C r e di t s c o r e$R a t e , na . rm = T) )
Emp=C r e d i t s c o r e %>% group by (Home, Cl a s s ) %>%
summarise (Max=max( C r e di t s c o r e$Em p l e n g t h , na . rm=T ) , . g r oup s = ” keep ” )
#ch an gin g the na column with the mean
nrow ( Data )
C r e di t s c o r e $R a t e=i f e l s e ( i s . na ( C r e di t s c o r e $R a t e ) , 1 1 , C r e di t s c o r e $R a t e )
C r e di t s c o r e$Em p l e n g t h=i f e l s e ( i s . na ( C r e di t s c o r e$Em p l e n g t h )&
( C r e d i t s c o r e $C l a s s ==0) ,54 ,
i f e l s e ( i s . na ( C r e di t s c o r e$Em p l e n g t h )&
( C r e d i t s c o r e $C l a s s ==1) ,46.25 ,
C r e di t s c o r e$Em p l e n g t h ) )

summary ( C r e di t s c o r e $C r e d l e n g t h )
#J oi ni n g i t with the main t a b l e
New Cred=C r e d i t s c o r e
######I n t e n t 1
I n t e n t 1=s u b s e t ( New Cred , New Cred$Intent==”DEBTCONSOLIDATION” )

19

I n t e n t 2=s u b s e t ( New Cred , New Cred$Intent==”PERSONAL” )
I n t e n t 3=s u b s e t ( New Cred , New Cred$Intent==”VENTURE” )
I n t e n t 4=s u b s e t ( New Cred , New Cred$Intent==”EDUCATION” )
I n t e n t 5=s u b s e t ( New Cred , New Cred$Intent==”HOMEIMPROVEMENT” )
I n t e n t 6=s u b s e t ( New Cred , New Cred$Intent==”MEDICAL” )
par ( mfrow=c ( 2 , 3 ) )
ABC=f u n c ti o n (X){
X P=l e n g t h ( which ( X$De fault==”Y” ) ) / nrow (X)
X$SI=NA
X$Expec ted re tu rn=NA
f o r ( i i n 1 : nrow (X) ) {
S . I =(X$Amount [ i ] ∗ X$Rate [ i ] ∗ X$Cred leng th [ i ] / ( 1 2 ∗ 1 0 0 ) )
X$SI [ i ]=(X$Amount [ i ]+S . I )
}
f o r ( i i n 1 : nrow (X) ) {
ExpRet=(X$SI [ i ] ∗X P∗0. 8 )+ ( X$SI [ i ]∗(1 −X P ) )
X$Expec ted re tu rn [ i ]=ExpRet
}
X$P r o fi t=X$Expected return−X$Amount
P r o f i t 6=sum ( X$P r o fi t )
h i s t ( X$P r o fi t , main = p a s t e ( ” Histogram o f ” , X$Intent [ 1 ] ) , xl ab=”P r o f i t ” )
r e t u r n (X)
}
I n t e n t 1=ABC( I n t e n t 1 )
I n t e n t 2=ABC( I n t e n t 2 )
I n t e n t 3=ABC( I n t e n t 3 )
I n t e n t 4=ABC( I n t e n t 4 )
I n t e n t 5=ABC( I n t e n t 5 )
I n t e n t 6=ABC( I n t e n t 6 )
par ( mfrow=c ( 1 , 1 ) )
p r o f i t D f=data . frame ( Category=c ( ”DEBTCONSOLIDATION” , ”PERSONAL” ,

”VENTURE” ,
”EDUCATION” ,
”HOMEIMPROVEMENT” ,
”MEDICAL” ) ,
P r o f i t s=c ( sum ( I n t e n t 1 $P r o f i t ) ,
sum ( I n t e n t 2 $P r o f i t ) ,
sum ( I n t e n t 3 $P r o f i t ) ,
sum ( I n t e n t 4 $P r o f i t ) ,
sum ( I n t e n t 5 $P r o f i t ) ,
sum ( I n t e n t 6 $P r o f i t ) ) )
20

Category=data . frame ( Category=c ( ”DEBTCONSOLIDATION”=859886.5 ,
”PERSONAL”=1148443.9 ,
”VENTURE”=1148443.9 ,
”EDUCATION”=841113.4 ,
”HOMEIMPROVEMENT”=867121.0 ,
”MEDICAL”= 1 1 4 5 4 5 4. 6 ) )

b a r pl o t ( p r o f i t D f [ , 2 ] ,
y l i m i t=c ( 0 , 1 0 ∗ 1 0 ˆ 8 ) ,
c o l = c ( ” red ” , ” bl u e ” , ” g r ee n ” , ” o r an ge ” , ” v i o l e t ” , ” p u r pl e ” ) ,
xl ab = ” P r o f i t ” ,
yl ab=”Amount i n Rs ” ,
names . a r g = c ( ”DEBTCONSOLIDATION” ,

”PERSONAL” , ”VENTURE” , ”EDUCATION” ,
”HOMEIMPROVEMENT” , ”MEDICAL” ) , sp ac e =0.5)

sum ( I n t e n t 6 $P r o f i t )−sum ( I n t e n t 6 $P r o f i t )
Cor=c o r ( Train Data [ , 1 : 7 ] , method = ” k e n d all ” )
l i b r a r y ( c o r r p l o t )
c o r r p l o t ( Cor )

p r o f i t D f=data . frame ( Category=c ( ”DEBTCONSOLIDATION” ,
”PERSONAL” , ”VENTURE” ,
”EDUCATION” , ”HOMEIMPROVEMENT”
, ”MEDICAL” ) ,
P r o f i t s=c ( sum ( I n t e n t 1 $P r o f i t ) ,
sum ( I n t e n t 2 $P r o f i t ) ,
sum ( I n t e n t 3 $P r o f i t ) ,
sum ( I n t e n t 4 $P r o f i t ) ,
sum ( I n t e n t 5 $P r o f i t ) ,
sum ( I n t e n t 6 $P r o f i t ) ) )

Category=data . frame ( Category=c ( ”DEBTCONSOLIDATION”=859886.5 ,
”PERSONAL”=1148443.9 ,
”VENTURE”=1148443.9 ,
”EDUCATION”=841113.4 ,
”HOMEIMPROVEMENT”=867121.0 ,
”MEDICAL”= 1 1 4 5 4 5 4. 6 ) )

b a r pl o t ( p r o f i t D f [ , 2 ] , width =0.6 ,
y l i m i t=c ( 0 , 1 0 ∗ 1 0 ˆ 8 ) ,
c o l = c ( ” tan3 ” ) ,
xl ab = ” P r o f i t ” ,
yl ab=”Amount i n Rs ” , names . a r g = c ( ”DEBTCONSOLIDATION” ,
”PERSONAL” , ”VENTURE” ,
”EDUCATION” , ”HOMEIMPROVEMENT”, ”MEDICAL” ) , space =0.5)

