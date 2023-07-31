#Samantha Schlegel 7-26-23
#HM data analysis 

library("tidyverse")

#first steps are to visualize data thorugh scatter and box plots and then to check out residuals

HM<-read.csv("odata/sam_HM1_26_July_2023.csv")

HM1<-HM%>%
  select(site,sample,V,Cr,As,Ba,Pb,Cu,Zn,Fe,totalHM,debris,oilgas,shipping,temp,do,pH)%>%
  group_by(site)%>%
  mutate(V.mean=mean(V,na.rm=TRUE),Cr.mean=mean(Cr,na.rm=TRUE),Fe.mean=mean(Fe,na.rm=TRUE),
            As.mean=mean(As,na.rm=TRUE),Ba.mean=mean(Ba,na.rm=TRUE),Pb.mean=mean(Pb,na.rm=TRUE),Cu.mean=mean(Cu,na.rm=TRUE),
            Zn.mean=mean(Zn,na.rm=TRUE),totalHM.mean=mean(totalHM,na.rm=TRUE))

HM.av<-HM%>%
  select(site,sample,V,Cr,As,Ba,Pb,Cu,Zn,Fe,totalHM,debris,oilgas,shipping,temp,do,pH)%>%
  group_by(site)%>%
  summarize(V.mean=mean(V,na.rm=TRUE),Cr.mean=mean(Cr,na.rm=TRUE),Fe.mean=mean(Fe,na.rm=TRUE),
         As.mean=mean(As,na.rm=TRUE),Ba.mean=mean(Ba,na.rm=TRUE),Pb.mean=mean(Pb,na.rm=TRUE),Cu.mean=mean(Cu,na.rm=TRUE),
         Zn.mean=mean(Zn,na.rm=TRUE),totalHM.mean=mean(totalHM,na.rm=TRUE),debris1=mean(debris,na.rm=TRUE),
         oilgas1=mean(oilgas,na.rm=TRUE),shipping1=mean(shipping,na.rm=TRUE),temp1=mean(temp,na.rm=TRUE),
         do1=mean(do,na.rm=TRUE),ph1=mean(pH,na.rm=TRUE))

#scatterplots with site and each heavy metal and total heavy metal 
ggplot()+
  geom_point(data=HM.av,aes(x=site,y=totalHM.mean))
ggplot()+
  geom_point(data=HM.av,aes(x=site,y=V.mean))
ggplot()+
  geom_point(data=HM.av,aes(x=site,y=Fe.mean))
ggplot()+
  geom_point(data=HM.av,aes(x=site,y=As.mean))
ggplot()+
  geom_point(data=HM.av,aes(x=site,y=Ba.mean))
ggplot()+
  geom_point(data=HM.av,aes(x=site,y=Pb.mean))
ggplot()+
  geom_point(data=HM.av,aes(x=site,y=Cu.mean))
ggplot()+
  geom_point(data=HM.av,aes(x=site,y=Zn.mean))
ggplot()+
  geom_point(data=HM.av,aes(x=site,y=Cr.mean))

#splitting data by site to do correlation for each site by itself because I am 
#not sure how to do that when it is all together like in HM.av

HM.av.L<-subset(HM.av,subset=HM.av$site=="L")
HM.av.b<-subset(HM.av,subset=HM.av$site=="b")
HM.av.h<-subset(HM.av,subset=HM.av$site=="h")
HM.av.s<-subset(HM.av,subset=HM.av$site=="s")

ggplot()+
  geom_point(data=HM.av.L,aes(x=site,y=totalHM.mean))

cor.test(HM.av.L$debris1,HM.av.L$V.mean)
#there is only two points so that is why it cannot do a correlation if I am understanding correctly,
#but if I do it with all vanadium values instead of the average, there will still only be one value for each human category - ANALYZE ACROSS SITES

  

#scatter plots with site and total heavy metals 
ggplot()+
  geom_point(data=HM1,aes(x=site,y=totalHM.mean))
                           
ggplot()+
    geom_jitter(data=HM,aes(x=site,y=totalHM))

ggplot()+
  geom_jitter(data=HM,aes(x=site,y=totalHM),position=position_jitter(width=0.09),
              geom_point(data=HM1,aes(x=site,y=totalHM.mean)))
ggplot()+
geom_point(data=HM1,aes(x=site,y=totalHM.mean)+
             geom_jitter(data=HM,aes(x=site,y=totalHM)))

#scatterplots with debris categories and total heavy metals
ggplot()+
  geom_point(data=HM,aes(x=debris,y=totalHM))
ggplot()+
  geom_point(data=HM1,aes(x=debris,y=totalHM.mean))
ggplot()+
  geom_point(data=HM1,aes(x=shipping,y=totalHM.mean))
ggplot()+
  geom_point(data=HM1,aes(x=oilgas,y=totalHM.mean))
ggplot()+
  geom_point(data=HM1,aes(x=temp,y=totalHM.mean))
ggplot()+
  geom_point(data=HM1,aes(x=do,y=totalHM.mean))
ggplot()+
  geom_point(data=HM1,aes(x=pH,y=totalHM.mean))

#summarystats
class(HM$site)
class(HM$totalHM)
summary(HM1)
library(moments)
skewness(HM1$totalHM)
kurtosis(HM1$totalHM)
range(HM1$totalHM)
hist(HM1$totalHM)
var(HM1$totalHM)

cor.test(HM1$debris,HM1$totalHM.mean)
cor.test(HM1$oilgas,HM1$totalHM.mean)
cor.test(HM1$shipping,HM1$totalHM.mean)

cor.test(HM$debris,HM$V.mean)
class(HM$V.mean)
class(HM$V)
HM$V.mean<-as.numeric(HM$V.mean)
cor.test(HM$V,HM$debris)

ggplot()+
  geom_point(data=HM,aes(x=debris,y=V),fill=site)
ggplot()+
  geom_point(data=HM,aes(x=debris,y=V),groupName=site)
#how to color points by site 

ggplot()+
  geom_boxplot(data=HM,aes(y=V,fill=site))

ggplot()+
  geom_boxplot(data=HM,aes(y=V,fill=debris,x=site))

ggplot()+
  geom_boxplot(data=HM,aes(y=Pb,x=debris,fill=site))

ggplot()+
  geom_boxplot(data=HM,aes(y=Cr,x=debris,fill=site))

ggplot()+
  geom_boxplot(data=HM,aes(y=Fe,x=debris,fill=site))

ggplot()+
  geom_boxplot(data=HM,aes(y=Ba,x=debris,fill=site))

ggplot()+
  geom_boxplot(data=HM,aes(y=Zn,x=debris,fill=site))

ggplot()+
  geom_boxplot(data=HM,aes(y=As,x=debris,fill=site))



#starting models
install.packages("lmerTest")
library(lmerTest)
#random effects model (might use)

sam2<-lm(totalHM~as.factor(site,data=HM))

sam3<-aov(totalHM~as.factor(site,data=HM))
#look at data with scatter and box 
sam1<-lm(totalHM~debris+temp+do+pH,data=HM)
sam1<-lm(totalHM~debris+temp,data=HM)#choose this one - pH and do had relationships with each other (check again) 



#makes scatter plots and from those plots, make notes about what relationships you see
#and then come up with models you want to run 
#outliers mean you need to do non-parametric 
#lead, arsenic, chromium - very bad metals in terms of impact

#expect total HM to be explained by debris, temp, do, and pH 
# * means the variables are interacting and affecting each other ; it gets complicated ; know how to interpret why those two things 
#would react to change your response variable
# + means the predictor variables are independently influencing the response variable


par(mfrow=c(2,2))
plot(sam1)#investigate residuals 
  
#want to plot 2 rows and 2 columns
# fourth plot if you have factor variable
#normal QQ - residuals are normally distributed - need to fit close to line-looking for big departures
#two biggest assumptions to worry about - resisudal vs fitted and normal QQ 

summary(sam1)
#NAs - dont have enough data points or if you have it in your data or if its perfectly correlated with other thigns 
#look at spread of do, temp, pH - widest spread 
#adjsuted R squared - accounting for predcitor values, this is the proportion of variance in response variables  
#the higher = the better fit the model 
#p value for overall model 
#individual p values are for specific predictor variabels 

#checking out temp do pH and how they relate to each other - if there are relationships you may just have to pick one that you feel the 
#most comfortable explaining how it impacts heavy metals 
ggplot(data=HM)+geom_point(aes(x=temp,y=debris))
#dont want correlation - multicorelleantiy 
#only include one covariate in model - which one i understnad best 



#starting models part #2

#looking at relationship between covariates 
ggplot(data=HM)+geom_point(aes(x=temp,y=do))
#correlated
ggplot(data=HM)+geom_point(aes(x=temp,y=pH))
#relationship
ggplot(data=HM)+geom_point(aes(x=pH,y=do))
#relationship
#all covariates have a relationship with each other 


#HM between sites
#totalHM concentrations are not significant between sites
#individual HM are not significant between sites
ggplot()+
  geom_point(data=HM,aes(x=site,y=totalHM,color=site))
sam4<-lm(totalHM~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam4)#investigate residuals 
summary(sam4)

ggplot()+
  geom_point(data=HM,aes(x=site,y=V,color=site))
sam5<-lm(V~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam5)#investigate residuals 
summary(sam5)

ggplot()+
  geom_point(data=HM,aes(x=site,y=Cr,color=site))
sam6<-lm(Cr~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam6)#investigate residuals 
summary(sam6)

ggplot()+
  geom_point(data=HM,aes(x=site,y=Fe,color=site))
sam7<-lm(Fe~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam7)#investigate residuals 
summary(sam7)

ggplot()+
  geom_point(data=HM,aes(x=site,y=As,color=site))
sam8<-lm(As~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam8)#investigate residuals 
summary(sam8)

ggplot()+
  geom_point(data=HM,aes(x=site,y=Ba,color=site))
sam9<-lm(Ba~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam9)#investigate residuals 
summary(sam9)

ggplot()+
  geom_point(data=HM,aes(x=site,y=Pb,color=site))
sam10<-lm(Pb~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam10)#investigate residuals 
summary(sam10)
#bayou has outlier - looks like a small pattern, but it is not signficant 

ggplot()+
  geom_point(data=HM,aes(x=site,y=Cu,color=site))
sam11<-lm(Cu~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam11)#investigate residuals 
summary(sam11)

ggplot()+
  geom_point(data=HM,aes(x=site,y=Zn,color=site))
sam12<-lm(Zn~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam12)#investigate residuals 
summary(sam12)



##looking at totalHM vs. human categories
#debris
#no clear pattern between totalHM and debris - not significant 
ggplot()+
  geom_point(data=HM,aes(x=debris,y=totalHM,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=debris1,y=totalHM.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=totalHM,fill=debris,x=site))
#no clear pattern between totalHM and debris between sites - not significant 
sam1<-lm(totalHM~debris+temp+do+pH,data=HM)
par(mfrow=c(2,2))
plot(sam1)#investigate residuals 
summary(sam1)


#?review residualas vs. fitted - supposed to look nonuniform and randomly distributed? and 
#what the coefficients table shows

sam1a<-lm(totalHM~debris+temp,data=HM)
par(mfrow=c(2,2))
plot(sam1a)#investigate residuals 
summary(sam1a)

sam1c<-lm(totalHM~do,data=HM)
par(mfrow=c(2,2))
plot(sam1c)#investigate residuals 
summary(sam1c)

sam1d<-lm(totalHM~pH,data=HM)
par(mfrow=c(2,2))
plot(sam1d)#investigate residuals 
summary(sam1d)



#ANOVA
sam1b<-aov(totalHM~site,data=HM)
summary(sam1b)
par(mfrow=c(2,2))
plot(sam1b)
#no clear pattern between totalHM and debris - using lm and aov - not significant 

#shipping
#small pattern between total HM and shipping - not significant
ggplot()+
  geom_point(data=HM,aes(x=shipping,y=totalHM,color=site))
ggplot()+
  geom_jitter(data=HM,aes(x=shipping,y=totalHM,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=shipping1,y=totalHM.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=totalHM,fill=shipping,x=site))
#shows sites that are in shipping channels (3 out of 4) have higher total heavy metal []
#the bayou is a tighter more concentrated channel and has the highest av HM concentration (taking into account 
#there is one high value that may be skewing the mean); the houma nav is a less concentrated area; LUMCON channel
#is not as busy as the other channels; sister lake is not exactly in a major shipping channel 
summary(HM.av)

sam2<-lm(totalHM~shipping+temp,data=HM)
par(mfrow=c(2,2))
plot(sam2)#investigate residuals 
summary(sam2)


#oilgas
#oilgas does not influence totalHM a lot
ggplot()+
  geom_point(data=HM,aes(x=oilgas,y=totalHM,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=oilgas1,y=totalHM.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=totalHM,fill=oilgas,x=site))

sam3<-lm(totalHM~oilgas+temp,data=HM)
par(mfrow=c(2,2))
plot(sam3)#investigate residuals 
summary(sam3)
#not signficant - total HM and oilgas




#HM concentrations are not significant between sites
sam4<-lm(totalHM~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam4)#investigate residuals 
summary(sam4)

sam5<-lm(V~site+temp,data=HM)
par(mfrow=c(2,2))
plot(sam5)#investigate residuals 
summary(sam5)



##looking at individual HM 
#V
#debris
ggplot()+
  geom_point(data=HM,aes(x=debris,y=V,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=debris1,y=V.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=V,fill=debris,x=site))
#small relationship - sites with more debris had slightly higher levels of V - a lot of outliers
samV<-lm(V~debris+temp,data=HM)
par(mfrow=c(2,2))
plot(samV)#investigate residuals 
summary(samV)
#no significant relationship between V and debris 

#shipping
ggplot()+
  geom_point(data=HM,aes(x=shipping,y=V,color=site))
ggplot()+
  geom_jitter(data=HM,aes(x=shipping,y=V,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=shipping1,y=V.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=V,fill=shipping,x=site))
#no clear pattern between V and shipping between sites

samVs<-lm(V~shipping+temp,data=HM)#Vs= vanadium and shipping
par(mfrow=c(2,2))
plot(samVs)#investigate residuals 
summary(samVs)
#no significance between V and shipping between sites

#oilgas
ggplot()+
  geom_point(data=HM,aes(x=oilgas,y=V,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=oilgas1,y=V.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=V,fill=oilgas,x=site))
#LUMCON has the highest oil/gas and the highest V median - makes sense as oilgas is a source of V

samVo<-lm(V~oilgas+temp,data=HM)#samVo = vanadiuma and oilgas
par(mfrow=c(2,2))
plot(samVo)#investigate residuals 
summary(samVo)
#not significant - V vs oilgas between sites

#Cr
#debris
ggplot()+
  geom_point(data=HM,aes(x=debris,y=Cr,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=debris1,y=Cr.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Cr,fill=debris,x=site))
#no relationship between debris and Cr between sites

samCr.d<-lm(Cr~debris+temp,data=HM)#samCr.d= chromium vs debris
par(mfrow=c(2,2))
plot(samCr.d)#investigate residuals 
summary(samCr.d)
#no significant relationship between Cr and debris 

#shipping
ggplot()+
  geom_point(data=HM,aes(x=shipping,y=Cr,color=site))
ggplot()+
  geom_jitter(data=HM,aes(x=shipping,y=Cr,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=shipping1,y=Cr.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Cr,fill=shipping,x=site))
#no clear pattern between V and shipping between sites

samCr.s<-lm(Cr~shipping+temp,data=HM)
par(mfrow=c(2,2))
plot(samCr.s)#investigate residuals 
summary(samCr.s)
#no significance between Cr and shipping between sites

#oilgas
ggplot()+
  geom_point(data=HM,aes(x=oilgas,y=Cr,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=oilgas1,y=Cr.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Cr,fill=oilgas,x=site))


samCr.o<-lm(Cr~oilgas+temp,data=HM)#samVo = vanadiuma and oilgas
par(mfrow=c(2,2))
plot(samCr.o)#investigate residuals 
summary(samCr.o)
#not significant - Cr vs oilgas between sites

#Fe
#debris
ggplot()+
  geom_point(data=HM,aes(x=debris,y=Fe,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=debris1,y=Fe.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Fe,fill=debris,x=site))
#no relationship 

samFe.d<-lm(Fe~debris+temp,data=HM)
par(mfrow=c(2,2))
plot(samFe.d)#investigate residuals 
summary(samFe.d)
#no significant relationship between Fe and debris 

#shipping
ggplot()+
  geom_point(data=HM,aes(x=shipping,y=Fe,color=site))
ggplot()+
  geom_jitter(data=HM,aes(x=shipping,y=Fe,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=shipping1,y=Fe.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Fe,fill=shipping,x=site))
#no clear pattern between Fe and shipping between sites

samFe.s<-lm(Fe~shipping+temp,data=HM)
par(mfrow=c(2,2))
plot(samFe.s)#investigate residuals 
summary(samFe.s)
#no significance between Fe and shipping between sites

#oilgas
ggplot()+
  geom_point(data=HM,aes(x=oilgas,y=Fe,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=oilgas1,y=Fe.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Fe,fill=oilgas,x=site))


samFe.o<-lm(Fe~oilgas+temp,data=HM)
par(mfrow=c(2,2))
plot(samFe.o)#investigate residuals 
summary(samFe.o)
#not significant - Fe vs oilgas between sites


#As
#debris
ggplot()+
  geom_point(data=HM,aes(x=debris,y=As,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=debris1,y=As.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=As,fill=debris,x=site))
#no relationship 

samAs.d<-lm(As~debris+temp,data=HM)
par(mfrow=c(2,2))
plot(samAs.d)#investigate residuals 
summary(samAs.d)
#no significant relationship between As and debris 

#shipping
ggplot()+
  geom_point(data=HM,aes(x=shipping,y=As,color=site))
ggplot()+
  geom_jitter(data=HM,aes(x=shipping,y=As,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=shipping1,y=As.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=As,fill=shipping,x=site))
#no clear pattern between As and shipping between sites

samAs.s<-lm(As~shipping+temp,data=HM)
par(mfrow=c(2,2))
plot(samAs.s)#investigate residuals 
summary(samAs.s)
#no significance between As and shipping between sites

#oilgas
ggplot()+
  geom_point(data=HM,aes(x=oilgas,y=As,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=oilgas1,y=As.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=As,fill=oilgas,x=site))
#no relationship

samAs.o<-lm(As~oilgas+temp,data=HM)
par(mfrow=c(2,2))
plot(samAs.o)#investigate residuals 
summary(samAs.o)
#not significant - As vs oilgas between sites

#Ba
#debris
ggplot()+
  geom_point(data=HM,aes(x=debris,y=Ba,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=debris1,y=Ba.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Ba,fill=debris,x=site))
#sites with more debris have slightly more ba

samBa.d<-lm(Ba~debris+temp,data=HM)
par(mfrow=c(2,2))
plot(samBa.d)#investigate residuals 
summary(samBa.d)
#no significant relationship between Ba and debris 

#shipping
ggplot()+
  geom_point(data=HM,aes(x=shipping,y=Ba,color=site))
ggplot()+
  geom_jitter(data=HM,aes(x=shipping,y=Ba,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=shipping1,y=Ba.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Ba,fill=shipping,x=site))
#no clear pattern between Ba and shipping between sites

samBa.s<-lm(Ba~shipping+temp,data=HM)
par(mfrow=c(2,2))
plot(samBa.s)#investigate residuals 
summary(samBa.s)
#no significance between Ba and shipping 

#oilgas
ggplot()+
  geom_point(data=HM,aes(x=oilgas,y=Ba,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=oilgas1,y=Ba.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Ba,fill=oilgas,x=site))
#small relationship between more oilgas and more Ba

samBa.o<-lm(Ba~oilgas+temp,data=HM)
par(mfrow=c(2,2))
plot(samBa.o)#investigate residuals 
summary(samBa.o)
#not significant - Ba vs oilgas 

#Pb
#debris
ggplot()+
  geom_point(data=HM,aes(x=debris,y=Pb,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=debris1,y=Pb.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Pb,fill=debris,x=site))
#sites with more debris have slightly more Pb - small linear relationship

samPb.d<-lm(Pb~debris+temp,data=HM)
par(mfrow=c(2,2))
plot(samPb.d)#investigate residuals 
summary(samPb.d)
#?significant relationship between Pb and debris but residuals vs. fitted does not look
#like it fits the assumption of normality 

#shipping
ggplot()+
  geom_point(data=HM,aes(x=shipping,y=Pb,color=site))
ggplot()+
  geom_jitter(data=HM,aes(x=shipping,y=Pb,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=shipping1,y=Pb.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Pb,fill=shipping,x=site))
#no clear pattern between Pb and shipping between sites

samPb.s<-lm(Pb~shipping+temp,data=HM)
par(mfrow=c(2,2))
plot(samPb.s)#investigate residuals 
summary(samPb.s)
#no significance between Pb and shipping 

#oilgas
ggplot()+
  geom_point(data=HM,aes(x=oilgas,y=Pb,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=oilgas1,y=Pb.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Pb,fill=oilgas,x=site))
#no relationship between Pb and oilgas 

samPb.o<-lm(Pb~oilgas+temp,data=HM)
par(mfrow=c(2,2))
plot(samPb.o)#investigate residuals 
summary(samPb.o)
#?significant Pb and oilgas but not sure if residuals are normal

#?is there a way to look at significance between site,heavy metal, and human activity? so far 
#i have only tried site vs. HM and human acitvity vs. HM 

#Cu
#debris
ggplot()+
  geom_point(data=HM,aes(x=debris,y=Cu,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=debris1,y=Cu.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Cu,fill=debris,x=site))
#no relationship with Cu and debris 

samCu.d<-lm(Cu~debris+temp,data=HM)
par(mfrow=c(2,2))
plot(samCu.d)#investigate residuals 
summary(samCu.d)
#no significance between Cu and debris

#shipping
ggplot()+
  geom_point(data=HM,aes(x=shipping,y=Cu,color=site))
ggplot()+
  geom_jitter(data=HM,aes(x=shipping,y=Cu,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=shipping1,y=Cu.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Cu,fill=shipping,x=site))
#slight pattern between Cu and shipping between sites

samCu.s<-lm(Cu~shipping+temp,data=HM)
par(mfrow=c(2,2))
plot(samCu.s)#investigate residuals 
summary(samCu.s)
#no significance between Cu and shipping 

#oilgas
ggplot()+
  geom_point(data=HM,aes(x=oilgas,y=Cu,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=oilgas1,y=Cu.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Cu,fill=oilgas,x=site))
#no relationship between Cu and oilgas 

samCu.o<-lm(Cu~oilgas+temp,data=HM)
par(mfrow=c(2,2))
plot(samCu.o)#investigate residuals 
summary(samCu.o)
#no signficance - Cu and oilgas

#Zn
#debris
ggplot()+
  geom_point(data=HM,aes(x=debris,y=Zn,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=debris1,y=Zn.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Zn,fill=debris,x=site))
#no relationship with Zn and debris 

samZn.d<-lm(Zn~debris+temp,data=HM)
par(mfrow=c(2,2))
plot(samZn.d)#investigate residuals 
summary(samZn.d)
#no significance between Zn and debris

#shipping
ggplot()+
  geom_point(data=HM,aes(x=shipping,y=Zn,color=site))
ggplot()+
  geom_jitter(data=HM,aes(x=shipping,y=Zn,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=shipping1,y=Zn.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Zn,fill=shipping,x=site))
#less shipping, less Zn 

samZn.s<-lm(Zn~shipping+temp,data=HM)
par(mfrow=c(2,2))
plot(samZn.s)#investigate residuals 
summary(samZn.s)
#no significance between Zn and shipping 

#oilgas
ggplot()+
  geom_point(data=HM,aes(x=oilgas,y=Zn,color=site))
ggplot()+
  geom_point(data=HM.av,aes(x=oilgas1,y=Zn.mean,color=site))
ggplot()+
  geom_boxplot(data=HM,aes(y=Zn,fill=oilgas,x=site))
#no relationship between Zn and oilgas 

samZn.o<-lm(Zn~oilgas+temp,data=HM)
par(mfrow=c(2,2))
plot(samZn.o)#investigate residuals 
summary(samZn.o)
#no signficance - Zn and oilgas



