#Samantha Schlegel 7-26-23
#HM data analysis 

library("tidyverse")

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
#but if I do it with all vanadium values instead of the average, there will still only be one value for each human category

  

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
  geom_boxplot(data=HM,aes(y=V,x=debris,fill=site))

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

