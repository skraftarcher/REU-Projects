#Samantha Schlegel 7-29-23
#CI and HM data 

summary(ci2)
summary(HM)

library("tidyverse")
library("gsheet")

ci2<-read.csv("wdata/ci.final.data_29_July_2023.csv")


# creating same column name 
ci3<-ci2%>%
  separate(Label,into=c("sample","ext1","ext2"),sep="-")

ci4<-ci3%>%
  unite(sample,c(sample,ext1),sep="",remove=FALSE)%>%
  select(-ext1,-ext2)


#getting CI first 
oc1<-ci4%>%
  mutate(tdw=final.dw,
         sdw=dw.shell-shell.tin.weight,
         sww=ww.shell-shell.tin.weight)%>%
  group_by(collection.date,sample)%>%
  #summarize(whole.oyster.weight=sum(whole.oyster.weight),
  #tdw=sum(tdw),
  #sdw=sum(sdw),
  #sww=sum(sww))%>%
  mutate(oc1=(tdw/(whole.oyster.weight-sww))*100,
         oc2=(tdw/(whole.oyster.weight-sdw))*100)%>%
  select(date=collection.date,sample,oc1,oc2)

write.csv(oc1,paste0("wdata/sam_oyster_condition",Sys.Date(),".csv"),row.names = FALSE)

# joining data sets - creating matching column name to merge the data sets
ci.hm<-oc1%>%
  left_join(HM)


ci.hm.1<-ci.hm%>%
  group_by(site)%>%
  mutate(oc1.mean=mean(oc1,na.rm=TRUE),oc2.mean=mean(oc2,na.rm=TRUE),V.mean=mean(V,na.rm=TRUE),Cr.mean=mean(Cr,na.rm=TRUE),Fe.mean=mean(Fe,na.rm=TRUE),
            As.mean=mean(As,na.rm=TRUE),Ba.mean=mean(Ba,na.rm=TRUE),Pb.mean=mean(Pb,na.rm=TRUE),Cu.mean=mean(Cu,na.rm=TRUE),
            Zn.mean=mean(Zn,na.rm=TRUE),totalHM.mean=mean(totalHM,na.rm=TRUE),debris1=mean(debris,na.rm=TRUE),
            oilgas1=mean(oilgas,na.rm=TRUE),shipping1=mean(shipping,na.rm=TRUE),temp1=mean(temp,na.rm=TRUE),
            do1=mean(do,na.rm=TRUE),ph1=mean(pH,na.rm=TRUE))

ci.hm.av<-ci.hm%>%
  group_by(site)%>%
  summarize(oc1.mean=mean(oc1,na.rm=TRUE),oc2.mean=mean(oc2,na.rm=TRUE),V.mean=mean(V,na.rm=TRUE),Cr.mean=mean(Cr,na.rm=TRUE),Fe.mean=mean(Fe,na.rm=TRUE),
         As.mean=mean(As,na.rm=TRUE),Ba.mean=mean(Ba,na.rm=TRUE),Pb.mean=mean(Pb,na.rm=TRUE),Cu.mean=mean(Cu,na.rm=TRUE),
         Zn.mean=mean(Zn,na.rm=TRUE),totalHM.mean=mean(totalHM,na.rm=TRUE),debris1=mean(debris,na.rm=TRUE),
         oilgas1=mean(oilgas,na.rm=TRUE),shipping1=mean(shipping,na.rm=TRUE),temp1=mean(temp,na.rm=TRUE),
         do1=mean(do,na.rm=TRUE),ph1=mean(pH,na.rm=TRUE))

#graphing 

#site vs CI 
#ci1
ggplot()+
  geom_point(data=ci.hm,aes(x=site,y=oc1))
ggplot()+
  geom_point(data=ci.hm.1,aes(x=site,y=oc1.mean,color=site))
ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc1,x=site))
#lm - significant 
oc1.site<-lm(oc1~site+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(oc1.site)
summary(oc1.site)

#ANOVA - significant but violates homogeneity of variance 
oc1.site.aov<-aov(oc1~site,data=ci.hm)
par(mfrow=c(2,2))
plot(oc1.site.aov)
summary(oc1.site.aov)
TukeyHSD(oc1.site.aov)
# h and b significant 


#ci2
ggplot()+
  geom_point(data=ci.hm,aes(x=site,y=oc2))
ggplot()+
  geom_point(data=ci.hm.1,aes(x=site,y=oc2.mean,color=site))
ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc2,x=site))
#lm - significant 
oc2.site<-lm(oc2~site+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(oc2.site)
summary(oc2.site)

#ANOVA - significant 
oc2.site.aov<-aov(oc2~site,data=ci.hm)
par(mfrow=c(2,2))
plot(oc2.site.aov)
summary(oc2.site.aov)
TukeyHSD(oc2.site.aov)
#h and b significant 



#totalHM
#oyster condition 1
ggplot()+
  geom_point(data=ci.hm,aes(x=totalHM,y=oc1,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=totalHM.mean,y=oc1.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc1,x=totalHM,fill=site))

#oyster condition 2
ggplot()+
  geom_point(data=ci.hm,aes(x=totalHM,y=oc2,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=totalHM.mean,y=oc2.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc2,x=totalHM,fill=site))

#all sites except bayou shows as heavy metal increases, condition index decreases

hm.ci1<-lm(oc1~totalHM+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(hm.ci1)
summary(hm.ci1)

hm.ci2<-lm(oc2~totalHM+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(hm.ci2)
summary(hm.ci2)
#neither significant but does not show normality 

#vanadium 
#oc1
ggplot()+
  geom_point(data=ci.hm,aes(x=V,y=oc1,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=V.mean,y=oc1.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc1,x=V,fill=site))

#oc2
ggplot()+
  geom_point(data=ci.hm,aes(x=V,y=oc2,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=V.mean,y=oc2.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc2,x=V,fill=site))

V.ci1<-lm(oc1~V+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(V.ci1)
summary(V.ci1)
#not significant - V vs OC1

V.ci2<-lm(oc2~V+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(V.ci2)
summary(V.ci2)
#not significant between V and OC2


#Chromium 
#oc1
ggplot()+
  geom_point(data=ci.hm,aes(x=Cr,y=oc1,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Cr.mean,y=oc1.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc1,x=Cr,fill=site))

#oc2
ggplot()+
  geom_point(data=ci.hm,aes(x=Cr,y=oc2,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Cr.mean,y=oc2.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc2,x=Cr,fill=site))
#all sites except houma nav show as cr increases oc decreases

Cr.ci1<-lm(oc1~Cr+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Cr.ci1)
summary(Cr.ci1)
#not significant - Cr vs OC1

Cr.ci2<-lm(oc2~Cr+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Cr.ci2)
summary(Cr.ci2)
#not significant between Cr and OC2


#Fe 
#oc1
ggplot()+
  geom_point(data=ci.hm,aes(x=Fe,y=oc1,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Fe.mean,y=oc1.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc1,x=Fe,fill=site))

#oc2
ggplot()+
  geom_point(data=ci.hm,aes(x=Fe,y=oc2,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Fe.mean,y=oc2.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc2,x=Fe,fill=site))
#all sites except sister lake show as fe increases oc decreases

Fe.ci1<-lm(oc1~Fe+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Fe.ci1)
summary(Fe.ci1)
#not significant - Fe vs OC1

Fe.ci2<-lm(oc2~Fe+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Fe.ci2)
summary(Fe.ci2)
#not significant between Fe and OC2

#As
#oc1
ggplot()+
  geom_point(data=ci.hm,aes(x=As,y=oc1,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=As.mean,y=oc1.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc1,x=As,fill=site))

#oc2
ggplot()+
  geom_point(data=ci.hm,aes(x=As,y=oc2,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=As.mean,y=oc2.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc2,x=As,fill=site))
#no pattern 

As.ci1<-lm(oc1~As+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(As.ci1)
summary(As.ci1)
#not significant - As vs OC1

As.ci2<-lm(oc2~As+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(As.ci2)
summary(As.ci2)
#not significant between As and OC2

#Ba
#oc1
ggplot()+
  geom_point(data=ci.hm,aes(x=Ba,y=oc1,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Ba.mean,y=oc1.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc1,x=Ba,fill=site))

#oc2
ggplot()+
  geom_point(data=ci.hm,aes(x=Ba,y=oc2,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Ba.mean,y=oc2.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc2,x=Ba,fill=site))
#no pattern 

Ba.ci1<-lm(oc1~Ba+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Ba.ci1)
summary(Ba.ci1)
#not significant - Ba vs OC1

Ba.ci2<-lm(oc2~Ba+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Ba.ci2)
summary(Ba.ci2)
#not significant between Ba and OC2

#Pb
#oc1
ggplot()+
  geom_point(data=ci.hm,aes(x=Pb,y=oc1,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Pb.mean,y=oc1.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc1,x=Pb,fill=site))

#oc2
ggplot()+
  geom_point(data=ci.hm,aes(x=Pb,y=oc2,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Pb.mean,y=oc2.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc2,x=Pb,fill=site))
#no pattern 

Pb.ci1<-lm(oc1~Pb+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Pb.ci1)
summary(Pb.ci1)
#not significant - Pb vs OC1

Pb.ci2<-lm(oc2~Pb+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Pb.ci2)
summary(Pb.ci2)
#not significant between Pb and OC2


#Cu
#oc1
ggplot()+
  geom_point(data=ci.hm,aes(x=Cu,y=oc1,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Cu.mean,y=oc1.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc1,x=Cu,fill=site))
#as cu increases oc1 increases except for sister lake

#oc2
ggplot()+
  geom_point(data=ci.hm,aes(x=Cu,y=oc2,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Cu.mean,y=oc2.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc2,x=Cu,fill=site))
#no pattern 

Cu.ci1<-lm(oc1~Cu+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Cu.ci1)
summary(Cu.ci1)
#not significant - Cu vs OC1

Cu.ci2<-lm(oc2~Cu+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Cu.ci2)
summary(Cu.ci2)
#not significant between Cu and OC2


#Zn
#oc1
ggplot()+
  geom_point(data=ci.hm,aes(x=Zn,y=oc1,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Zn.mean,y=oc1.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc1,x=Zn,fill=site))
#as zn increases oc1 decreases except for bayou

#oc2
ggplot()+
  geom_point(data=ci.hm,aes(x=Zn,y=oc2,color=site))

ggplot()+
  geom_point(data=ci.hm.1,aes(x=Zn.mean,y=oc2.mean,color=site))

ggplot()+
  geom_boxplot(data=ci.hm,aes(y=oc2,x=Zn,fill=site))
#as zn increases oc1 decreases except for bayou

Zn.ci1<-lm(oc1~Zn+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Zn.ci1)
summary(Zn.ci1)
#not significant - Zn vs OC1

Zn.ci2<-lm(oc2~Zn+temp,data=ci.hm)
par(mfrow=c(2,2))
plot(Zn.ci2)
summary(Zn.ci2)
#not significant between Zn and OC2








