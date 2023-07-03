#Figure Practice for Project (using FAKE data)
#Samantha Schlegel 7-1-23

#reading file in 
HMCIP<-read.csv("odata/sam_HMCIprac.csv")
class(HMCIP)
class(HMCIP$Site)
class(HMCIP$HMC1)
class(HMCIP$CI1)  
class(HMCIP$CI2) 
class(HMCIP$CI3) 
class(HMCIP$CI4) 
class(HMCIP$CI5) 
HMCIP$CI1<-as.numeric(HMCIP$CI1)
HMCIP$CI2<-as.numeric(HMCIP$CI2)
HMCIP$CI3<-as.numeric(HMCIP$CI3)
HMCIP$CI4<-as.numeric(HMCIP$CI4)
HMCIP$CI5<-as.numeric(HMCIP$CI5)


HMCIP2<-HMCIP%>%
  mutate(hmc.mean=mean(HMC1,HMC2,HMC3,HMC4,HMC5,na.rm=TRUE))

#sg192<-sg19%>%
 # select(Site,Quadrat,Date,Cnpy1Tt,Dnsty1Tt)%>%
  #group_by(Site)%>%
  #mutate(cnpy.m=mean(Cnpy1Tt,na.rm=TRUE))

#sg192<-sg19%>%
 # select(Site,Quadrat,Date,Cnpy1Tt,Dnsty1Tt)%>%
  #group_by(Site)%>%
  #summarize(cnpy.m=mean(Cnpy1Tt,na.rm=TRUE),
         #   cnpy.sd=sd(Cnpy1Tt,na.rm=TRUE))

#changed data format - reading in new file 
HMCIP2<-read.csv("odata/sam_HMCIpract2.csv")
class(HMCIP2)
class(HMCIP2$HMC)
class(HMCIP2$CI)
HMCIP2$CI<-as.numeric(HMCIP2$CI)

HMCIP2a<-HMCIP2%>%
  select(Site,Oyster.,HMC,CI)%>%
  group_by(Site)%>%
  mutate(hmc.mean=mean(HMC,na.rm=TRUE),ci.mean=mean(CI,na.rm=TRUE))

#working plot - plain 
ggplot()+
  geom_point(data=HMCIP2a,aes(x=hmc.mean,y=ci.mean,fill=Site))

#adding things to plot 
sd.hmc=sd(HMCIP2$HMC,na.rm=TRUE)
sd.hmc
sd.ci=sd(HMCIP2$CI,na.rm=TRUE)
sd.ci
hmc.mean=mean(HMCIP2$HMC)

#working plot (minus the sd bars)
ggplot()+
  #geom_jitter(data=HMCIP2,aes(x=HMC,y=CI,color=Site),size=2,alpha=.3,width=.1)+
  geom_point(data=HMCIP2a,aes(x=hmc.mean,y=ci.mean,color=Site),size=3)+
  labs(x="Mean Heavy Metal concentration (mg/kg)",y="Mean Condition Index")+
               geom_errorbar(data=HMCIP2a,aes(x=hmc.mean,ymin=hmc.mean-sd.hmc,ymax=hmc.mean+sd.hmc))+
              theme(panel.grid=element_blank())
      
  
                               
  #reading in Pb experiment practice fake data
Pbprac<-read.csv("odata/sam_PbPrac.csv")
class(Pbprac)
class(Pbprac$O2consumption)
class(Pbprac$treatment)
class(Pbprac$oyster)
class(Pbprac$day)

#not sure how to get mean o2 consumption for each day and treatment (two group_bys)
Pbpraca<-Pbprac%>%
  select(day,oyster,treatment,O2consumption)%>%
  group_by(day)%>%
  mutate(o2.mean=mean(O2consumption,na.rm=TRUE))

#reading in rearranged data 
Pbprac2<-read.csv("odata/sam_PbPrac2.csv")
ggplot()+
  geom_col(data=Pbprac2,aes(x=treatment,y=av.O2,color=day))
?geom_bar
#questions^ how to get low,medium, high in that order; how to get multiple bars -
#for each treatment instead of what is shown; change color of each bar/ color scale 

           
           
           