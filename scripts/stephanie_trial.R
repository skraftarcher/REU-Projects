# This script is a test to learn how to push code to github

# Stephanie Archer 6/12/2023

# example of loading packages----
source("scripts/install_packages_function.R")

lp(pck="tidyverse")

lp("gsheet")


# load data----

sg19<-read.csv("odata/Nov2019_seagrass.csv")

glimpse(sg19)
summary(sg19)

sg19$Date[1:5]
sg19[1:5,2]

ggplot(sg19)+
  geom_boxplot(aes(y=Cnpy1Tt,fill=Site))

sg19$Site<-factor(sg19$Site,level=c("HC","TC","SC","MOW","CA","HT","LH"))

ggplot(data=sg19)+
#  geom_vline(aes(xintercept=0),size=3,color="purple")+
  #geom_boxplot(aes(y=Cnpy1Tt,fill=Site,color=Site))+
   geom_jitter(aes(x=Site,y=Cnpy2Tt,color=Cnpy3Tt,size=Tt),shape=17,width = .1,alpha=.2)+
  #geom_jitter(aes(x=Dnsty1Tt,y=Cnpy2Tt,color=Site),size=4,width = .1,alpha=.5)+
  #geom_point(aes(x=Dnsty1Tt,y=Cnpy2Tt,color=Cnpy3Tt),size=4,alpha=.3)+
  #scale_color_viridis_d(option="A",end=.8,begin=.2)#+
  scale_color_viridis_c(option="B",end=.8)+
  ylab("This is an ugly figure")+
  theme(legend.position = "top")

# creating a list and working with them.----
x=data.frame(a=c(1,2,3),b=c(2,3,4))
x[2,1]
x$a[3]

x2<-x%>%
  mutate(c=a+b,
         d=mean(a),
         e=sd(a))


sglist<-list(x,y="this is part of the list",z=c(1,2,3))

sglist$x$a
sglist[[1]]

sgvector<-c(1,1,1,2,3,4,5,1,2,233)

sglm<-lm(Tt~Hw,data=sg19)
summary(sglm)
sglm$coefficients



# playing with real data
sg192<-sg19%>%
  select(Site,Quadrat,Date,Cnpy1Tt,Dnsty1Tt)%>%
  group_by(Site)%>%
  summarize(cnpy.m=mean(Cnpy1Tt,na.rm = TRUE),
            cnpy.sd=sd(Cnpy1Tt,na.rm = TRUE))

sg192
  

# make a figure
ggplot()+
  geom_jitter(data=sg19,aes(x=Site,y=Cnpy1Tt,color=Site),
              size=2,alpha=.3,width=.1)+
  geom_point(data=sg192,aes(x=Site,y=cnpy.m),size=4)+
  geom_errorbar(data=sg192,aes(x=Site,ymin=cnpy.m-cnpy.sd,ymax=cnpy.m+cnpy.sd),width=.2)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(family="mono",face = "bold",size=24),
        legend.position = "bottom",
        legend.background = element_rect(color="black"),
        plot.title = element_text(hjust=0.5,size=24))+
  ggtitle("Plot title")

