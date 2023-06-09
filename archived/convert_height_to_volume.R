#playing around w fia's live oyster data

#bringing in a package
source("scripts/install_packages_function.R")
lp("tidyverse")

#bringing in data
lod<-read.csv("odata/liveoysterdata.csv")

#visualizing data
ggplot(data=lod, aes(x=log(Height.mm), y=log(Volume.Oyster.mL)))+
  geom_point()+
  geom_smooth(method = "lm")

#generating equation
lod.lm<-lm(log(Volume.Oyster.mL)~log(Height.mm),data = lod)
par(mfrow=c(2,2))  
plot(lod.lm)
summary(lod.lm)
