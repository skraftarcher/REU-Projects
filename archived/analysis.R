# start to analysis script

# install/load packages and download maps/data
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("lmerTest")
lp("glmmTMB")
lp("DHARMa")
theme_set(theme_bw()+theme(panel.grid=element_blank()))

# load data
sites<-read.csv("wdata/sites_withsummarydata.csv")
quads<-read.csv("wdata/full_quadratlevel_dataset.csv")%>%
  mutate(oys.perm2=Live.Count/(0.25*0.25))
tide<-read.csv("odata/noaa.csv")


# look and see if sites differ on oyster abundance and oyster material (live and dead material)
# first visualize it
ggplot()+
  geom_jitter(data=quads,aes(x=SiteID,y=oys.perm2),color="grey")+
  geom_point(data=sites,aes(x=SiteID,y=m.count),size=3)+
  geom_errorbar(data=sites,aes(x=SiteID,ymin=m.count-sd.count,
                               ymax=m.count+sd.count),width=.1)

lo.glm<-glmmTMB(Live.Count~SiteID,
              data=quads,
              family=nbinom1)
testDispersion(lo.glm)
plot(simulateResiduals(fittedModel = lo.glm))

summary(lo.glm)
car::Anova(lo.glm)

# variability within reefs means there's no significant
# difference in live oyster density between reefs

# look at total shell material in same way
ggplot()+
  geom_jitter(data=quads,aes(x=SiteID,y=shell.Lperm3),color="grey")+
  geom_point(data=sites,aes(x=SiteID,y=tot.vol),size=3)+
  geom_errorbar(data=sites,aes(x=SiteID,ymin=tot.vol-tot.vol.sd,
                               ymax=tot.vol+tot.vol.sd),width=.1)

shell.glm<-glmmTMB(shell.Lperm3~SiteID,
                data=quads)
testDispersion(shell.glm)
plot(simulateResiduals(fittedModel = shell.glm))

summary(shell.glm)
car::Anova(shell.glm)

# there are differences between sites in volume of total shell material

#difference of distance to marsh within a site
ggplot()+
  geom_point(data=quads%>%filter(SiteID=="OH5"),aes(x=dist.marsh,y=shell.Lperm3),color="grey")
 # geom_point(data=sites,aes(x=SiteID,y=tot.vol),size=3)+
 # geom_errorbar(data=sites,aes(x=SiteID,ymin=tot.vol-tot.vol.sd,
                              # ymax=tot.vol+tot.vol.sd),width=.1)

#Shell volume difference

# note from Stephanie - this is the same analysis as
# the shell L per m3 
ggplot()+
  geom_jitter(data=quads,aes(x=SiteID,y=shell.volume.L),color="grey")+
  geom_point(data=sites,aes(x=SiteID,y=tot.vol),size=3)+
  geom_errorbar(data=sites,aes(x=SiteID,ymin=tot.vol-tot.vol.sd,
                               ymax=tot.vol+tot.vol.sd),width=.1)

shell.glm<-glmmTMB(shell.volume.L~SiteID,
                   data=quads)
testDispersion(shell.glm)
plot(simulateResiduals(fittedModel = shell.glm))

summary(shell.glm)
car::Anova(shell.glm)


# Organic matter
# lets visualize this a couple ways
ggplot(data=quads)+
  #geom_point(aes(x=shell.Lperm3,y=om,color=SiteID))
  #geom_point(aes(x=oys.perm2,y=om,color=SiteID))
  #geom_point(aes(x=dist.marsh.m,y=om,color=SiteID))
  geom_jitter(aes(x=SiteID,y=om,color=oys.perm2),width=.1)+
  scale_color_viridis_c()

# doesn't look like anything will be significant but
# we can try

om.glm<-glmmTMB(om~dist.marsh.m*oys.perm2*shell.Lperm3+(1|SiteID),
               data=quads)

testDispersion(om.glm)
plot(simulateResiduals(fittedModel = om.glm))

summary(om.glm)
