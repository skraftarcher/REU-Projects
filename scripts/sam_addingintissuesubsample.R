# script to add icp sample back in for oyster condition 

# load necessary packages
library("tidyverse")

# bringing your data 
ci<-read.csv(text=gsheet2text(ci.url,format='csv'),
             stringsAsFactors = FALSE)

icp<-read.csv(text=gsheet2text(icp.url,format='csv'),
              stringsAsFactors = FALSE)

# first organize data to allow us to join data sets
ci2<-ci%>%
  separate(oyster,into=c("Label","ext"),sep="-T")%>%
  select(-ext)%>%
  left_join(icp)%>%
  mutate(ww.tissue2=ww.tissue-tissue.tin.weight,
         dw.tissue2=dw.tissue-tissue.tin.weight)

# now we can calculate the relationship between wet and dry tissue weights
# first I'm going to look at it
ggplot(data=ci2,aes(x=ww.tissue2,y=dw.tissue2))+
  geom_point()+
  geom_smooth(method="lm")

#now modeling it 
wet.dry<-lm(dw.tissue2~0+ww.tissue2,data=ci2)

par(mfrow=c(2,2))
plot(wet.dry)
summary(wet.dry)

icp2<-ci2%>%
  select(ww.tissue2=ww.tissue.icp)

ci2$dw.tissue.icp<-predict(wet.dry,newdata = icp2)

ci2<-ci2%>%
  mutate(final.dw=dw.tissue2+dw.tissue.icp)
