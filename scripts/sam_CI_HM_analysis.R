#Samantha Schlegel 
#CI and HM data 

summary(ci2)
summary(HM)

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





