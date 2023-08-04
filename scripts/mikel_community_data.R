source("scripts/install_packages_function.R")
lp("readxl")
lp("tidyverse")
lp("vegan")

# bring in data
total.comd<-read_xlsx("odata/Mikeldata.xlsx",sheet = 1)[-6,]%>%# this brings in your community data (that I downloaded and put into my odata) and removes the total row as we don't need that.
    mutate(Pond=as.numeric(Pond))
# bring in the datasets for each sampling method individually 
trap.comd<-read_xlsx("odata/Mikeldata.xlsx",sheet = 2)[-6,]%>%# this brings in your community data (that I downloaded and put into my odata) and removes the total row as we don't need that.
  mutate(Pond=as.numeric(Pond))
cast.comd<-read_xlsx("odata/Mikeldata.xlsx",sheet = 4)[-6,]%>%# this brings in your community data (that I downloaded and put into my odata) and removes the total row as we don't need that.
  mutate(Pond=as.numeric(Pond))
seine.comd<-read_xlsx("odata/Mikeldata.xlsx",sheet = 3)[-6,]%>%# this brings in your community data (that I downloaded and put into my odata) and removes the total row as we don't need that.
  mutate(Pond=as.numeric(Pond))
      
pond.char<-read_xlsx("odata/Pond_depth_salin_temp.xlsx",sheet=1)# this brings in your pond characteristics data (that I downloaded and put into my odata)

#bring in acoustic data
pond.spl<-read_rds("wdata/pond_sum_SPL.rds")%>%
  rename(Pond=pond)
pond.index<-read_rds("wdata/pond_sum_ACI_NDSI.rds")%>%
  rename(Pond=pond)

# split data set into two - a dataset with your community data in one and your environmental data in the other

pond.env<-left_join(total.comd[,1:3],pond.char)%>%
  left_join(pond.spl)%>%
  left_join(pond.index)%>%
  mutate(seine.effort=ifelse(Pond==12,`Surface area`,`Surface area`*2))

pond.com.total<-total.comd[,-1:-3]
pond.com.trap<-trap.comd[,-1:-3]
pond.com.cast<-cast.comd[,-1:-3]
pond.com.seine<-seine.comd[,-1:-3]

# now we can first standardize by effort
pond.com.cast2<-pond.com.cast/5

pond.com.trap2<-pond.com.trap/pond.env$`Minnow trap`

pond.com.seine2<-pond.com.seine/pond.env$seine.effort

pond.com.cast.std<-decostand(pond.com.cast2,method = "frequency")
pond.com.trap.std<-decostand(pond.com.trap2,method = "frequency")
pond.com.seine.std<-decostand(pond.com.seine2,method = "frequency")

pond.com.dens<-pond.com.total/pond.env$`Surface area`
pond.com.std<-pond.com.cast.std+pond.com.trap.std+pond.com.seine.std

#now we can calculate diversity variables
pond.env$spr<-specnumber(pond.com.total)# species richness
pond.env$div<-diversity(pond.com.std)# species diversity (Shannon-weiner)
pond.env$evn<-pond.env$div/log(pond.env$spr)#species evenness
