# example script for Mikel-
# bringing in SPL Data, calculating date/time and summary variables

# load packages
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("lubridate")

# bring in data
pond8<-read_rds("odata/Conk_pond8split_rename_Rel_Broadband_96000ptHannWindow_5000pcOlap.rds")[-1,]

# get a valid date time
pond8b<-as.data.frame(pond8)%>%
  mutate(date.time=as.POSIXct(V1,origin="1970-01-01"),
         pond=8)%>%
  select(pond,date.time,spl=V2)

# do the above code for every pond

# calculate summary statistics of SPL
pond.sum<-pond8b%>%# when you have every pond do rbind(pond1b,pond2b, etc) instead of a single data set
  group_by(pond)%>%
  summarize(mean.spl=mean(spl),
            median.spl=median(spl),
            sd.spl=sd(spl))
  