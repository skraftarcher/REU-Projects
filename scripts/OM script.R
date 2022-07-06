#Haley.Crawford.7.6.22.REU.Organic.Matter.in.Reefs
#bringing in data, looking at data, (little bit) reorganization
#load packages
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("gsheet")
#bringing in 1st data sheet - 
l1<-"https://docs.google.com/spreadsheets/d/1UE8_6ugM6WHM_59GSWc9J2lrzkgt4lOtrvylQcRWO2o/edit?usp=sharing"
omquadratdata<-read.csv(text = gsheet2text(l1,format = "csv"),stringsAsFactors = FALSE)
#finished bringing in data, looking at data
View(omquadratdata)
summary(omquadratdata)
glimpse(omquadratdata)
#visualizing data
theme_set(theme_bw()+theme(panel.grid=element_blank()))
ggplot(data=omquadratdata)+
  #geom_bar(aes(x=Depth,y=Live.Count,fill=Live.Count),stat = "identity")
  #geom_bar(fill="pink",aes(x=Depth,y=Live.Count),stat = "identity")
  #geom_point(aes(x=Depth,y=Live.Count),color="red")
  #geom_histogram(aes(x=Depth),fill="green",color="black")
  #geom_violin(aes(x=Site,y=Live.Count))
  #geom_boxplot(aes(x=Site,y=Live.Count))
