#Test run to learn how to push code to github 
#Samantha Schlegel 06/12/2023

#example of loading packages 

#load packages script

#argumetn pck 
#! = is not
#; new line of code 
#" " 
lp<-function(pck){
  if(!require(pck,character.only = TRUE))install.packages(pck);library(pck,character.only = TRUE)
}
source("scripts/install_packages_function.R")

lp(pck="tidyverse")
lp(pck="gsheet")

# seagrass data


#load data ----
sg19<-read.csv("odata/Nov2019_seagrass.csv")
glimpse(sg19)
summary(sg19)