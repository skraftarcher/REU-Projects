#this is a comment, and this script is for pushing code to Github

# Mikel  06/12/23

# example of leading packages----


source("scripts/install_packages_function.R")

lp(pck="tidyverse")

lp("gsheet")

#SG= seagrass
# Load seagrass data----

sg19<-read.csv("odata/Nov2019_seagrass.csv")

glimpse(sg19)
summary(sg19)

