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
