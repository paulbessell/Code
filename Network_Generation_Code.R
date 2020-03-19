####*****************************************************************
#### SECTION A #### This is the code for generating network topologies
####*****************************************************************
library(ggplot2)
library(igraph)
library(reshape2)
library(reshape)
library(pROC)
library(raster)
library(sp)
library(maptools)
library(stringr)
library(dplyr)
library(lattice) # required for trellis.par.set():
library(cowplot)
library(ggrepel)
library(grid)
library(qgraph)
library(gridExtra)
library(ggmap)

##************************************
#### DATA SET CLEANING
##************************************
##**********************************************************************************
#### Demographic analysis Cameroon popn data Empirical /Census data
###*********************************************************************************

## Read in the data

Demogp_dat <- read.csv("cattle_human_pop_2014.csv",sep=",", header=T)#
# cameroon_districs<-readRDS("./cameroon_administrative/CMR_adm1.rds")
cameroon_districts <- readRDS("./cameroon_administrative/CMR_adm1.rds")
cameroon_subdistricts <- readRDS("./cameroon_administrative/CMR_adm2.rds")
cameroon_sub2districts <- readRDS("./cameroon_administrative/CMR_adm3.rds")
livsk_2005 <- read.csv("cattle_human_pop_2014.csv")
livsk_2005$Region <- as.factor(trimws(as.character(livsk_2005$Region)))
regions <- c("Adamawa", "Central", "East", "Ext_North","Littoral", "North", "North_West", "South", "South_West", "West")
livsk_2005$Region <- regions[livsk_2005$Region]
livsk_2005$Pop_ANHU_diff <- livsk_2005$Subdivision_cattle_pop_num - livsk_2005$Subdivision_human_pop_num

Cattle_population <- tapply(livsk_2005$Subdivision_cattle_pop_num, livsk_2005$Region, sum)

Cameroon_dat_popn <- data.frame(regions, as.vector(Cattle_population))
names(Cameroon_dat_popn) <- c("Region", "Cattle_population")


### Human Population
### Include the regions as in the livsk_2005 db

Cameroon_HUM_popn <- as.data.frame(tapply(livsk_2005$Subdivision_human_pop_num_2005, livsk_2005$Region, sum))
popDiff <- tapply(livsk_2005$pop_diff20052016, livsk_2005$Region, sum)

names(Cameroon_HUM_popn) <- "Human_population"
Cameroon_HUM_popn$Region <- regions
Cameroon_HUM_popn$popDiff <- as.vector(popDiff)


### CENSUS DATA CAMEROON (DBSX3)


## MOLECULAR DATABASE(DBSX2)-##---------- -----------------------------------Start-------------------------------------------------------------------Section A1

VNTR_data <- read.csv("mat_DB_cstrct1.csv", sep=',',header=T)

# Recode  variables Spoligotype and VNTR(This also rescales data to Subdivision level as described in the Maniscripts) 
#WE are only interested in Variation within a specific spoligotype
# Try to use stringr next time
#  VNTR_data$random=do.call("rbind",strsplit(as.character(VNTR_data$Dep_Genotype),"_"))[,1]
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_69"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_69"),]$Dep_VNTR<-"V69"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_70"),]$Dep_VNTR<-"V70"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_70"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_72"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_72"),]$Dep_VNTR<-"V72"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V12"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V12"),]$Dep_VNTR<-"V12"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V2"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V2"),]$Dep_VNTR<-"V2"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V22"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V22"),]$Dep_VNTR<-"V22"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V24"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V24"),]$Dep_VNTR<-"V24"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V26"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V26"),]$Dep_VNTR<-"V26"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V28"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V28"),]$Dep_VNTR<-"V28"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V36"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V36"),]$Dep_VNTR<-"V36"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V37"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V37"),]$Dep_VNTR<-"V37"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V40"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V40"),]$Dep_VNTR<-"V40"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V43"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V43"),]$Dep_VNTR<-"V43"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V44"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V44"),]$Dep_VNTR<-"V44"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V45"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V45"),]$Dep_VNTR<-"V45"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V5"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V5"),]$Dep_VNTR<-"V5"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V52"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V52"),]$Dep_VNTR<-"V52"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V53"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V53"),]$Dep_VNTR<-"V53"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V55"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V55"),]$Dep_VNTR<-"V55"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V57"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V57"),]$Dep_VNTR<-"V57"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V59"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V59"),]$Dep_VNTR<-"V59"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V60"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V60"),]$Dep_VNTR<-"V60"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V61"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V61"),]$Dep_VNTR<-"V61"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V62"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V62"),]$Dep_VNTR<-"V62"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V64"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V64"),]$Dep_VNTR<-"V64"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V65"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V65"),]$Dep_VNTR<-"V65"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V66"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V66"),]$Dep_VNTR<-"V66"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V67"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V67"),]$Dep_VNTR<-"V67"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V73"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V73"),]$Dep_VNTR<-"V73"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V74"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V74"),]$Dep_VNTR<-"V74"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V75"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V75"),]$Dep_VNTR<-"V75"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V78"),]$Dep_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0944_V78"),]$Dep_VNTR<-"V78"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_19"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_19"),]$Dep_VNTR<-"V19"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V1"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V1"),]$Dep_VNTR<-"V1"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V10"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V10"),]$Dep_VNTR<-"V10"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V12"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V12"),]$Dep_VNTR<-"V12"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V14"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V14"),]$Dep_VNTR<-"V14"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V15"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V15"),]$Dep_VNTR<-"V15"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V17"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V17"),]$Dep_VNTR<-"V17"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V2"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V2"),]$Dep_VNTR<-"V2"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V3"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V3"),]$Dep_VNTR<-"V3"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V4"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V4"),]$Dep_VNTR<-"V4"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V56"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V56"),]$Dep_VNTR<-"V56"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V6"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V6"),]$Dep_VNTR<-"V6"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V76"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V76"),]$Dep_VNTR<-"V76"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V8"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V8"),]$Dep_VNTR<-"V8"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V9"),]$Dep_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB0953_V9"),]$Dep_VNTR<-"V9"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB1025_V38"),]$Dep_spoligo<-"SB1025"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB1025_V38"),]$Dep_VNTR<-"V38"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB1025_V47"),]$Dep_spoligo<-"SB1025"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB1025_V47"),]$Dep_VNTR<-"V47"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB1025_V71"),]$Dep_spoligo<-"SB1025"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB1025_V71"),]$Dep_VNTR<-"V71"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB1460_V22"),]$Dep_spoligo<-"SB1460"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB1460_V22"),]$Dep_VNTR<-"V22"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB1460_V32"),]$Dep_spoligo<-"SB1460"
VNTR_data[which(VNTR_data$Dep_Genotype=="SB1460_V32"),]$Dep_VNTR<-"V32"

# **** ***** ***** **** ****** **
# **** FOR THE DESTINATION NOW***
# **** ***** ***** **** ****** **
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_69"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_69"),]$Dest_VNTR<-"V69"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V12"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V12"),]$Dest_VNTR<-"V12"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V2"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V2"),]$Dest_VNTR<-"V2"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V22"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V22"),]$Dest_VNTR<-"V22"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V24"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V24"),]$Dest_VNTR<-"V24"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V25"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V25"),]$Dest_VNTR<-"V25"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V26"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V26"),]$Dest_VNTR<-"V26"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V28"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V28"),]$Dest_VNTR<-"V28"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V36"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V36"),]$Dest_VNTR<-"V36"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V37"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V37"),]$Dest_VNTR<-"V37"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V40"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V40"),]$Dest_VNTR<-"V40"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V43"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V43"),]$Dest_VNTR<-"V43"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V44"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V44"),]$Dest_VNTR<-"V44"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V45"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V45"),]$Dest_VNTR<-"V45"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V5"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V5"),]$Dest_VNTR<-"V5"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V52"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V52"),]$Dest_VNTR<-"V52"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V53"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V53"),]$Dest_VNTR<-"V53"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V55"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V55"),]$Dest_VNTR<-"V55"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V57"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V57"),]$Dest_VNTR<-"V57"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V59"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V59"),]$Dest_VNTR<-"V59"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V60"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V60"),]$Dest_VNTR<-"V60"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V61"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V61"),]$Dest_VNTR<-"V61"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V62"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V62"),]$Dest_VNTR<-"V62"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V64"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V64"),]$Dest_VNTR<-"V64"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V65"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V65"),]$Dest_VNTR<-"V65"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V66"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V66"),]$Dest_VNTR<-"V66"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V67"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V67"),]$Dest_VNTR<-"V67"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V73"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V73"),]$Dest_VNTR<-"V73"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V74"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V74"),]$Dest_VNTR<-"V74"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V75"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V75"),]$Dest_VNTR<-"V75"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V78"),]$Dest_spoligo<-"SB0944"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0944_V78"),]$Dest_VNTR<-"V78"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_19"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_19"),]$Dest_VNTR<-"V19"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V1"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V1"),]$Dest_VNTR<-"V1"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V10"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V10"),]$Dest_VNTR<-"V10"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V12"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V12"),]$Dest_VNTR<-"V12"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V14"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V14"),]$Dest_VNTR<-"V14"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V15"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V15"),]$Dest_VNTR<-"V15"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V17"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V17"),]$Dest_VNTR<-"V17"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V2"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V2"),]$Dest_VNTR<-"V2"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V3"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V3"),]$Dest_VNTR<-"V3"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V4"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V4"),]$Dest_VNTR<-"V4"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V56"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V56"),]$Dest_VNTR<-"V56"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V6"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V6"),]$Dest_VNTR<-"V6"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V76"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V76"),]$Dest_VNTR<-"V76"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V8"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V8"),]$Dest_VNTR<-"V8"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V9"),]$Dest_spoligo<-"SB0953"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB0953_V9"),]$Dest_VNTR<-"V9"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB1025_V38"),]$Dest_spoligo<-"SB1025"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB1025_V38"),]$Dest_VNTR<-"V38"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB1025_V47"),]$Dest_spoligo<-"SB1025"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB1025_V47"),]$Dest_VNTR<-"V47"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB1025_V71"),]$Dest_spoligo<-"SB1025"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB1025_V71"),]$Dest_VNTR<-"V71"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB1460_V22"),]$Dest_spoligo<-"SB1460"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB1460_V22"),]$Dest_VNTR<-"V22"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB1460_V32"),]$Dest_spoligo<-"SB1460"
VNTR_data[which(VNTR_data$Dest_Genotype=="SB1460_V32"),]$Dest_VNTR<-"V32"

# MERGING THE MOLECULAR_DISTANCES TO THE REST OF THE DB

#  Genetic relationship are onldone for VNTR of the same spoligotype

dat <- VNTR_data[which(VNTR_data$Dep_spoligo == VNTR_data$Dest_spoligo), ]

dat$combine <- NA
for (i in 1:length(dat$Dep_VNTR)){
  dat$combine[i] <- paste(dat$Dep_VNTR[i], dat$Dest_VNTR[i], sep="") 
}

VNTR_DIS_data <- read.csv("VNTR_DISTANCES.csv", sep=",", header=T) ### This is found in DBSX2 

dat$Genoty_dist <- VNTR_DIS_data[match(dat$combine, VNTR_DIS_data$combine),]$diffA
dat$Dist_Dep_Dest <- sqrt((dat$Dep_lat-dat$Dest_lat) ^ 2 + ((dat$Dep_lon-dat$Dest_lon) ^ 2)) # Distance based on latitudes and longitudes. Could compute correct distances

# Restrict the plot to 3 distance unites since mycobacterium bovis is clonal in nature, there should be a positive linear relationship, that is  short genetic distances correspond to short geo-distances, so  this scatter is divided into four quandrants,  the q= 0-3/0-4  and 4-6/5-8  must have a positive COR if the theory is to hold. the others should have a negative 


dat1 <- dat[which(dat$Dist_Dep_Dest <=3 & dat$Genoty_dist <= 3), ]
dat1 <- dat[which(dat$Dist_Dep_Dest >3), ]
dat3 <- dat[which(dat$Dist_Dep_Dest <=3 &dat$Genoty_dist<=4), ]
dat4 <- dat[which(dat$Dist_Dep_Dest >=3 & dat$Genoty_dist>=4), ]
dat5 <- dat[which(dat$Dist_Dep_Dest <=3 & dat$Genoty_dist>=4), ]
dat6 <- dat[which(dat$Dist_Dep_Dest >=3 & dat$Genoty_dist<=4), ]
# This defines a genotype distance short enough to still have relevance with regards to geographic distace
# If evolution is a function of time, there for a very large genetic distance is most likely to have taken a considerable time
# in which case the relationship with physical distance becomes noisy and more random. the same is true for Longer distances especially 
# pouplation mixing is common the genotype distance start to occur randomly so the relationship flatenns ( becomes random)
dat7<-  dat[which(dat$Genoty_dist<=4),]


### Correlation test between Physical distance and molecular distance

cor( dat5$Genoty_dist, dat5$Dist_Dep_Dest, method = "pearson")
cor( dat3$Genoty_dist, dat3$Dist_Dep_Dest, method = "pearson")
cor( dat4$Genoty_dist, dat4$Dist_Dep_Dest, method = "pearson")
cor( dat6$Genoty_dist, dat6$Dist_Dep_Dest, method = "pearson")

####***********************************************************
# We are going to put direction on the Molecular based network
#***************************************************************
Movement_matrix_data <- dat3 # This is the most interesting datset form the plots
Movement_matrix_data <- Movement_matrix_data[!duplicated(Movement_matrix_data[2:3]),]

temp <- subset(Movement_matrix_data,Movement_matrix_data$Dep == Movement_matrix_data$Dest)
temp2 <- subset(Movement_matrix_data, !rownames(Movement_matrix_data) %in% rownames(temp))
Movement_matrix_data1 <- temp2
Movement_matrix_data1 <- as.data.frame(Movement_matrix_data1)
#Movement_matrix_data1<-Movement_matrix_data1[,c(2:7,15)]
Movement_matrix_data1$Dep <- gsub(" ", "",Movement_matrix_data1$Dep, fixed = FALSE)
Movement_matrix_data1$Dest <- gsub(" ", "",Movement_matrix_data1$Dest, fixed = FALSE)

#Here we reoved movements that are duplicated dep and dest if if we have a movement in both directions we keep one with the lowest molecular distance (law of evo by diffusion), 
# if both have equal mole distance then we keep both


Diversity_dat <- read.csv("Diversities.csv", sep=",", header=T) #### This is found in DBSX2 

Div_direct <- Diversity_dat### this file is produced acouple of steps down (dont ask me why :)
Div_direct$subdivisions <- gsub(" ", "",Div_direct$subdivisions, fixed = FALSE)
NAKOUE <- c("NAKOUE",0.00000)
Div_direct <- data.frame(rbind(NAKOUE, Div_direct))
p_y <- subset(Div_direct, Div_direct$subdivisions %in% Movement_matrix_data1$Dep==F)
unique(p_y$subdivisions)

p_x <- (subset(Movement_matrix_data1, Movement_matrix_data1$Dep %in% Div_direct$subdivisions==F))
unique(p_x$Dep)
Div_direct$subdivisions[which(Div_direct$subdivisions=="BELEL")] <- "BELO"# Recode Belel==Belo

p_x<-(subset(Movement_matrix_data1,Movement_matrix_data1$Dep %in% Div_direct$subdivisions==F))
unique(p_x$Dep)
Movement_matrix_data1$Dep <- as.character(Movement_matrix_data1$Dep)
Movement_matrix_data1$Dest <- as.character(Movement_matrix_data1$Dest)

Div_direct <- Div_direct[-2,]# removing Bamenda II

Movement_matrix_data1_1 <- Movement_matrix_data1 #[-c(27,80,81,82),]# Clean up step for Nakoue to the sub division

p_x<-(subset(Movement_matrix_data1_1, !Movement_matrix_data1_1$Dep %in% Div_direct$subdivisions))
unique(p_x$Dep)### zero character

p_x_x<-(subset(Movement_matrix_data1_1, !Movement_matrix_data1_1$Dest %in% Div_direct$subdivisions))
unique(p_x_x$Dest)### zero character

Movement_matrix_data1_1$Diversity1<-NA
Movement_matrix_data1_1$Diversity2<-NA
Movement_matrix_data1_1$Div_diff<-NA
Movement_matrix_data1_1$Diversity1 <- Div_direct[match(Movement_matrix_data1_1$Dep, Div_direct$subdivisions),]$Diversity
Movement_matrix_data1_1$Diversity2 <- Div_direct[match(Movement_matrix_data1_1$Dest, Div_direct$subdivisions),]$Diversity
Movement_matrix_data1_1$Div_diff <- as.numeric(Movement_matrix_data1_1$Diversity1) - as.numeric(Movement_matrix_data1_1$Diversity2)


##*************************************************************
# lets direct movement based on  difference in diversity
##*************************************************************

keepOneLine <- function(city1, city2, myTable) {
  subTable <- subset(myTable,
                     (myTable$Dep == city1 & myTable$Dest == city2) | (myTable$Dep == city2 & myTable$Dest == city1)
  )
  subTable<-subset(subTable,Genoty_dist>=0&Div_diff>0)
  
  #subTable <- subTable[order(subTable$Genoty_dist, decreasing = FALSE), ][1, , drop = FALSE]
  return(subTable)
  
}

keepOneLine("BELO", "SANTA", Movement_matrix_data1_1)

cityComb <- combn(
  unique(c(Movement_matrix_data1_1$Dep, Movement_matrix_data1_1$Dest)), 2
)

Movement_matrix_data3 <- do.call(
  rbind,
  lapply(
    1:ncol(cityComb),
    function(x) keepOneLine(cityComb[1, x], cityComb[2, x], Movement_matrix_data1_1)
  )
)
Movement_matrix_data3 <- Movement_matrix_data3[!is.na(Movement_matrix_data3[, 1]), ]

## Rename the nodes so that names are same as in the other networks

## also recode the coordinates of DZELAO

Movement_matrix_data3$Dest_lat[Movement_matrix_data3$Dest== "DZELAO"]<- "10.0088331"
Movement_matrix_data3$Dest_lon[Movement_matrix_data3$Dest== "DZELAO"]<- "14.7829199"

Movement_matrix_data3$Dest_lat[Movement_matrix_data3$Dest== "MOULVOUDAIY"]<- "10.317787"
Movement_matrix_data3$Dest_lon[Movement_matrix_data3$Dest== "MOULVOUDAIY"]<- "14.815887"
Movement_matrix_data3$Dep_lat[Movement_matrix_data3$Dep== "MOULVOUDAIY"]<- "10.317787"
Movement_matrix_data3$Dep_lon[Movement_matrix_data3$Dep== "MOULVOUDAIY"]<- "14.815887"
Movement_matrix_data3$Dep_lat[Movement_matrix_data3$Dep== "NGONG"]<- "7.3142207"
Movement_matrix_data3$Dep_lon[Movement_matrix_data3$Dep== "NGONG"]<- "13.5751265"## this will change to Ngaoundere

Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="BELO"]<- "Belo"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="KUMBO"]<- "Nkambe"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="MBENGWI"]<- "Mbengwi"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="SANTA"]<- "Bamenda I"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="WUM"]<- "Wum"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="BIBEMI"]<- "Bibemi"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="MAROUAI"]<- "Bogo"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="TCHOLLIRE"]<- "Tchollire"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="MISAJE"]<- "Misaje"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="GAROUAIII"]<- "Garoua III"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="NGONG"]<- "Ngaoundere I"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="NGAN-HA"]<- "Ngan-ha"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="FUNDONG"]<- "Fundong"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="NGAOUNDEREII"]<- "Ngaoundere II"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="TOUBORO"]<- "Touboro"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="MOULVOUDAIY"]<- "Moulvoudaye"
Movement_matrix_data3$Dep[Movement_matrix_data3$Dep=="PITOA"]<- "Pitoa"
#Dest recode names

Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="BUM"]<- "Bum"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="FUNDONG"]<- "Fundong"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="BELO"]<- "Belo"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="MISAJE"]<- "Misaje"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="NKUM"]<- "Nkum"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="BOGO"]<- "Bogo"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="DZELAO"]<- "Dziguilao"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="GAROUAIII"]<- "Garoua III"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="MAROUAI"]<- "Maroua I"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="MOULVOUDAIY"]<- "Moulvoudaye"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="NGONG"]<- "Ngaoundere I"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="NGAOUNDEREII"]<- "Ngaoundere II"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="NGAN-HA"]<- "Ngan-ha"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="PITOA"]<- "Pitoa"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="DEMBO"]<- "Dembo"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="NAKOUE"]<- "Banyo"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="SANTA"]<- "Bamenda I"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="KUMBO"]<- "Nkambe"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="MBENGWI"]<- "Mbengwi"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="NYAMBAKA"]<- "Nyambaka"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="NYAMBAKA"]<- "Nyambaka"
Movement_matrix_data3$Dest[Movement_matrix_data3$Dest=="NGAOUNDEREIII"]<- "Ngaoundere III"

# 
# table(Movement_matrix_data3$Dest %in% Em_edgelist$X1)
# table(Movement_matrix_data3$Dest %in% Em_edgelist$X2)
# table(Movement_matrix_data3$Dep %in% Em_edgelist$X2)
# table(Movement_matrix_data3$Dep %in% Em_edgelist$X1)

Movement_matrix_data3$Dep_lat<-as.numeric(Movement_matrix_data3$Dep_lat)
Movement_matrix_data3$Dep_lon<-as.numeric(Movement_matrix_data3$Dep_lon)
Movement_matrix_data3$Dest_lat<-as.numeric(Movement_matrix_data3$Dest_lat)
Movement_matrix_data3$Dest_lon<-as.numeric(Movement_matrix_data3$Dest_lon)

### We are droping and renaming some nodes to match the emperical network

Molecular_diff_edgelist<-Movement_matrix_data3[,c(1:2),]
Molecular_diff_edgelist<-Molecular_diff_edgelist[!duplicated(Molecular_diff_edgelist[1:2]),]

### We are droping and renaming some nodes to match the emperical network

removeThose_m <- c("Garoua III","Moulvoudaye","Dziguilao","Pitoa")
rowToKeep_2 <- !(Molecular_diff_edgelist$Dep %in% removeThose_m | Molecular_diff_edgelist$Dest %in% removeThose_m)
Molecular_diff_edgelist_2 <- Molecular_diff_edgelist[rowToKeep_2, ]

tp <- subset(Molecular_diff_edgelist_2, Dep == Dest)
Nmov_loc <- subset(Molecular_diff_edgelist_2, !rownames(Molecular_diff_edgelist_2) %in% rownames(tp))
Molecular_diff_edgelist_2 <- Nmov_loc
Nmov_loc.data<-Nmov_loc
Nmov_loc.data$Dep_lat<-NA
Nmov_loc.data$Dep_lon<-NA
Nmov_loc.data$Dest_lat<-NA
Nmov_loc.data$Dest_lon<-NA

Nmov_loc.data$Dep <- as.character(Nmov_loc.data$Dep)
Nmov_loc.data$Dest <- as.character(Nmov_loc.data$Dest)
Nmov_loc.data$Dep_lat <- as.numeric(Movement_matrix_data3[match(Nmov_loc.data$Dep,Movement_matrix_data3$Dep),]$Dep_lat)
Nmov_loc.data$Dep_lon <- as.numeric(Movement_matrix_data3[match(Nmov_loc.data$Dep,Movement_matrix_data3$Dep),]$Dep_lon)
Nmov_loc.data$Dest_lat <- as.numeric(Movement_matrix_data3[match(Nmov_loc.data$Dest,Movement_matrix_data3$Dest),]$Dest_lat)
Nmov_loc.data$Dest_lon <- as.numeric(Movement_matrix_data3[match(Nmov_loc.data$Dest,Movement_matrix_data3$Dest),]$Dest_lon)

Nmov_loc.data$Dep[Nmov_loc.data$Dep=="Maroua I"]<- "Belel"
Nmov_loc.data$Dest[Nmov_loc.data$Dest=="Maroua I"]<- "Belel"
Nmov_loc.data$Dep_lat[Nmov_loc.data$Dep=="Belel"]<- "7.0527248"
Nmov_loc.data$Dest_lat[Nmov_loc.data$Dest=="Belel"]<- "7.0527248"
Nmov_loc.data$Dep_lon[Nmov_loc.data$Dep=="Belel"]<- "13.3149575"
Nmov_loc.data$Dest_lon[Nmov_loc.data$Dest=="Belel"]<- "13.3149575"

Nmov_loc.data$Dep_lat<- as.numeric(Nmov_loc.data$Dep_lat)
Nmov_loc.data$Dep_lon<- as.numeric(Nmov_loc.data$Dep_lon)
Nmov_loc.data$Dest_lat<- as.numeric(Nmov_loc.data$Dest_lat)
Nmov_loc.data$Dest_lon<- as.numeric(Nmov_loc.data$Dest_lon)
##------------------------------------------------------End---------- -----------------------------------------------------------------------------------------------Section A1


                                                        #******************************
                                                         ## EMPIRICAL DATA BASE---(DBSX1)
                                                        ####***************************

##--------------------------------------------------------------------START---------------------------------------------------------------------------------------------Section A2


Em_mat <- read.csv("adrian_matrix.csv")# sheet one named adrian_matrix_DBSX1
Em_mat_une <- Em_mat
rownames(Em_mat) <- Em_mat[,1]
Em_mat <- Em_mat[,-1]
class(Em_mat)
Em_mat <- as.matrix(Em_mat)


# Lets create an edgelis from this

Em_mat_edge_list <- melt(Em_mat)## Since this is a subset of the alarger matrix some intermediary are missing, they are generated through recipeint nodes
Em_mat_edge_list$value[Em_mat_edge_list$X1=="MBANG_FOULBE" & Em_mat_edge_list$X2=="TELLO" ] <- 1
Em_mat_edge_list$value[Em_mat_edge_list$X1=="TELLO" & Em_mat_edge_list$X2=="MBANG_FOULBE" ] <- 1
Em_mat_edge_list$value[Em_mat_edge_list$X1=="ADOUMRI" & Em_mat_edge_list$X2=="NGAOUNDERE" ] <- 1
Em_mat_edge_list$value[Em_mat_edge_list$X1=="BOGO" & Em_mat_edge_list$X2=="NGAOUNDERE" ] <- 1
Em_mat_edge_list$value[Em_mat_edge_list$X1=="BOGO" & Em_mat_edge_list$X2=="ADOUMRI" ] <- 1
Em_mat_edge_list$value[Em_mat_edge_list$X1=="BOGO" & Em_mat_edge_list$X2=="ADOUMRI" ] <- 1
Em_mat_edge_list$value[Em_mat_edge_list$X1=="BINSHUA" & Em_mat_edge_list$X2=="BANYO" ] <- 1
Em_mat_edge_list$value[Em_mat_edge_list$X1=="TAKIJA" & Em_mat_edge_list$X2=="BANYO" ] <- 1

Em_mat_edge_list <- subset(Em_mat_edge_list, value > 0)
Em_edgelist <- Em_mat_edge_list[,c(1:2),]

# recode the markets to subdivision

location_dictionary <- read.csv("empirical_data_Paolo .csv", header=T, sep=",")## This is DBSX1_location_dictionary
unique(location_dictionary$Market)
unique(location_dictionary$Subdivision)
Em_edgelist$X1 <- as.character(Em_edgelist$X1)
Em_edgelist$X2 <- as.character(Em_edgelist$X2)
Em_edgelist$X1[Em_edgelist$X1=="BANYO"] <- "Banyo"
Em_edgelist$X1[Em_edgelist$X1=="BAMENDA"] <- "Bamenda I"
Em_edgelist$X1[Em_edgelist$X1=="FOUMBAN"] <- "Foumban"
Em_edgelist$X1[Em_edgelist$X1=="ACHA_TUGI"] <- "Mbengwi"
Em_edgelist$X1[Em_edgelist$X1=="BINKA"] <- "Nkambe"
Em_edgelist$X1[Em_edgelist$X1=="BINSHUA"] <- "Nkambe"
Em_edgelist$X1[Em_edgelist$X1=="FUNDONG"] <- "Fundong"
Em_edgelist$X1[Em_edgelist$X1=="KIMBI"] <- "Bum"
Em_edgelist$X1[Em_edgelist$X1=="KONENE"] <- "Bum"
Em_edgelist$X1[Em_edgelist$X1=="MISAJE"] <- "Misaje"
Em_edgelist$X1[Em_edgelist$X1=="TAKIJA"] <- "Nkum"
Em_edgelist$X1[Em_edgelist$X1=="WUM"] <- "Wum"
Em_edgelist$X1[Em_edgelist$X1=="MBANTI_KATARKO"] <- "Banyo"
Em_edgelist$X1[Em_edgelist$X1=="SAMBA"] <- "Tibati"
Em_edgelist$X1[Em_edgelist$X1=="SAMBO_LABO"] <- "Banyo"
Em_edgelist$X1[Em_edgelist$X1=="MARGOL"] <- "Banyo"
Em_edgelist$X1[Em_edgelist$X1=="MBANG_FOULBE"] <- "Ngan-ha"
Em_edgelist$X1[Em_edgelist$X1=="KOGNOLI"] <- "Nyambaka"
Em_edgelist$X1[Em_edgelist$X1=="TOURNINGAL"] <- "Belel"
Em_edgelist$X1[Em_edgelist$X1=="DIBI"] <- "Nyambaka"
Em_edgelist$X1[Em_edgelist$X1=="MBAYMBOUM"] <- "Tchollire"
Em_edgelist$X1[Em_edgelist$X1=="NYAMBAKA"] <- "Nyambaka"
Em_edgelist$X1[Em_edgelist$X1=="TOUBORO"] <- "Touboro"
Em_edgelist$X1[Em_edgelist$X1=="DANG"] <- "Ngaoundere III"
Em_edgelist$X1[Em_edgelist$X1=="BELEL"] <- "Belel"
Em_edgelist$X1[Em_edgelist$X1=="GARGA"]<- "Meiganga"
Em_edgelist$X1[Em_edgelist$X1=="DJALINGO"]<- "Ngaoundere II"
Em_edgelist$X1[Em_edgelist$X1=="GALDI"]<- "Nyambaka"
Em_edgelist$X1[Em_edgelist$X1=="BAFANG"]<- "Bafang"
Em_edgelist$X1[Em_edgelist$X1=="NGAOUNDERE"]<- "Ngaoundere I"
Em_edgelist$X1[Em_edgelist$X1=="SUBUM"]<- "Bum"
Em_edgelist$X1[Em_edgelist$X1=="DUMBU"]<- "Misaje"
Em_edgelist$X1[Em_edgelist$X1=="TELLO"]<- "Belel"
Em_edgelist$X1[Em_edgelist$X1=="GOP_REY"]<- "Tchollire"
Em_edgelist$X1[Em_edgelist$X1=="BOGO"]<- "Bogo"
Em_edgelist$X1[Em_edgelist$X1=="ADOUMRI"]<- "Bibemi"
Em_edgelist$X1[Em_edgelist$X1=="ACHA_TUGI"]<- "Mbengwi"

#**********************
# do the same for X2
#**********************
Em_edgelist$X2[Em_edgelist$X2=="TELLO"]<- "Banyo"
Em_edgelist$X2[Em_edgelist$X2=="BAMENDA"]<- "Bamenda I"
Em_edgelist$X2[Em_edgelist$X2=="FOUMBAN"]<- "Foumban"
Em_edgelist$X2[Em_edgelist$X2=="DUMBU"]<- "Misaje"
Em_edgelist$X2[Em_edgelist$X2=="BINKA"]<- "Nkambe"
Em_edgelist$X2[Em_edgelist$X2=="BINSHUA"]<- "Nkambe"
Em_edgelist$X2[Em_edgelist$X2=="FUNDONG"]<- "Fundong"
Em_edgelist$X2[Em_edgelist$X2=="KIMBI"]<- "Bum"
Em_edgelist$X2[Em_edgelist$X2=="KONENE"]<- "Bum"
Em_edgelist$X2[Em_edgelist$X2=="MISAJE"]<- "Misaje"
Em_edgelist$X2[Em_edgelist$X2=="TAKIJA"]<- "Nkum"
Em_edgelist$X2[Em_edgelist$X2=="MBANTI_KATARKO"]<- "Banyo"
Em_edgelist$X2[Em_edgelist$X2=="SAMBA"]<- "Tibati"
Em_edgelist$X2[Em_edgelist$X2=="MARGOL"]<- "Banyo"
Em_edgelist$X2[Em_edgelist$X2=="KOGNOLI"]<- "Nyambaka"
Em_edgelist$X2[Em_edgelist$X2=="TOURNINGAL"]<- "Belel"
Em_edgelist$X2[Em_edgelist$X2=="DIBI"]<- "Nyambaka"
Em_edgelist$X2[Em_edgelist$X2=="NYAMBAKA"]<- "Nyambaka"
Em_edgelist$X2[Em_edgelist$X2=="DANG"]<- "Ngaoundere III"
Em_edgelist$X2[Em_edgelist$X2=="GARGA"]<- "Meiganga"
Em_edgelist$X2[Em_edgelist$X2=="DJALINGO"]<- "Ngaoundere II"
Em_edgelist$X2[Em_edgelist$X2=="GALDI"]<- "Nyambaka"
Em_edgelist$X2[Em_edgelist$X2=="BAFANG"]<- "Bafang"
Em_edgelist$X2[Em_edgelist$X2=="NGAOUNDERE"]<- "Ngaoundere I"
Em_edgelist$X2[Em_edgelist$X2=="BANYO"]<- "Banyo"
Em_edgelist$X2[Em_edgelist$X2=="BAFOUSSAM"]<- "Bafoussam"
Em_edgelist$X2[Em_edgelist$X2=="DCHANG"]<- "Bafoussam"
Em_edgelist$X2[Em_edgelist$X2=="KUMBA"]<- "Dchang"
Em_edgelist$X2[Em_edgelist$X2=="MEIGANGA"]<- "Meiganga"
Em_edgelist$X2[Em_edgelist$X2=="NKONSAMBA"]<- "Nkongsamba"
Em_edgelist$X2[Em_edgelist$X2=="SUBUM"]<- "Bum"
Em_edgelist$X2[Em_edgelist$X2=="ADOUMRI"]<- "Bibemi"
Em_edgelist$X2[Em_edgelist$X2=="MBANG_FOULBE"]<- "Ngan-ha"
Em_edgelist$X2[Em_edgelist$X2=="BELO"]<- "Belo"
Em_edgelist$X2[Em_edgelist$X2=="ACHA_TUGI"]<- "Mbengwi"


## This component renames some done to rescale the data to subdivision as described in the manuscript
# dp_bf<-Em_edgelist[Em_edgelist$X2== "Bafang",]
# dp_bfs<-Em_edgelist[Em_edgelist$X2== "Bafang",]
# dp_dsh<-Em_edgelist[Em_edgelist$X2== "Dchang",]
# dp_bf2<-Em_edgelist[Em_edgelist$X1== "Bafang",]

removeThose <- c("Bafang", "Dchang" , "Bafoussam","Tibati","Meiganga","Nkongsamba","Foumban")
rowToKeep <- !(Em_edgelist$X1 %in% removeThose | Em_edgelist$X2 %in% removeThose)
Em_edgelist2 <- Em_edgelist[rowToKeep, ]
# visualize the network in igraph
ss <- subset(Em_edgelist2, X1 == X2)
Nsam_loc <- subset(Em_edgelist2, !rownames(Em_edgelist2) %in% rownames(ss))

Em_edgelist2 <-Nsam_loc
Em_edgelist2 <- Em_edgelist2[!duplicated(Em_edgelist2[1:2]),]

Em_edgelist2<-as.matrix(Em_edgelist2)
gve=graph.edgelist(Em_edgelist2,directed=T)
plot(gve)

## Overlay the emperical 
Em_edgelist2<- as.data.frame(Em_edgelist2)
Em_edgelist2$X1<- as.character(Em_edgelist2$X1)
Em_edgelist2$X2<- as.character(Em_edgelist2$X2)
ss<- subset(Em_edgelist2,X1 == X2)

Nsam_loc=subset(Em_edgelist2,!rownames(Em_edgelist2) %in% rownames(ss))

Em_edgelist2<-Nsam_loc
Em_edgelist2<-Em_edgelist2[!duplicated(Em_edgelist2[1:2]),]
Emperical.data<-Em_edgelist2

Emperical.data$Dep_lat <- NA
Emperical.data$Dep_lon <- NA
Emperical.data$Dest_lat <- NA
Emperical.data$Dest_lon <- NA

Emperical.data$X1<-as.character(Emperical.data$X1)
Emperical.data$X2<- as.character(Emperical.data$X2)
# Issue here with a difference in cases
Emperical.data$Dep_lat<-as.numeric(Movement_matrix_data3[match(Emperical.data$X1,Movement_matrix_data3$Dep),]$Dep_lat)
Emperical.data$Dep_lon<-as.numeric(Movement_matrix_data3[match(Emperical.data$X1,Movement_matrix_data3$Dep),]$Dep_lon)
Emperical.data$Dest_lat<-as.numeric(Movement_matrix_data3[match(Emperical.data$X2,Movement_matrix_data3$Dest),]$Dest_lat)
Emperical.data$Dest_lon<-as.numeric(Movement_matrix_data3[match(Emperical.data$X2,Movement_matrix_data3$Dest),]$Dest_lon)
Emperical.data$Dep_lat[Emperical.data$X1== "Nkum"]<- 6.270548
Emperical.data$Dest_lat[Emperical.data$X2== "Nkum"]<- 6.270548
Emperical.data$Dep_lon[Emperical.data$X1== "Nkum"]<- 10.72352
Emperical.data$Dest_lon[Emperical.data$X2== "Nkum"]<- 10.72352

Emperical.data$Dep_lat[Emperical.data$X1== "Bamenda I"]<- 5.7965323
Emperical.data$Dest_lat[Emperical.data$X2== "Bamenda I"]<- 5.7965323
Emperical.data$Dep_lon[Emperical.data$X1== "Bamenda I"]<- 10.1608944
Emperical.data$Dest_lon[Emperical.data$X2== "Bamenda I"]<- 10.1608944


Emperical.data$Dep_lat[Emperical.data$X1== "Nkambe"]<- 6.217137
Emperical.data$Dest_lat[Emperical.data$X2== "Nkambe"]<- 6.217137
Emperical.data$Dep_lon[Emperical.data$X1== "Nkambe"]<- 10.67821
Emperical.data$Dest_lon[Emperical.data$X2== "Nkambe"]<- 10.67821

Emperical.data$Dep_lat[Emperical.data$X1== "Banyo"]<- 6.3499923
Emperical.data$Dep_lon[Emperical.data$X1== "Banyo"]<- 11.5666667

Emperical.data$Dest_lat[Emperical.data$X2== "Banyo"]<- 6.3499923
Emperical.data$Dest_lon[Emperical.data$X2== "Banyo"]<- 11.5666667

Emperical.data$Dep_lat[Emperical.data$X1== "Ngan-ha"]<- 7.432174
Emperical.data$Dep_lon[Emperical.data$X1== "Ngan-ha"]<- 12.80299
Emperical.data$Dest_lat[Emperical.data$X2== "Ngan-ha"]<- 7.432174
Emperical.data$Dest_lon[Emperical.data$X2== "Ngan-ha"]<- 12.80299

Emperical.data$Dest_lat[Emperical.data$X2== "Bibemi"]<- 9.310848
Emperical.data$Dest_lon[Emperical.data$X2== "Bibemi"]<- 13.87573

Emperical.data$Dep_lat[Emperical.data$X1== "Ngaoundere I"]<- 9.025347
Emperical.data$Dep_lon[Emperical.data$X1== "Ngaoundere I"]<- 13.51130
Emperical.data$Dest_lat[Emperical.data$X2== "Ngaoundere I"]<- 9.025347
Emperical.data$Dest_lon[Emperical.data$X2== "Ngaoundere I"]<- 13.51130


Emperical.data$Dep_lat[Emperical.data$X1== "Ngaoundere III"]<- 7.347795
Emperical.data$Dep_lon[Emperical.data$X1== "Ngaoundere III"]<- 13.4779485
Emperical.data$Dest_lat[Emperical.data$X2== "Ngaoundere III"]<- 7.347795
Emperical.data$Dest_lon[Emperical.data$X2== "Ngaoundere III"]<- 13.4779485

Emperical.data$Dep_lat[Emperical.data$X1== "Bum"]<- 5.998111
Emperical.data$Dep_lon[Emperical.data$X1== "Bum"]<- 10.179016
Emperical.data$Dest_lat[Emperical.data$X2== "Bum"]<- 5.998111
Emperical.data$Dest_lon[Emperical.data$X2== "Bum"]<- 10.179016

Emperical.data$Dep_lat[Emperical.data$X1== "Nyambaka"]<- 6.8916622
Emperical.data$Dep_lon[Emperical.data$X1== "Nyambaka"]<- 14.0955018
Emperical.data$Dest_lat[Emperical.data$X2== "Nyambaka"]<- 6.8916622
Emperical.data$Dest_lon[Emperical.data$X2== "Nyambaka"]<- 14.0955018

Emperical.data$Dest_lat[Emperical.data$X2== "Ngaoundere I"]<- 6.8916622
Emperical.data$Dest_lon[Emperical.data$X2== "Ngaoundere I"]<- 14.0955018
Emperical.data$Dep_lat[Emperical.data$X1== "Ngaoundere I"]<- 6.8916622
Emperical.data$Dep_lon[Emperical.data$X1== "Ngaoundere I"]<- 14.0955018

Emperical.data$Dest_lat[Emperical.data$X2== "Belel"]<- NA
Emperical.data$Dest_lon[Emperical.data$X2== "Belel"]<- NA

## Recode the location to subdivision level
Emperical.data$Dest_lat[Emperical.data$X2== "Belel"]<- 7.0527248
Emperical.data$Dest_lon[Emperical.data$X2== "Belel"]<- 13.3149575
Emperical.data$Dep_lat[Emperical.data$X1== "Belel"]<- 7.0527248
Emperical.data$Dep_lon[Emperical.data$X1== "Belel"]<- 13.3149575

Emperical.data$Dep_lon<-as.numeric(Emperical.data$Dep_lon)
Emperical.data$Dep_lat<-as.numeric(Emperical.data$Dep_lat)
Emperical.data$Dest_lat<-as.numeric(Emperical.data$Dest_lat)
Emperical.data$Dest_lon<-as.numeric(Emperical.data$Dest_lon)

##--------------------------------------------------------------------END---------------------------------------------------------------------------------------------Section A2


                                                ###********  ********** ********** ***
                                                 ### GRAVITY NETWORK DERIVATION 
                                                ###********  ********** ********** ***

##--------------------------------------------------------------------START---------------------------------------------------------------------------------------------Section A3


#Over lay the gravity model(Remember to convert datasets into csv files where needed for this code)
# new_grav_dat<-read.csv("DBSX3",sep=",", header=T)############### This is in DBSX3/Sheetname=DBSX3_Generate_20_node
new_grav_dat <- livsk_2005[complete.cases(livsk_2005),]
new_grav_dat <- new_grav_dat[c(-17, -36),]#(removed bertoua and mandijou) ### to maintain commom nodes between datasets
# table(new_grav_dat$Subdivision %in% humn_2005$capital)


### To have all the data at sub division level, Jakiri is recoded to represent Nkum

removeThose_g <- c("Akwaya","Mayo Baleo","Bafang","Bafut","Bafoussam II","Mandjou","Ngaoundal","Bali","Bamkim","Mbandjok","Bangourain","Buea","Dchang","Dir","Djohong","Douala II","Tignere","Ebolowa I","Nwa","Fungom","Foumban","Meiganga","Ekondotiti","Kribi I","Kumba II","Kye-ossi","Noni","Martap","Limbe I","Mbven","Minta","Ndop","Ngambe Tikar","Njinekon","Nkongsamba I","Ndu","Obala","Oku","Galim Tignere","Tibati","Sangmelima","Koutaba","Yaounde III","Yoko")
rowToKeep_3 <- !(new_grav_dat$Subdivision %in% removeThose_g)
new_grav_dat <- new_grav_dat[rowToKeep_3, ]
new_grav_dat$Subdivision<-as.character(new_grav_dat$Subdivision)
new_grav_dat$Subdivision[new_grav_dat$Subdivision== "Jakiri"]<-"Nkum"

#Using new census data----
CAM_g_model_new <- matrix(data = NA, nrow = length(new_grav_dat$Subdivision), ncol = length(new_grav_dat$Subdivision), dimnames = list(new_grav_dat$Subdivision,new_grav_dat$Subdivision))

for (i in 1:length(new_grav_dat$Subdivision)){
  for (j in 1:length(new_grav_dat$Subdivision)){
    CAM_g_model_new[i,j] <- ((new_grav_dat$cattle_per_person[i]*new_grav_dat$cattle_per_person[j])/(pointDistance(c(new_grav_dat$LATS[i], new_grav_dat$LONGS[i]), c(new_grav_dat$LATS[j], new_grav_dat$LONGS[j]), lonlat=FALSE)))
  }
}


### Optimisation for determing the threshold (AUC) For movement in the gravity model

# View(CAM_g_model_new)
diag(CAM_g_model_new) <- 0 ## convert the diagnal inf to 0
c(CAM_g_model_new)## Vector
length(c(CAM_g_model_new))

#normalising the contact network optain by gravity model
CAM_g_model_new2 <- vector()
for(ii in 1:20){ CAM_g_model_new2 <- rbind(CAM_g_model_new2, CAM_g_model_new[ii,]/rowSums(CAM_g_model_new)[ii])}

### gve is the igraph object representing the emperical network
matem=get.adjacency(gve)## extract the contacts from it
matem
as.matrix(matem)
c(as.matrix(matem))
length(c(CAM_g_model_new2))
length(c(as.matrix(matem)))
dattest=as.data.frame(cbind(truth=c(as.matrix(matem)) , pred=c(CAM_g_model_new2)))
auc(truth~pred,data=dattest)

## nodes in the emperical network
#===========================
CAM_g_model_new3 <- matrix(data= NA, nrow=length(new_grav_dat$Subdivision), ncol=length(new_grav_dat$Subdivision), dimnames=list(new_grav_dat$Subdivision,new_grav_dat$Subdivision))

#function to calculate the gravity model output
#data requirements: H,A,lat,lon
calc_grav <- function(data,from,to){
  names(data) <- c("nH","nA","lat","lon")
  #calculate the squared distance
  distval <- (pointDistance(c(data$lat[from], data$lon[from]), c(data$lat[to], data$lon[to]), lonlat=T)  ) +1 # Change this to  square parameter and update the threshold selection to the new values
   distval <- distval / 100000 # Included to transform for the quesrt values
#   print(distval)
  #calculate the gravitation force of from
  grav.from= as.numeric(data$nA[from])
  #calculate the gravitation force of to
  grav.to= as.numeric(data$nH[to])
  #calculate weight
  outval <- (grav.from * grav.to) / (distval ^ 2)
  return(outval)
}

maxV <- 0
tested_thresholds <- seq(0, 5, 0.1)
rval<- vector()
for(kk in 0.010^tested_thresholds){
# for(kk in tested_thresholds){
    
    CAM_g_model_new3 <- matrix(NA,ncol=length(new_grav_dat$Subdivision), nrow=length(new_grav_dat$Subdivision))
  for (i in 1:length(new_grav_dat$Subdivision)){
    for (j in 1:length(new_grav_dat$Subdivision)){  
      CAM_g_model_new3[i,j] <- calc_grav(new_grav_dat[,c("Subdivision_human_pop_num","Subdivision_cattle_pop_num","LATS","LONGS")], i, j)
      if(is.na(CAM_g_model_new3[i,j]) ) message("from: ", i,", to: ", j,", val: ",CAM_g_model_new3[i,j])
    }
  }
  #names
  colnames(CAM_g_model_new3) = rownames(CAM_g_model_new3) = new_grav_dat$Subdivision
  
  #match matrix order with that of empirical
  CAM_g_model_new3 = CAM_g_model_new3[rownames(matem),colnames(matem)]
  
  #normalise the matrix::: this generatates the proportion of force to each node
  norm_mat <- vector(); for(ii in 1:20){ norm_mat <- rbind(norm_mat, CAM_g_model_new3[ii,]/rowSums(CAM_g_model_new3)[ii])}
  attr.dat=new_grav_dat
  rownames(attr.dat) = attr.dat$Subdivision
  norm_mat <<- norm_mat * attr.dat[rownames(matem), "Subdivision_cattle_pop_num"] # this define the number of animals from the node to the rest of the nodes
  #dichotomising the matrix
 # print(norm_mat)
  diag(norm_mat)<-0
  norm_mat[norm_mat<kk] <- 0
  norm_mat[norm_mat>0] <- 1
  #remove the within market movements
  #calculate the auc
  dattest=as.data.frame(cbind(truth=c(as.matrix(matem)) , pred=c(norm_mat)))
  auc_val = auc(truth~pred,data=dattest)
  rval <- rbind(rval, c(kk,auc_val))
}

plot(rval[,1],rval[,2],type="l", xlab="threshold value",ylab="auc",log="x")

rval[which.max(rval[,2]),1]

thresh = rval[which.max(rval[,2]),1] # minimum of 1.25 animals define a movement
save(thresh, file = "..//RImages//GravityThreshold.RData")

CAM_g_model_new3 <- matrix(NA,ncol=length(new_grav_dat$Subdivision), nrow=length(new_grav_dat$Subdivision))
for (i in 1:length(new_grav_dat$Subdivision)){
  for (j in 1:length(new_grav_dat$Subdivision)){  
    CAM_g_model_new3[i,j] <- calc_grav(new_grav_dat[,c("Subdivision_human_pop_num","Subdivision_cattle_pop_num","LATS","LONGS")], i, j)
    if(is.na(CAM_g_model_new3[i,j]) ) message("from: ", i,", to: ", j,", val: ",CAM_g_model_new3[i,j])
  }
}
#names
colnames(CAM_g_model_new3) = rownames(CAM_g_model_new3) = new_grav_dat$Subdivision

#match matrix order with that of empirical
CAM_g_model_new3 = CAM_g_model_new3[rownames(matem),colnames(matem)]

#normalise the matrix
norm_mat <- vector(); for(ii in 1:20){ norm_mat <- rbind(norm_mat, CAM_g_model_new3[ii,]/rowSums(CAM_g_model_new3)[ii])}
attr.dat=new_grav_dat
rownames(attr.dat) = attr.dat$Subdivision
norm_mat = norm_mat * attr.dat[rownames(matem),"Subdivision_cattle_pop_num"]
#dichotomising the matrix
norm_mat[norm_mat<thresh] <- 0
norm_mat[norm_mat>0] <- 1
diag(norm_mat)<-0 ## remove movements at the same point..

gv2<- graph.adjacency(norm_mat,mode="directed",weighted=TRUE)
plot(gv2)
plot.igraph(gv2,layout=layout.fruchterman.reingold,edge.curved=0.2, edge.arrow.size=0.3) #


Gravity.data<-melt(norm_mat)
Gravity.data$X1[Gravity.data$X1== 1]<- "Bogo"
Gravity.data$X1[Gravity.data$X1== 2]<- "Bibemi"
Gravity.data$X1[Gravity.data$X1== 3]<- "Mbengwi"
Gravity.data$X1[Gravity.data$X1== 4]<- "Bamenda I"
Gravity.data$X1[Gravity.data$X1== 5]<- "Nkambe"
Gravity.data$X1[Gravity.data$X1== 6]<- "Misaje"
Gravity.data$X1[Gravity.data$X1== 7]<- "Fundong"
Gravity.data$X1[Gravity.data$X1== 8]<- "Bum"
Gravity.data$X1[Gravity.data$X1== 9]<- "Nkum"
Gravity.data$X1[Gravity.data$X1== 10]<- "Wum"
Gravity.data$X1[Gravity.data$X1== 11]<- "Banyo"
Gravity.data$X1[Gravity.data$X1== 12]<- "Ngaoundere I"
Gravity.data$X1[Gravity.data$X1== 13]<- "Belo"
Gravity.data$X1[Gravity.data$X1== 14]<- "Ngaoundere III"
Gravity.data$X1[Gravity.data$X1== 15]<- "Ngan-ha"
Gravity.data$X1[Gravity.data$X1== 16]<- "Tchollire"
Gravity.data$X1[Gravity.data$X1== 17]<- "Ngaoundere II"
Gravity.data$X1[Gravity.data$X1== 18]<- "Belel"
Gravity.data$X1[Gravity.data$X1== 19]<- "Nyambaka"
Gravity.data$X1[Gravity.data$X1== 20]<- "Touboro"

st<-subset(Gravity.data,X1==X2)
Nsam_g_loc=subset(Gravity.data,!rownames(Gravity.data) %in% rownames(st))
Gravity.data<-Nsam_g_loc
Gravity.mov.data<- subset(Gravity.data,value>0)
Gravity.mov.data<-Gravity.mov.data[,1:2]

Gravity.mov.data$Dep_lat<-NA
Gravity.mov.data$Dep_lon<-NA
Gravity.mov.data$Dest_lat<-NA
Gravity.mov.data$Dest_lon<-NA
Gravity.mov.data$X1<-as.character(Gravity.mov.data$X1)
Gravity.mov.data$X2<-as.character(Gravity.mov.data$X2)

Gravity.mov.data$Dep_lat<-as.numeric(Nmov_loc.data[match(Gravity.mov.data$X1,Nmov_loc.data$Dep),]$Dep_lat)
Gravity.mov.data$Dep_lon<-as.numeric(Nmov_loc.data[match(Gravity.mov.data$X1,Nmov_loc.data$Dep),]$Dep_lon)
Gravity.mov.data$Dest_lat<-as.numeric(Nmov_loc.data[match(Gravity.mov.data$X2,Nmov_loc.data$Dest),]$Dest_lat)
Gravity.mov.data$Dest_lon<-as.numeric(Nmov_loc.data[match(Gravity.mov.data$X2,Nmov_loc.data$Dest),]$Dest_lon)

Gravity.mov.data$Dep_lat<-as.numeric(Emperical.data[match(Gravity.mov.data$X1,Emperical.data$X1),]$Dep_lat)
Gravity.mov.data$Dep_lon<-as.numeric(Emperical.data[match(Gravity.mov.data$X1,Emperical.data$X1),]$Dep_lon)
Gravity.mov.data$Dest_lat<-as.numeric(Emperical.data[match(Gravity.mov.data$X2,Emperical.data$X2),]$Dest_lat)
Gravity.mov.data$Dest_lon<-as.numeric(Emperical.data[match(Gravity.mov.data$X2,Emperical.data$X2),]$Dest_lon)

Gravity.mov.data$Dest_lat[Gravity.mov.data$X2== "Tchollire"]<-"8.400121"
Gravity.mov.data$Dest_lon[Gravity.mov.data$X2== "Tchollire"]<-"14.17352"

Gravity.mov.data$Dest_lat[Gravity.mov.data$X2== "Touboro"]<-"7.770738"
Gravity.mov.data$Dest_lon[Gravity.mov.data$X2== "Touboro"]<-"15.35232"

Gravity.mov.data$Dep_lon<-as.numeric(Gravity.mov.data$Dep_lon)
Gravity.mov.data$Dep_lat<-as.numeric(Gravity.mov.data$Dep_lat)
Gravity.mov.data$Dest_lat<-as.numeric(Gravity.mov.data$Dest_lat)
Gravity.mov.data$Dest_lon<-as.numeric(Gravity.mov.data$Dest_lon)

dim(Gravity.mov.data)

Gravity.mov.data### FOR PLOTTING

gv2 ### the  network
Graviplot<-plot.igraph(gv2,layout=layout.fruchterman.reingold,edge.curved=0.2, edge.arrow.size=0.3) #

##--------------------------------------------------------------------End---------------------------------------------------------------------------------------------Section A3


 ### PLOTING ON THE MAP
Nmov_loc.data   ## This is the network for ploting


                                                                         ##******************************************
                                                                          ## Section B-- NETWORK TOPOLOGY COMPARISON
                                                                          ##******************************************
                                                                          
##--------------------------------------------------------------------START---------------------------------------------------------------------------------------------Section B


gv2 ### the  network gravity

save(gv2, file = "..//RImages//GravityObject.RData")

gve ### the  emperical network 
Emperiplot<-plot.igraph(gve,layout=layout.fruchterman.reingold,edge.curved=0.2, edge.arrow.size=0.3) #



## NETWORK PARAMETERS
#**************************************************
##### ANALYSING MEASURES OF CENTRALITY OF ALL NETWORKS
#*****************************************************
library(igraph)  
# 
 mol_net2 <- read.csv("Edge_list_molecular_3_edit.csv")
 mol_net2 <- Nmov_loc.data[,1:2,]
 
 #mol_net2<-mol_net2[,-1]
# mol_net1 <- read.csv("Molecular_extended_mv_mat.csv")
# grav_net1 <- read.csv("Gravity_mv_mat_20.csv")
# modified_grav<-read.csv("Edge_list_gravity_model.csv")
# modified_grav<-as.matrix(modified_grav)
 mol_net2 <- as.matrix(mol_net2) # same thing for mol_net2
###********************************************
##### mol_net2 and grav_net1_edge, Em_edgelist rand.g are from the epidemic simulations
#**********************************************
#g1=graph.edgelist(Em_edgelist2,directed=T)
g2 = graph.edgelist(mol_net2, directed=T)

save(g2, file = "..//RImages//MolecularObject.RData")

# g4 <- graph.edgelist(grav_net1_edge,directed=T)
Molecplot <- plot.igraph(g2, layout=layout.fruchterman.reingold, edge.curved=0.2, edge.arrow.size=0.3) #

rand.g=list()
for (i in 1:1000){
  rand.g[[i]] <- get.edgelist(erdos.renyi.game(20,43, type='gnm', directed=TRUE)) # set the num of nodes and edges you want = has to = to the empirical net I guess
  #rand.g <- get.adjacency(erdos.renyi.game(20,43, type='gnm', directed=TRUE))
  #deg[,i] <- betweenness(rand.g,v=V(rand.g),directed =TRUE, weights= NA)  # gets the avg. cc for each of the generated nets and puts them in the matrix created before
}


rand.M=list()
for (i in 1:1000){
  rand.M[[i]] <- get.edgelist(erdos.renyi.game(20,53, type='gnm', directed=TRUE)) # set the num of nodes and edges you want = has to = to the empirical net I guess
  #rand.g <- get.adjacency(erdos.renyi.game(20,43, type='gnm', directed=TRUE))
  #deg[,i] <- betweenness(rand.g,v=V(rand.g),directed =TRUE, weights= NA)  # gets the avg. cc for each of the generated nets and puts them in the matrix created before
}

rand.G=list()
for (i in 1:1000){
  rand.G[[i]] <- get.edgelist(erdos.renyi.game(20,134, type='gnm', directed=TRUE)) # set the num of nodes and edges you want = has to = to the empirical net I guess
  #rand.g <- get.adjacency(erdos.renyi.game(20,43, type='gnm', directed=TRUE))
  #deg[,i] <- betweenness(rand.g,v=V(rand.g),directed =TRUE, weights= NA)  # gets the avg. cc for each of the generated nets and puts them in the matrix created before
}


g3 <- rand.g[2]
g4<-rand.g
g5<-rand.M[2]
g6<-rand.G[2]

Random.data<- as.data.frame(rand.g[2])
Random.data$X1[Random.data$X1== 1]<- "Bogo"
Random.data$X1[Random.data$X1== 2]<- "Bibemi"
Random.data$X1[Random.data$X1== 3]<- "Mbengwi"
Random.data$X1[Random.data$X1== 4]<- "Bamenda I"
Random.data$X1[Random.data$X1== 5]<- "Nkambe"
Random.data$X1[Random.data$X1== 6]<- "Misaje"
Random.data$X1[Random.data$X1== 7]<- "Fundong"
Random.data$X1[Random.data$X1== 8]<- "Bum"
Random.data$X1[Random.data$X1== 9]<- "Nkum"
Random.data$X1[Random.data$X1== 10]<- "Wum"
Random.data$X1[Random.data$X1== 11]<- "Banyo"
Random.data$X1[Random.data$X1== 12]<- "Ngaoundere I"
Random.data$X1[Random.data$X1== 13]<- "Belo"
Random.data$X1[Random.data$X1== 14]<- "Ngaoundere III"
Random.data$X1[Random.data$X1== 15]<- "Ngan-ha"
Random.data$X1[Random.data$X1== 16]<- "Tchollire"
Random.data$X1[Random.data$X1== 17]<- "Ngaoundere II"
Random.data$X1[Random.data$X1== 18]<- "Belel"
Random.data$X1[Random.data$X1== 19]<- "Nyambaka"
Random.data$X1[Random.data$X1== 20]<- "Touboro"

Random.data$X2[Random.data$X2== 1]<- "Bogo"
Random.data$X2[Random.data$X2== 2]<- "Bibemi"
Random.data$X2[Random.data$X2== 3]<- "Mbengwi"
Random.data$X2[Random.data$X2== 4]<- "Bamenda I"
Random.data$X2[Random.data$X2== 5]<- "Nkambe"
Random.data$X2[Random.data$X2== 6]<- "Misaje"
Random.data$X2[Random.data$X2== 7]<- "Fundong"
Random.data$X2[Random.data$X2== 8]<- "Bum"
Random.data$X2[Random.data$X2== 9]<- "Nkum"
Random.data$X2[Random.data$X2== 10]<- "Wum"
Random.data$X2[Random.data$X2== 11]<- "Banyo"
Random.data$X2[Random.data$X2== 12]<- "Ngaoundere I"
Random.data$X2[Random.data$X2== 13]<- "Belo"
Random.data$X2[Random.data$X2== 14]<- "Ngaoundere III"
Random.data$X2[Random.data$X2== 15]<- "Ngan-ha"
Random.data$X2[Random.data$X2== 16]<- "Tchollire"
Random.data$X2[Random.data$X2== 17]<- "Ngaoundere II"
Random.data$X2[Random.data$X2== 18]<- "Belel"
Random.data$X2[Random.data$X2== 19]<- "Nyambaka"
Random.data$X2[Random.data$X2== 20]<- "Touboro"

Random.data_M<- as.data.frame(rand.M[2])
Random.data_M$X1[Random.data_M$X1== 1]<- "Bogo"
Random.data_M$X1[Random.data_M$X1== 2]<- "Bibemi"
Random.data_M$X1[Random.data_M$X1== 3]<- "Mbengwi"
Random.data_M$X1[Random.data_M$X1== 4]<- "Bamenda I"
Random.data_M$X1[Random.data_M$X1== 5]<- "Nkambe"
Random.data_M$X1[Random.data_M$X1== 6]<- "Misaje"
Random.data_M$X1[Random.data_M$X1== 7]<- "Fundong"
Random.data_M$X1[Random.data_M$X1== 8]<- "Bum"
Random.data_M$X1[Random.data_M$X1== 9]<- "Nkum"
Random.data_M$X1[Random.data_M$X1== 10]<- "Wum"
Random.data_M$X1[Random.data_M$X1== 11]<- "Banyo"
Random.data_M$X1[Random.data_M$X1== 12]<- "Ngaoundere I"
Random.data_M$X1[Random.data_M$X1== 13]<- "Belo"
Random.data_M$X1[Random.data_M$X1== 14]<- "Ngaoundere III"
Random.data_M$X1[Random.data_M$X1== 15]<- "Ngan-ha"
Random.data_M$X1[Random.data_M$X1== 16]<- "Tchollire"
Random.data_M$X1[Random.data_M$X1== 17]<- "Ngaoundere II"
Random.data_M$X1[Random.data_M$X1== 18]<- "Belel"
Random.data_M$X1[Random.data_M$X1== 19]<- "Nyambaka"
Random.data_M$X1[Random.data_M$X1== 20]<- "Touboro"

Random.data_M$X2[Random.data_M$X2== 1]<- "Bogo"
Random.data_M$X2[Random.data_M$X2== 2]<- "Bibemi"
Random.data_M$X2[Random.data_M$X2== 3]<- "Mbengwi"
Random.data_M$X2[Random.data_M$X2== 4]<- "Bamenda I"
Random.data_M$X2[Random.data_M$X2== 5]<- "Nkambe"
Random.data_M$X2[Random.data_M$X2== 6]<- "Misaje"
Random.data_M$X2[Random.data_M$X2== 7]<- "Fundong"
Random.data_M$X2[Random.data_M$X2== 8]<- "Bum"
Random.data_M$X2[Random.data_M$X2== 9]<- "Nkum"
Random.data_M$X2[Random.data_M$X2== 10]<- "Wum"
Random.data_M$X2[Random.data_M$X2== 11]<- "Banyo"
Random.data_M$X2[Random.data_M$X2== 12]<- "Ngaoundere I"
Random.data_M$X2[Random.data_M$X2== 13]<- "Belo"
Random.data_M$X2[Random.data_M$X2== 14]<- "Ngaoundere III"
Random.data_M$X2[Random.data_M$X2== 15]<- "Ngan-ha"
Random.data_M$X2[Random.data_M$X2== 16]<- "Tchollire"
Random.data_M$X2[Random.data_M$X2== 17]<- "Ngaoundere II"
Random.data_M$X2[Random.data_M$X2== 18]<- "Belel"
Random.data_M$X2[Random.data_M$X2== 19]<- "Nyambaka"
Random.data_M$X2[Random.data_M$X2== 20]<- "Touboro"

Random.data_G<- as.data.frame(rand.G[2])
Random.data_G$X1[Random.data_G$X1== 1]<- "Bogo"
Random.data_G$X1[Random.data_G$X1== 2]<- "Bibemi"
Random.data_G$X1[Random.data_G$X1== 3]<- "Mbengwi"
Random.data_G$X1[Random.data_G$X1== 4]<- "Bamenda I"
Random.data_G$X1[Random.data_G$X1== 5]<- "Nkambe"
Random.data_G$X1[Random.data_G$X1== 6]<- "Misaje"
Random.data_G$X1[Random.data_G$X1== 7]<- "Fundong"
Random.data_G$X1[Random.data_G$X1== 8]<- "Bum"
Random.data_G$X1[Random.data_G$X1== 9]<- "Nkum"
Random.data_G$X1[Random.data_G$X1== 10]<- "Wum"
Random.data_G$X1[Random.data_G$X1== 11]<- "Banyo"
Random.data_G$X1[Random.data_G$X1== 12]<- "Ngaoundere I"
Random.data_G$X1[Random.data_G$X1== 13]<- "Belo"
Random.data_G$X1[Random.data_G$X1== 14]<- "Ngaoundere III"
Random.data_G$X1[Random.data_G$X1== 15]<- "Ngan-ha"
Random.data_G$X1[Random.data_G$X1== 16]<- "Tchollire"
Random.data_G$X1[Random.data_G$X1== 17]<- "Ngaoundere II"
Random.data_G$X1[Random.data_G$X1== 18]<- "Belel"
Random.data_G$X1[Random.data_G$X1== 19]<- "Nyambaka"
Random.data_G$X1[Random.data_G$X1== 20]<- "Touboro"

Random.data_G$X2[Random.data_G$X2== 1]<- "Bogo"
Random.data_G$X2[Random.data_G$X2== 2]<- "Bibemi"
Random.data_G$X2[Random.data_G$X2== 3]<- "Mbengwi"
Random.data_G$X2[Random.data_G$X2== 4]<- "Bamenda I"
Random.data_G$X2[Random.data_G$X2== 5]<- "Nkambe"
Random.data_G$X2[Random.data_G$X2== 6]<- "Misaje"
Random.data_G$X2[Random.data_G$X2== 7]<- "Fundong"
Random.data_G$X2[Random.data_G$X2== 8]<- "Bum"
Random.data_G$X2[Random.data_G$X2== 9]<- "Nkum"
Random.data_G$X2[Random.data_G$X2== 10]<- "Wum"
Random.data_G$X2[Random.data_G$X2== 11]<- "Banyo"
Random.data_G$X2[Random.data_G$X2== 12]<- "Ngaoundere I"
Random.data_G$X2[Random.data_G$X2== 13]<- "Belo"
Random.data_G$X2[Random.data_G$X2== 14]<- "Ngaoundere III"
Random.data_G$X2[Random.data_G$X2== 15]<- "Ngan-ha"
Random.data_G$X2[Random.data_G$X2== 16]<- "Tchollire"
Random.data_G$X2[Random.data_G$X2== 17]<- "Ngaoundere II"
Random.data_G$X2[Random.data_G$X2== 18]<- "Belel"
Random.data_G$X2[Random.data_G$X2== 19]<- "Nyambaka"
Random.data_G$X2[Random.data_G$X2== 20]<- "Touboro"



Random.data_db<- Random.data
Random.data_Mdb<- Random.data_M
Random.data_Gdb<- Random.data_G


Random.data_db$Dep_lat<- Gravity.mov.data[match(Random.data_db$X1,Gravity.mov.data$X1),]$Dep_lat
Random.data_db$Dep_lon<- Gravity.mov.data[match(Random.data_db$X1,Gravity.mov.data$X1),]$Dep_lon
Random.data_db$Dest_lat<-Gravity.mov.data[match(Random.data_db$X2,Gravity.mov.data$X2),]$Dest_lat
Random.data_db$Dest_lon<-Gravity.mov.data[match(Random.data_db$X2,Gravity.mov.data$X2),]$Dest_lon

Random.data_Mdb$Dep_lat<- Gravity.mov.data[match(Random.data_Mdb$X1,Gravity.mov.data$X1),]$Dep_lat
Random.data_Mdb$Dep_lon<- Gravity.mov.data[match(Random.data_Mdb$X1,Gravity.mov.data$X1),]$Dep_lon
Random.data_Mdb$Dest_lat<-Gravity.mov.data[match(Random.data_Mdb$X2,Gravity.mov.data$X2),]$Dest_lat
Random.data_Mdb$Dest_lon<-Gravity.mov.data[match(Random.data_Mdb$X2,Gravity.mov.data$X2),]$Dest_lon


Random.data_Gdb$Dep_lat<- Gravity.mov.data[match(Random.data_Gdb$X1,Gravity.mov.data$X1),]$Dep_lat
Random.data_Gdb$Dep_lon<- Gravity.mov.data[match(Random.data_Gdb$X1,Gravity.mov.data$X1),]$Dep_lon
Random.data_Gdb$Dest_lat<-Gravity.mov.data[match(Random.data_Gdb$X2,Gravity.mov.data$X2),]$Dest_lat
Random.data_Gdb$Dest_lon<-Gravity.mov.data[match(Random.data_Gdb$X2,Gravity.mov.data$X2),]$Dest_lon


Random.data_db$Dest_lat[Random.data_db$X2== "Bogo"]<-"10.735"
Random.data_db$Dest_lon[Random.data_db$X2== "Bogo"]<-"14.594"
Random.data_db$Dep_lat[Random.data_db$X1== "Bogo"]<-"10.735"
Random.data_db$Dep_lon[Random.data_db$X1== "Bogo"]<-"14.594"

Random.data_db$Dest_lat[Random.data_db$X2== "Belel"]<-"7.0527248"
Random.data_db$Dest_lon[Random.data_db$X2== "Belel"]<-"13.3149575"
Random.data_db$Dep_lat[Random.data_db$X1== "Belel"]<-"7.0527248"
Random.data_db$Dep_lon[Random.data_db$X1== "Belel"]<-"13.3149575"

Random.data_db$Dest_lat[Random.data_db$X2== "Wum"]<-"6.391707"
Random.data_db$Dest_lon[Random.data_db$X2== "Wum"]<-"10.07574"
Random.data_db$Dep_lat[Random.data_db$X1== "Wum"]<-"6.391707"
Random.data_db$Dep_lon[Random.data_db$X1== "Wum"]<-"10.07574"

Random.data_db$Dest_lat[Random.data_db$X2== "Fundong"]<-"6.280064"
Random.data_db$Dest_lon[Random.data_db$X2== "Fundong"]<-"10.28509"
Random.data_db$Dep_lat[Random.data_db$X1== "Fundong"]<-"6.280064"
Random.data_db$Dep_lon[Random.data_db$X1== "Fundong"]<-"10.28509"

Random.data_db$Dest_lat[Random.data_db$X2== "Bibemi"]<-"9.310848"
Random.data_db$Dest_lon[Random.data_db$X2== "Bibemi"]<-"13.87573"
Random.data_db$Dep_lat[Random.data_db$X1== "Bibemi"]<-"9.310848"
Random.data_db$Dep_lon[Random.data_db$X1== "Bibemi"]<-"13.87573"

Random.data_db$Dest_lat[Random.data_db$X2== "Mbengwi"]<-"6.012306"
Random.data_db$Dest_lon[Random.data_db$X2== "Mbengwi"]<-"10.02218"
Random.data_db$Dep_lat[Random.data_db$X1== "Mbengwi"]<-"6.012306"
Random.data_db$Dep_lon[Random.data_db$X1== "Mbengwi"]<-"10.02218"

Random.data_db$Dest_lat[Random.data_db$X2== "Nkum"]<-"6.270548"
Random.data_db$Dest_lon[Random.data_db$X2== "Nkum"]<-"10.72352"
Random.data_db$Dep_lat[Random.data_db$X1== "Nkum"]<-"6.270548"
Random.data_db$Dep_lon[Random.data_db$X1== "Nkum"]<-"10.72352"

Random.data_db$Dest_lat[Random.data_db$X2== "Ngaoundere I"]<-"6.891662"
Random.data_db$Dest_lon[Random.data_db$X2== "Ngaoundere I"]<-"14.09550"
Random.data_db$Dep_lat[Random.data_db$X1== "Ngaoundere I"]<-"6.891662"
Random.data_db$Dep_lon[Random.data_db$X1== "Ngaoundere I"]<-"14.09550"

Random.data_db$Dest_lat[Random.data_db$X2== "Ngan-ha"]<-"7.432174"
Random.data_db$Dest_lon[Random.data_db$X2== "Ngan-ha"]<-"12.80299"
Random.data_db$Dep_lat[Random.data_db$X1== "Ngan-ha"]<-"7.432174"
Random.data_db$Dep_lon[Random.data_db$X1== "Ngan-ha"]<-"12.80299"

Random.data_db$Dest_lat[Random.data_db$X2== "Bamenda I"]<-"5.796532"
Random.data_db$Dest_lon[Random.data_db$X2== "Bamenda I"]<-"10.16089"
Random.data_db$Dep_lat[Random.data_db$X1== "Bamenda I"]<-"5.796532"
Random.data_db$Dep_lon[Random.data_db$X1== "Bamenda I"]<-"10.16089"

Random.data_db$Dep_lat[Random.data_db$X1== "Belo"]<-"6.1367145"
Random.data_db$Dep_lon[Random.data_db$X1== "Belo"]<-"10.2526474"

Random.data_db$Dep_lat[Random.data_db$X1== "Touboro"]<- "7.770737"
Random.data_db$Dep_lon[Random.data_db$X1== "Touboro"]<- "15.33481"

# Molecular
Random.data_Mdb$Dest_lat[Random.data_Mdb$X2== "Bogo"]<-"10.735"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X2== "Bogo"]<-"14.594"
Random.data_Mdb$Dest_lat[Random.data_Mdb$X1== "Bogo"]<-"10.735"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X1== "Bogo"]<-"14.594"

Random.data_Mdb$Dest_lat[Random.data_Mdb$X2== "Belel"]<-"7.0527248"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X2== "Belel"]<-"13.3149575"
Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Belel"]<-"7.0527248"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Belel"]<-"13.3149575"

Random.data_Mdb$Dest_lat[Random.data_Mdb$X2== "Wum"]<-"6.391707"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X2== "Wum"]<-"10.07574"
Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Wum"]<-"6.391707"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Wum"]<-"10.07574"

Random.data_Mdb$Dest_lat[Random.data_Mdb$X2== "Fundong"]<-"6.280064"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X2== "Fundong"]<-"10.28509"
Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Fundong"]<-"6.280064"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Fundong"]<-"10.28509"

Random.data_Mdb$Dest_lat[Random.data_Mdb$X2== "Bibemi"]<-"9.310848"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X2== "Bibemi"]<-"13.87573"
Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Bibemi"]<-"9.310848"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Bibemi"]<-"13.87573"

Random.data_Mdb$Dest_lat[Random.data_Mdb$X2== "Mbengwi"]<-"6.012306"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X2== "Mbengwi"]<-"10.02218"
Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Mbengwi"]<-"6.012306"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Mbengwi"]<-"10.02218"

Random.data_Mdb$Dest_lat[Random.data_Mdb$X2== "Nkum"]<-"6.270548"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X2== "Nkum"]<-"10.72352"
Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Nkum"]<-"6.270548"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Nkum"]<-"10.72352"

Random.data_Mdb$Dest_lat[Random.data_Mdb$X2== "Ngaoundere I"]<-"6.891662"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X2== "Ngaoundere I"]<-"14.09550"
Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Ngaoundere I"]<-"6.891662"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Ngaoundere I"]<-"14.09550"

Random.data_Mdb$Dest_lat[Random.data_Mdb$X2== "Ngan-ha"]<-"7.432174"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X2== "Ngan-ha"]<-"12.80299"
Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Ngan-ha"]<-"7.432174"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Ngan-ha"]<-"12.80299"

Random.data_Mdb$Dest_lat[Random.data_Mdb$X2== "Bamenda I"]<-"5.796532"
Random.data_Mdb$Dest_lon[Random.data_Mdb$X2== "Bamenda I"]<-"10.16089"
Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Bamenda I"]<-"5.796532"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Bamenda I"]<-"10.16089"

Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Belo"]<-"6.1367145"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Belo"]<-"10.2526474"

Random.data_Mdb$Dep_lat[Random.data_Mdb$X1== "Touboro"]<- "7.770737"
Random.data_Mdb$Dep_lon[Random.data_Mdb$X1== "Touboro"]<- "15.33481"

### Gra_random

Random.data_Gdb$Dest_lat[Random.data_Gdb$X2== "Bogo"]<-"10.735"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X2== "Bogo"]<-"14.594"
Random.data_Gdb$Dest_lat[Random.data_Gdb$X1 == "Bogo"]<-"10.735"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X1 == "Bogo"]<-"14.594"

Random.data_Gdb$Dest_lat[Random.data_Gdb$X2== "Belel"]<-"7.0527248"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X2== "Belel"]<-"13.3149575"
Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Belel"]<-"7.0527248"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Belel"]<-"13.3149575"

Random.data_Gdb$Dest_lat[Random.data_Gdb$X2== "Wum"]<-"6.391707"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X2== "Wum"]<-"10.07574"
Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Wum"]<-"6.391707"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Wum"]<-"10.07574"

Random.data_Gdb$Dest_lat[Random.data_Gdb$X2== "Fundong"]<-"6.280064"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X2== "Fundong"]<-"10.28509"
Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Fundong"]<-"6.280064"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Fundong"]<-"10.28509"

Random.data_Gdb$Dest_lat[Random.data_Gdb$X2== "Bibemi"]<-"9.310848"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X2== "Bibemi"]<-"13.87573"
Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Bibemi"]<-"9.310848"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Bibemi"]<-"13.87573"

Random.data_Gdb$Dest_lat[Random.data_Gdb$X2== "Mbengwi"]<-"6.012306"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X2== "Mbengwi"]<-"10.02218"
Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Mbengwi"]<-"6.012306"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Mbengwi"]<-"10.02218"

Random.data_Gdb$Dest_lat[Random.data_Gdb$X2== "Nkum"]<-"6.270548"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X2== "Nkum"]<-"10.72352"
Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Nkum"]<-"6.270548"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Nkum"]<-"10.72352"

Random.data_Gdb$Dest_lat[Random.data_Gdb$X2== "Ngaoundere I"]<-"6.891662"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X2== "Ngaoundere I"]<-"14.09550"
Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Ngaoundere I"]<-"6.891662"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Ngaoundere I"]<-"14.09550"

Random.data_Gdb$Dest_lat[Random.data_Gdb$X2== "Ngan-ha"]<-"7.432174"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X2== "Ngan-ha"]<-"12.80299"
Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Ngan-ha"]<-"7.432174"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Ngan-ha"]<-"12.80299"

Random.data_Gdb$Dest_lat[Random.data_Gdb$X2== "Bamenda I"]<-"5.796532"
Random.data_Gdb$Dest_lon[Random.data_Gdb$X2== "Bamenda I"]<-"10.16089"
Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Bamenda I"]<-"5.796532"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Bamenda I"]<-"10.16089"


Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Belo"]<-"6.1367145"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Belo"]<-"10.2526474"

Random.data_Gdb$Dep_lat[Random.data_Gdb$X1== "Touboro"]<- "7.770737"
Random.data_Gdb$Dep_lon[Random.data_Gdb$X1== "Touboro"]<- "15.33481"

## Emprical_rand
Random.data_db$Dep_lon<-as.numeric(Random.data_db$Dep_lon)
Random.data_db$Dep_lat<-as.numeric(Random.data_db$Dep_lat)
Random.data_db$Dest_lat<-as.numeric(Random.data_db$Dest_lat)
Random.data_db$Dest_lon<-as.numeric(Random.data_db$Dest_lon)

## Mole_rand
Random.data_Mdb$Dep_lon<-as.numeric(Random.data_Mdb$Dep_lon)
Random.data_Mdb$Dep_lat<-as.numeric(Random.data_Mdb$Dep_lat)
Random.data_Mdb$Dest_lat<-as.numeric(Random.data_Mdb$Dest_lat)
Random.data_Mdb$Dest_lon<-as.numeric(Random.data_Mdb$Dest_lon)

# Gravit_rand

Random.data_Gdb$Dep_lon<-as.numeric(Random.data_Gdb$Dep_lon)
Random.data_Gdb$Dep_lat<-as.numeric(Random.data_Gdb$Dep_lat)
Random.data_Gdb$Dest_lat<-as.numeric(Random.data_Gdb$Dest_lat)
Random.data_Gdb$Dest_lon<-as.numeric(Random.data_Gdb$Dest_lon)


plot(g2)## Molecular
plot(gve)## Gravity
plot(gv2)## Emperical
Random.data<- as.matrix(Random.data)
g3=graph.edgelist(Random.data,directed=T)
## Random_control
Randplot<-plot.igraph(g3,layout=layout.fruchterman.reingold,edge.curved=0.2, edge.arrow.size=0.3) #

### Small worldness of each network
library(qgraph)
smallworldness(g2, B = 1000, up = 0.995, lo = 0.005)
smallworldness(gv2, B = 1000, up = 0.995, lo = 0.005)
smallworldness(gve, B = 1000, up = 0.995, lo = 0.005)

# rownames(mol_net1) <- mol_net1[,1]
# rownames(grav_net1) <- grav_net1[,1] # same for mol_net1
# mol_net1 <- subset(mol_net1, select=-X)
# grav_net1 <- subset(grav_net1, select=-X)
# grav_net1<- as.matrix(grav_net1)
# mol_net1<- as.matrix(mol_net1)
# View(mol_net1)
# g3=graph.adjacency(grav_net1,mode="directed",weighted=NULL) # g2 = mol_net2, g1 = mol_net1
# g1=graph.adjacency(mol_net2,mode="directed",weighted=NULL)

# plot(g1)
# plot(g3)
# plot(g2)


# NETWORK-LEVEL MEASURES

graph.density(g2,loops=FALSE) # density of the net
graph.density(gv2,loops=FALSE) # density of the net
graph.density(gve,loops=FALSE) # density of the net
#graph.density(g4,loops=FALSE) # density of the net
graph.density(g3,loops=FALSE) # density of the net

global_t1 <- transitivity(g2, type="global") # CC
global_t2 <- transitivity(gv2, type="global")
global_t3 <- transitivity(gve, type="global")
global_t4 <- transitivity(g3, type="global")
avg_1.path <- igraph::average.path.length(g2)
avg_2.path <- igraph::average.path.length(gv2)
avg_3.path <- igraph::average.path.length(gve)
avg_4.path <- igraph::average.path.length(g4)
diameter(g2, directed=TRUE, unconnected=TRUE, weights = NULL)
diameter(gv2, directed=TRUE, unconnected=TRUE, weights = NULL)
diameter(gve, directed=TRUE, unconnected=TRUE, weights = NULL)
diameter(g4, directed=TRUE, unconnected=TRUE, weights = NULL)
reciprocity(g2) # measures the tendency of vertex pairs to form mutual connections between each other 
reciprocity(gv2)
reciprocity(gve)
reciprocity(g4)
assortativity.degree(g2)
assortativity.degree(gv2)
assortativity.degree(gve)
assortativity.degree(g4)
# NODE-LEVEL MEASURES
# degre
in.deg.bin_1<-degree(g2,v=V(g2),mode="in")
out.deg.bin_1<-degree(g2,v=V(g2),mode="out")
in.deg.bin_2<-degree(gv2,v=V(gv2),mode="in")
out.deg.bin_2<-degree(gv2,v=V(gv2),mode="out")
in.deg.bin_3<-degree(gve,v=V(gve),mode="in")
out.deg.bin_3<-degree(gve,v=V(gve),mode="out")
in.deg.bin_4<-degree(g3,v=V(g3),mode="in")
out.deg.bin_4<-degree(g3,v=V(g3),mode="out")


out_deg.bin_sorted_1 <- as.table(sort(out.deg.bin_1, decreasing=TRUE))
in_deg.bin_sorted_1 <- as.table(sort(in.deg.bin_1, decreasing=TRUE))
out_deg.bin_sorted_2 <- as.table(sort(out.deg.bin_2, decreasing=TRUE))
in_deg.bin_sorted_2 <- as.table(sort(in.deg.bin_2, decreasing=TRUE))
out_deg.bin_sorted_3 <- as.table(sort(out.deg.bin_3, decreasing=TRUE))
in_deg.bin_sorted_3 <- as.table(sort(in.deg.bin_3, decreasing=TRUE))
out_deg.bin_sorted_4 <- as.table(sort(out.deg.bin_4, decreasing=TRUE))
in_deg.bin_sorted_4 <- as.table(sort(in.deg.bin_4, decreasing=TRUE))


out_deg.bin_sorted_1 <- as.data.frame(out_deg.bin_sorted_1) 
in_deg.bin_sorted_1 <- as.data.frame(in_deg.bin_sorted_1)
out_deg.bin_sorted_2 <- as.data.frame(out_deg.bin_sorted_2)
in_deg.bin_sorted_2 <- as.data.frame(in_deg.bin_sorted_2)
out_deg.bin_sorted_3 <- as.data.frame(out_deg.bin_sorted_3)
in_deg.bin_sorted_3 <- as.data.frame(in_deg.bin_sorted_3)
out_deg.bin_sorted_4 <- as.data.frame(out_deg.bin_sorted_4)
in_deg.bin_sorted_4 <- as.data.frame(in_deg.bin_sorted_4)


betweenness.bin_1<-betweenness(g2,v=V(g2), directed=TRUE,weights=NA) # 
betweenness.bin_2<-betweenness(gv2,v=V(gv2), directed=TRUE,weights=NA)
betweenness.bin_3<-betweenness(gve,v=V(gve), directed=TRUE,weights=NA)
betweenness.bin_4<-betweenness(g3,v=V(g3), directed=TRUE,weights=NA)


# bet
betweenness.bin_1<-sort(betweenness(g2,v=V(g2), directed=TRUE,weights=NA), decreasing=T, na.last=T) # 
betweenness.bin_2<-sort(betweenness(gv2,v=V(gv2), directed=TRUE,weights=NA), decreasing=T, na.last=T) 
betweenness.bin_3<-sort(betweenness(gve,v=V(gve), directed=TRUE,weights=NA), decreasing=T, na.last=T) 
betweenness.bin_4<-sort(betweenness(g3,v=V(g3), directed=TRUE,weights=NA), decreasing=T, na.last=T) 
#
# clseness
all.clo.bin_1 <- sort(igraph::closeness(g2,mode="all",weights=NA,normalized=TRUE), decreasing=T) #  is divided by n–1 when normalised (non-normalised = the inverse of the sum of all geodesic distances)
in.clo.bin_1 <- sort(igraph::closeness(g2,mode="in",weights=NA,normalized=TRUE), decreasing=T)
out.clo.bin_1<- sort(igraph::closeness(g2,mode="out",weights=NA,normalized=TRUE), decreasing=T)

all.clo.bin_2 <- sort(igraph::closeness(gv2,mode="all",weights=NA,normalized=TRUE), decreasing=T) #  is divided by n–1 when normalised (non-normalised = the inverse of the sum of all geodesic distances)
in.clo.bin_2 <- sort(igraph::closeness(gv2,mode="in",weights=NA,normalized=TRUE), decreasing=T)
out.clo.bin_2<- sort(igraph::closeness(gv2,mode="out",weights=NA,normalized=TRUE), decreasing=T)

all.clo.bin_3 <- sort(igraph::closeness(gve,mode="all",weights=NA,normalized=TRUE), decreasing=T) #  is divided by n–1 when normalised (non-normalised = the inverse of the sum of all geodesic distances)
in.clo.bin_3 <- sort(igraph::closeness(gve,mode="in",weights=NA,normalized=TRUE), decreasing=T)
out.clo.bin_3<- sort(igraph::closeness(gve,mode="out",weights=NA,normalized=TRUE), decreasing=T)


all.clo.bin_4 <- sort(igraph::closeness(g3,mode="all",weights=NA,normalized=TRUE), decreasing=T) #  is divided by n–1 when normalised (non-normalised = the inverse of the sum of all geodesic distances)
in.clo.bin_4 <- sort(igraph::closeness(g3,mode="in",weights=NA,normalized=TRUE), decreasing=T)
out.clo.bin_4<- sort(igraph::closeness(g3,mode="out",weights=NA,normalized=TRUE), decreasing=T)
#
#Eigenvector
undir.eig.bin_1 <- sort(evcent(g2, directed = F, scale = T, weights = NA)$vector, decreasing=T) #just for comparison
undir.eig.bin_2 <- sort(evcent(gv2, directed = F, scale = T, weights = NA)$vector, decreasing=T)
undir.eig.bin_3 <- sort(evcent(gve, directed = F, scale = T, weights = NA)$vector, decreasing=T)
undir.eig.bin_4 <- sort(evcent(g3, directed = F, scale = T, weights = NA)$vector, decreasing=T)


bin.centr.all_1 <-as.data.frame(list(Sub_loc=V(g2)$name, Bet=betweenness.bin_1, Out_Degree=out.deg.bin_1,
                                     In_Degree=in.deg.bin_1,Eigen=undir.eig.bin_1), stringsAsFactors=FALSE)


bin.centr.all_2 <-as.data.frame(list(Sub_los=V(gv2)$name, Bet=betweenness.bin_2, Out_Degree=out.deg.bin_2,
                                     In_Degree=in.deg.bin_2,Eigen=undir.eig.bin_2), stringsAsFactors=FALSE)

bin.centr.all_3 <-as.data.frame(list(Sub_loz=V(gve)$name, Bet=betweenness.bin_3, Out_Degree=out.deg.bin_3,
                                     In_Degree=in.deg.bin_3,Eigen=undir.eig.bin_3), stringsAsFactors=FALSE)

bin.centr.all_4 <-as.data.frame(list(Sub_lok=V(g3)$name, Bet=betweenness.bin_4, Out_Degree=out.deg.bin_4,
                                     In_Degree=in.deg.bin_4,Eigen=undir.eig.bin_4), stringsAsFactors=FALSE)

Pearson_correlation_matrix_B_all_1 <- cor(subset(bin.centr.all_1, select=-Sub_loc))
Pearson_correlation_matrix_B_all_2 <- cor(subset(bin.centr.all_2, select=-Sub_los))
Pearson_correlation_matrix_B_all_3 <- cor(subset(bin.centr.all_3, select=-Sub_loz))
Pearson_correlation_matrix_B_all_4 <- cor(subset(bin.centr.all_4, select=-Sub_lok))

                                          ###****************************** 
                                           ### NODE LEVEL PARAMETERS  ###
                                           ###****************************** 
### KEY ACTOR ANALYSIS

## Gravity 
undir.eig.bin_grav <- evcent(gv2, directed = FALSE, scale =TRUE, weights=NA)$vector 
dir.betweenness.bin_grav<-betweenness(gv2,v=V(gv2), directed=TRUE,weights=NA) # directed binary


cent_grav<- data.frame(bet_grav=dir.betweenness.bin_grav,eig_grav=undir.eig.bin_grav)
res_grav<-lm(eig_grav~bet_grav,data=cent_grav)$residuals # get the residuals of the lm between them
res2_grav <- res_grav*2 # doubling the values of residuals just for plotting them bigger in the next plot
cent<-transform(cent_grav, res_grav=res_grav)# add residuals to the df
# 

# Emperical

undir.eig.bin_emp <- evcent(gve, directed = FALSE, scale =TRUE, weights=NA)$vector 
dir.betweenness.bin_emp<-betweenness(gve,v=V(gve), directed=TRUE,weights=NA) # directed binary
#
# PLOT SIMPLE CORRELATION BETWEEN BET AND EIGEN
cent_emp<- data.frame(bet_emp=dir.betweenness.bin_emp,eig_emp=undir.eig.bin_emp)
res_emp<-lm(eig_emp~bet_emp,data=cent_emp)$residuals # get the residuals of the lm between them
res2 <- res_emp*2 # doubling the values of residuals just for plotting them bigger in the next plot
cent_emp<-transform(cent_emp,res_emp=res_emp)# add residuals to the df
# 

# Molecular
undir.eig.bin_mol <- evcent(g2, directed = FALSE, scale =TRUE, weights=NA)$vector 
dir.betweenness.bin_mol<-betweenness(g2,v=V(g2), directed=TRUE,weights=NA) # directed binary
#
# PLOT SIMPLE CORRELATION BETWEEN BET AND EIGEN
cent_mol<- data.frame(bet_mol=dir.betweenness.bin_mol,eig_mol=undir.eig.bin_mol)
res_mol<-lm(eig_mol~bet_mol,data=cent_mol)$residuals # get the residuals of the lm between them
res2_mol <- res_mol*2 # doubling the values of residuals just for plotting them bigger in the next plot
cent<-transform(cent_mol,res_mol=res_mol)# add residuals to the df
# 

# Random
undir.eig.bin_rand <- evcent(g3, directed = FALSE, scale =TRUE, weights=NA)$vector 
dir.betweenness.bin_rand<-betweenness(g3,v=V(g3), directed=TRUE,weights=NA) # directed binary
#
# PLOT SIMPLE CORRELATION BETWEEN BET AND EIGEN
cent_rand<- data.frame(bet_rand=dir.betweenness.bin_rand,eig_rand=undir.eig.bin_rand)
res_rand<-lm(eig_rand~bet_rand,data=cent_rand)$residuals # get the residuals of the lm between them
res2_rand <- res_rand*2 # doubling the values of residuals just for plotting them bigger in the next plot
sent<-transform(cent_rand,res_rand=res_rand)# add residuals to the df
# 

