####***********************************************************************************
#### The code for generating gravity model infered movement for the whole of Cameroon
####***********************************************************************************
library(ggplot2)
library(igraph)
library(reshape2)
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
library(diagram)

## Read in the data

Demogp_dat <- read.csv("DBSX3.csv",sep=",", header=T)# Demograhic data in DBSX3/sheetname= FULLnational data (convert this to csv to read in)
market_dat <- read.csv("cattle_human_pop.csv",sep=",", header=T)#   population in DBSX3  (convert this to csv to read in)
market_dat <- market_dat[market_dat$Country == "CAMEROON",] # Select nodes only withing Cameroon 

# cameroon_districs<-readRDS("./cameroon_administrative/CMR_adm1.rds")
cameroon_districts <- readRDS("./cameroon_administrative/CMR_adm1.rds")
cameroon_subdistricts <- readRDS("./cameroon_administrative/CMR_adm2.rds")
cameroon_sub2districts <- readRDS("./cameroon_administrative/CMR_adm3.rds")

sum(!market_dat$Subdivision %in% cameroon_sub2districts$NAME_3)
sum(market_dat$Subdivision %in% cameroon_subdistricts$NAME_2)

livsk_2005 <- read.csv("cattle_human_pop_2014.csv") #Demograhic data in DBSX3 (convert this to csv to read in)
livsk_2005$Region <- as.factor(trimws(as.character(livsk_2005$Region)))
regions <- c("Adamawa", "Central", "East", "Ext_North","Littoral", "North", "North_West", "South", "South_West", "West")
livsk_2005$Region <- regions[livsk_2005$Region]
livsk_2005$Pop_ANHU_diff <- livsk_2005$Subdivision_cattle_pop_num - livsk_2005$Subdivision_human_pop_num
orig_points <- livsk_2005

livsk_2005 <- livsk_2005[livsk_2005$Subdivision_cattle_pop_num > 0,]

#Over lay the gravity model
# new_grav_dat<-read.csv("cattle_human_pop_2Edit.csv",sep=",", header=T)#This the Empirical dataset DBSX1, this will be used to determing a threshold for movement
new_grav_dat <- livsk_2005[complete.cases(livsk_2005),]

new_grav_dat$Subdivision<-as.character(new_grav_dat$Subdivision)
# new_grav_dat$Subdivision[new_grav_dat$Subdivision== "Jakiri"]<-"Nkum"  ## This part of rescaling the data to subdivision level as discribed in the Maniscript

#Using census data----
CAM_g_model_new <- matrix(data = NA, nrow = length(new_grav_dat$Subdivision), ncol = length(new_grav_dat$Subdivision), dimnames = list(new_grav_dat$Subdivision,new_grav_dat$Subdivision))

for (i in 1:length(new_grav_dat$Subdivision)){
  for (j in 1:length(new_grav_dat$Subdivision)){
    CAM_g_model_new[i,j] <- ((new_grav_dat$cattle_per_person[i]*new_grav_dat$cattle_per_person[j])/(pointDistance(c(new_grav_dat$LATS[i], new_grav_dat$LONGS[i]), c(new_grav_dat$LATS[j], new_grav_dat$LONGS[j]), lonlat = FALSE)))
  }
}


### Code section for determining  optimal threshold(AUC) that defines a movement in the gravity network using Emperical 
###************************************************************************************************************************

# View(CAM_g_model_new)
diag(CAM_g_model_new) <- 0 ## convert the diagnal inf to 0
c(CAM_g_model_new)## Vector
length(c(CAM_g_model_new))

#normalising the contact network optain by gravity model
CAM_g_model_new2 <- vector()
for(ii in 1:nrow(livsk_2005)){ CAM_g_model_new2 <- rbind(CAM_g_model_new2, CAM_g_model_new[ii,]/rowSums(CAM_g_model_new)[ii])}


#========================================================================================================

#### In this gravity model, we take into account the repulsive forces between subdivisions 
# The normalisation is done using cattle population of the destination the dep animal population.
# this normalisation also helps to weight the movemenets 
#========================================================================================================

CAM_g_model_new3 <- matrix(data= NA, nrow=length(new_grav_dat$Subdivision), ncol =length(new_grav_dat$Subdivision), dimnames=list(new_grav_dat$Subdivision, new_grav_dat$Subdivision))

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

thresh <- get(load("..//RImages//GravityThreshold.RData"))


CAM_g_model_new3 <- matrix(NA,ncol=length(new_grav_dat$Subdivision), nrow=length(new_grav_dat$Subdivision))
for (i in 1:length(new_grav_dat$Subdivision)){
  for (j in 1:length(new_grav_dat$Subdivision)){  
    CAM_g_model_new3[i,j] <- calc_grav(new_grav_dat[,c("Subdivision_human_pop_num","Subdivision_cattle_pop_num","LATS","LONGS")], i, j)
    if(is.na(CAM_g_model_new3[i,j]) ) message("from: ", i,", to: ", j,", val: ",CAM_g_model_new3[i,j])
  }
}
#names
colnames(CAM_g_model_new3) = rownames(CAM_g_model_new3) = new_grav_dat$Subdivision

#normalise the matrix
norm_mat <- vector(); for(ii in 1:nrow(livsk_2005)){ norm_mat <- rbind(norm_mat, CAM_g_model_new3[ii,]/rowSums(CAM_g_model_new3)[ii])}
attr.dat=new_grav_dat
rownames(attr.dat) = attr.dat$Subdivision
norm_mat <- norm_mat * attr.dat[colnames(norm_mat),"Subdivision_cattle_pop_num"]

weight_mat <- norm_mat
weight_mat[weight_mat < thresh] <- 0
diag(weight_mat) <- 0

#dichotomising the matrix
norm_mat[norm_mat<thresh] <- 0
norm_mat[norm_mat>0] <- 1
diag(norm_mat)<-0 ## remove movements at the same point..


##**************************
### PLOT the network in Igraph
##**************************
gv2<- graph.adjacency(weight_mat, mode="directed", weighted=TRUE)
plot(gv2)
plot.igraph(gv2,layout=layout.fruchterman.reingold,edge.curved=0.2, edge.arrow.size=0.3) #


##****************************************************
### PLOT the network on the Map of Cameroon
##****************************************************

Gravity.data <- melt(weight_mat)
Gravity.data$Var1 <- colnames(weight_mat)[Gravity.data$Var1]

#Gravity.data <- subset(Gravity.data, X1!=X2)
Gravity.mov.data<- subset(Gravity.data, value > 0)

Gravity.mov.data$Dep_lat <- livsk_2005$LATS[match(Gravity.mov.data$Var1, livsk_2005$Subdivision)]
Gravity.mov.data$Dep_lon <- livsk_2005$LONGS[match(Gravity.mov.data$Var1, livsk_2005$Subdivision)]
Gravity.mov.data$Dest_lat <- livsk_2005$LATS[match(Gravity.mov.data$Var2, livsk_2005$Subdivision)]
Gravity.mov.data$Dest_lon <- livsk_2005$LONGS[match(Gravity.mov.data$Var2, livsk_2005$Subdivision)]


# Sort out the line weights -----------------------------------------------

lcoef <- 5000
Gravity.mov.data$lWeight <- Gravity.mov.data$value * lcoef
Gravity.mov.data$lWeight[Gravity.mov.data$lWeight > 2] <- 2

tail(sort(Gravity.mov.data$lWeight))


dep_coords <- coordinates(cbind(Gravity.mov.data$Dep_lon, Gravity.mov.data$Dep_lat))

Gravity.mov.data### FOR PLOTTING


