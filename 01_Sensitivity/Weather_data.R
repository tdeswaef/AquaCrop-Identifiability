graphics.off()
rm(list = ls())
options(error=stop)

#####################
#PATHS and files
#####################

setwd("C:/Doctoraat/KMI_DATA/DATA")
Coordinates_field <- "Proefvelden17.kml" #kml file with coordinates of the selected field
JRC_file <- "Flanders1975_2018.csv" #file with JRC weather data

################################################################
#look which pixels corresponds with the locations of proefvelden
################################################################

#place locations of proefvelden in google mymaps and download kml file which contain coordinates of the fields
library(maptools) #for the function getKMLcoordinates
Proefveld <- t(as.data.frame(getKMLcoordinates(Coordinates_field, ignoreAltitude=TRUE))) #place locations in a dataframe 
rownames(Proefveld) <- c()
colnames(Proefveld) <- c("LON", "LAT")
Proefveld <- as.data.frame(Proefveld)

library(rgeos) #for gdistance the coordinates need to be changed to sp objects.
setProefveldsp <- SpatialPoints(Proefveld)

#For JRC data
JRC <- read.csv(JRC_file, sep=";", header=TRUE)
JRC <- unique(JRC)
JRC_grid <- JRC[,c(1:3)]
JRC_grid <- unique(JRC_grid)
setJRCsp <- SpatialPoints(JRC_grid[, c(3,2)])
distancematrix <- gDistance(setProefveldsp, setJRCsp, byid=TRUE)
GridN <- apply(distancematrix, 2, which.min)
GridN <- unique(GridN)

#Since the rownumbers of the JRC dataset don't correspond with the location ids: we need to find the right ids for the calculated rownumbers
JRCno <- vector()

for (j in GridN){
  JRCno <- rbind(JRC_grid[j,1], JRCno)
}

######################################
#Put data for in a list in a dataframe
######################################
JRClist <- list() 

x = 1
for (n in JRCno){
  JRClist[[x]] <-  data.frame(JRC[JRC$GRID_NO==n,])
  names(JRClist)[[x]] <- paste("PIXEL", n, sep = "")
  x=x+1
}
