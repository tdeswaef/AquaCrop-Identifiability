graphics.off()
rm(list = ls())
options(error=stop)

#######
#PATHS
#######
output.path.collin <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/Induce_Variation/MultipleSoils/95MAX/" #location where collinearity output is saved
scaled_path <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/MorrisPotatoS95max/" #location where scaled sensitivity functions are saved
comb2sens_path <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/Induce_Variation/MultipleSoils/2SEN95MAX/" #location for combined sensitivity functions

files <- list.files(path=scaled_path, pattern="*.rds", full.names=TRUE)
chrs <- nchar(comb2sens_path)

#################################################################
#Combine sensitivity functions for combinations of soil types
#################################################################.

#baseT1, upT2, pexup3, pexlw4, psto5, psen6,rtx7 ,rtm8, rtshp9, rtexup10, rtexlw11,cgc12, ccx13, cdc14, hi15, 
#ccs16, kc17,kcdcl18, wp19, anaer20, evardc21, hipsflo22, hingsto23, hinc24, stbio25,plan26, yld27, root28, sen29, mat30,
#hilen31,  pexhp32, pstoshp33, psenshp34
parset <- c(7,26,12,27,16,28,9,29,32,19,17,14,4,15,31,3,6,8)
library(abind)

for(year in c(1975:2018)){
  soils <- NULL
  for (i in c("Loamysand","Loam")){
    morris <- readRDS(paste(scaled_path,year,i,".rds", sep = ""))
    soils <- abind(soils, morris[,parset,1:115,], along = 3)
  }
  saveRDS(soils, file = paste(comb2sens_path,year,"LoamysandLoam.rds", sep = ""))
}

##########################
#Calculate collinearities
##########################

library(FME)
library(plyr)

files <- list.files(path=comb2sens_path, pattern="*.rds", full.names=TRUE)

for (freq in c(7,14,21)){
whatdays <- c(seq(1,115, by = freq))
whatdays2 <- seq(116,230, by = freq)
whatdays3 <- seq(231,345, by = freq)

whatdays <- c(whatdays, whatdays2) #measuring days to select

out <- NULL
for (i in 1:length(files)){#length(files)
  out <- NULL
  morris <- readRDS(files[[i]]) #read in file by file
  for(r in 1:100){ 
    #select different slices from the 4 dimensional morris elementary effects output array to calculate collinearity from
    Yield <-   morris[r,,whatdays,4]
    CC <-      morris[r,,whatdays,2]
    Biomass <- morris[r,,whatdays,3]
    WC030 <-   morris[r,,whatdays,5]
    WC3060 <-  morris[r,,whatdays,6]
    Stage <-   morris[r,,whatdays,1]
    
    #merge the different arrays 
    vars <- rbind(t(Yield),t(CC),t(Biomass),t(WC030),t(WC3060),t(Stage))
    
    #remove columns without effects
    name = "noeff" 
    assign(x = name, value=vars[,apply(vars,2,function(x) !all(x==0))])
    
    #if only one parameter has an effect no collinearity can be calculated so skip
    if (ncol(as.data.frame(noeff)) < 2) next
    
    #calculate collinearity and put in a dataframe
    Coll <- collin(noeff, N=18)
    Coll$soil <- gsub('[[:digit:]]+', '', substr(substring(files[i],chrs+1),1,nchar(substring(files[i],chrs+1))-4))
    Coll$year <- substring(substring(files[i],chrs+1),1,4)
    Coll$replicate <- r
    out <- rbind.fill(Coll, out) 
    
    if (r == 100){
      filename <- paste(output.path.collin,freq, substring(substring(files[i],chrs+1),1,4),gsub('[[:digit:]]+', '', substr(substring(files[i],chrs+1),1,nchar(substring(files[i],chrs+1))-4)),".rds", sep = "")
      saveRDS(out, filename)
    }
  }
}
}

#########################################
#Merge all data in one dataframe
#########################################
library(plyr)
Noll <- NULL

for (freq in c("7","14","21")){
  for (sol in c("LoamysandLoam","LoamysandClayloam","LoamClayloam")){
    for (i in c(1975:2018)){
      
      filename <- paste(output.path.collin,freq,i,sol,".rds", sep = "")
      
      if(file.exists(filename)) {
        file <- readRDS(filename)
        dat <- file[which(file$N == 18),]
        dat$freq <- freq
        dat$soil <- sol
        Noll <- rbind.fill(Noll, dat)
      }
    }
  }
}

####################################################
#Change soil names to abbreviations
###################################################
library(ggplot2)
library(ggthemes)
theme_set(theme_few(base_size=14))
x1  = factor(Noll$freq, levels=c("7","14","21"))
soil_type  = factor(Noll$soil)
p2 <- ggplot(data = Noll, aes(x=x1, y=(1-1/collinearity))) + geom_boxplot(aes(fill=soil_type)) + 
  geom_hline(yintercept = 0.933333, linetype="dashed", color = "red") + xlab("Measurement interval (d)") + theme(legend.position = "top") + ylim(0.6,1)
p2

library(export)
graph2ppt(aspectr = 1, width = 6.2) 
