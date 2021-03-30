graphics.off()
rm(list = ls())
options(error=stop)

#######
#PATHS
#######
output.path.collin <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/Induce_Variation/MultipleYears/MAX95/" #location where collinearity output is saved
scaled_path <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/MorrisPotatoS95max/" #location where scaled sensitivity functions are saved
combsens_path <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/Induce_Variation/MultipleYears/" #location for combined sensitivity functions

files <- list.files(path=scaled.path, pattern="*.rds", full.names=TRUE)
chrs <- nchar(combsens_path)

#################################################################
#Combine sensitivity functions for a certain combination of years
#################################################################

parset <- c(7,26,12,27,16,28,9,29,32,19,17,14,4,15,31,3,6,8)

library(abind)

Yearstwo <- NULL

for (i in c("Loamysand","Loam", "Clayloam")){#"Loamysand","Loam"
  for(year in c(1975:2012)){
    Yearstwo <- NULL
    morris <- readRDS(paste(scaled_path,year,i,".rds", sep = ""))
    morris2 <- readRDS(paste(scaled_path,year+1,i,".rds", sep = ""))
    morris3 <- readRDS(paste(scaled_path,year+2,i,".rds", sep = ""))
    morris4 <- readRDS(paste(scaled_path,year+3,i,".rds", sep = ""))
    morris5 <- readRDS(paste(scaled_path,year+4,i,".rds", sep = ""))
    morris6 <- readRDS(paste(scaled_path,year+5,i,".rds", sep = ""))
    morris7 <- readRDS(paste(scaled_path,year+6,i,".rds", sep = ""))
    Yearsseven <- abind(morris[,parset,1:115,], morris2[,parset,1:115,], morris3[,parset,1:115,], morris4[,parset,1:115,],morris5[,parset,1:115,],morris6[,parset,1:115,],morris7[,parset,1:115,], along = 3)
    saveRDS(Yearsseven, file = paste(combsens_path,7,year,i,".rds", sep = ""))
  }
}


############################
#Calculate collinearities
############################

library(FME)
library(plyr)

#parset you want to calculate collinearity from
#order of parset: baseT1, upT2, pexup3, pexlw4, psto5, psen6,rtx7 ,rtm8, rtshp9, rtexup10, rtexlw11,cgc12, ccx13, cdc14, hi15, 
#ccs16, kc17,kcdcl18, wp19, anaer20, evardc21, hipsflo22, hingsto23, hinc24, stbio25,plan26, yld27, root28, sen29, mat30,
#hilen31,  pexhp32, pstoshp33, psenshp34

for (years in 1:7){
  files <- list.files(path=paste(combsens_path,years,sep = ""), pattern="*.rds", full.names=TRUE)
  for (fr in c(7,14,21)){
  whatdays <- seq(1,115, by = fr)
  whatdays2 <- seq(116,230, by = fr)
  whatdays3 <- seq(231,345, by = fr)
  whatdays4 <- seq(346,460, by = fr)
  whatdays5 <- seq(461,575, by = fr)
  whatdays6 <- seq(576,690, by = fr)
  whatdays7 <- seq(691,805, by = fr)
  
  if (years == 2){
    whatdays <- c(whatdays, whatdays2)
  }
  if (years == 3){
    whatdays <- c(whatdays, whatdays2, whatdays3)
  }
  if (years == 4){
    whatdays <- c(whatdays, whatdays2, whatdays3, whatdays4)
  }
  if (years == 5){
    whatdays <- c(whatdays, whatdays2, whatdays3, whatdays4, whatdays5)
  }
  if (years == 6){
    whatdays <- c(whatdays, whatdays2, whatdays3, whatdays4, whatdays5, whatdays6)
  }
  if (years == 7){
    whatdays <- c(whatdays, whatdays2, whatdays3, whatdays4, whatdays5, whatdays6, whatdays7)
  }

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
        filename <- paste(output.path.collin,years,fr ,substring(substring(files[i],chrs+1),1,4),gsub('[[:digit:]]+', '', substr(substring(files[i],chrs+1),1,nchar(substring(files[i],chrs+1))-4)),".rds", sep = "")
        saveRDS(out, filename)
      }
    }
  }
}
}

########################################################
#Merge and plot the results of collinearity experiments
########################################################

library(plyr)
Noll <- NULL

for (years in c(1:7)){
  for (freq in c("7","14","21")){
    for (sol in c("Clayloam","Loam","Loamysand")){ 
      for (i in c(1975:2018)){ 
        
        filename <- paste(output.path.collin,years,freq,i,sol,".rds", sep = "")
        
        if(file.exists(filename)) { 
          file <- readRDS(filename)
          dat <- file[which(file$N == 18),]
          dat$NYears <- years
          dat$freq <- freq
          Noll <- rbind.fill(Noll, dat)
        }
      }
    }
  }
}


x1  = factor(Noll$freq, levels=c("7", "14","21"))
library(ggplot2)
library(ggthemes)
theme_set(theme_few(base_size=14))
p1 <- ggplot(data = Noll, aes(x=x1, y=(1-1/collinearity))) + geom_boxplot(aes(fill=NYears)) + 
  geom_hline(yintercept = 0.933333, linetype="dashed", color = "red") + xlab("Measurement interval (d)") +
  theme(legend.position = "top") + ylim(0.6,1) 

p1
library(export)
graph2ppt(aspectr = 1, width = 6.2) 
