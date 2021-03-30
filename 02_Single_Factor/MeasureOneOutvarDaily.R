graphics.off()
rm(list = ls())
options(error=stop)

library(FME)
library(plyr)

#######
#PATHS
#######
output.path.collin <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/Capture_Variation/Add_Remove_ChangeFrequency_OUTV/ONE_evr95MAX/" #location where collinearity output is saved
scaled_path <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/MorrisPotatoS95max/" #location where scaled sensitivity functions are saved

#########################
#Variables and parameters
#########################

outlist <- c("Yield","Biomass","CC","Stage","WC030","WC3060", "None") #Variables you want to measure daily
parset <- c(7,26,12,27,16,28,9,29,32,19,17,14,4,15,31,3,6,8)

files <- list.files(path=scaled_path, pattern="*.rds", full.names=TRUE)
chrs <- nchar(scaled_path) #number of characters in pathname

#order of parset: baseT1, upT2, pexup3, pexlw4, psto5, psen6,rtx7 ,rtm8, rtshp9, rtexup10, rtexlw11,cgc12, ccx13, cdc14, hi15, 
#ccs16, kc17,kcdcl18, wp19, anaer20, evardc21, hipsflo22, hingsto23, hinc24, stbio25,plan26, yld27, root28, sen29, mat30,
#hilen31,  pexhp32, pstoshp33, psenshp34

#########################
#Calculate collinearities
#########################

for (ot in outlist){
  for (fr in c(7,14,21)){
    
    whatdays <- seq(1,115, by = fr)
    
    out <- NULL
    for (i in 1:length(files)){
      out <- NULL
      morris <- readRDS(files[[i]])
      for(r in 1:100){
        #select different slices from the 4 dimensional morris elementary effects output array to calculate collinearity from
        if (ot == "None"){
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
        }
        if (ot == "Biomass"){
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,1:115,3]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
        }
        if (ot == "Yield"){
          Yield <-   morris[r,parset,1:115,4]
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
        }
        if (ot == "CC"){
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,1:115,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
        }
        if (ot == "Stage"){
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,1:115,1]
        }
        if (ot == "WC030"){
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-   morris[r,parset,1:115,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
        }
        if (ot == "WC3060"){
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,1:115,6]
          Stage <-   morris[r,parset,whatdays,1]
        }
        #merge the different arrays 
        vars <- rbind(t(Yield),t(CC),t(Biomass),t(WC030),t(WC3060),t(Stage))
        
        #remove columns without effects
        name = "noeff" 
        assign(x = name, value=vars[,apply(vars,2,function(x) !all(x==0))])
        
        #if only one parameter has an effect no collinearity can be calculated so skip
        if (ncol(as.data.frame(noeff)) < 2) next
        
        #calculate collinearity and put in a dataframe
        Coll <- collin(noeff, N = 18)
        Coll$soil <- gsub('[[:digit:]]+', '', substr(substring(files[i],chrs+1),1,nchar(substring(files[i],chrs+1))-4))
        Coll$year <- substring(substring(files[i],chrs+1),1,4)
        Coll$replicate <- r
        out <- rbind.fill(Coll, out) 
        
        if (r == 100){
          filename <- paste(output.path.collin,ot,fr,substring(substring(files[i],chrs+1),1,4),gsub('[[:digit:]]+', '', substr(substring(files[i],chrs+1),1,nchar(substring(files[i],chrs+1))-4)),".rds", sep = "")
          saveRDS(out, filename)
        }
      }
    }
  }
}

##############################################################################################################
#Combine collinearities of different conditions in one dataframe for measuring only specific output every day
##############################################################################################################
library(plyr)
Noll <- NULL
outlist <- c("Yield","Biomass","CC","Stage","WC030","WC3060")
for (ot in outlist){
  for (fr in c(7,14,21)){
    for (sol in c("Clayloam","Loam","Loamysand")){ 
      for (i in c(1975:2018)){ 
        
        filename <- paste(output.path.collin,ot,fr,i,sol,".rds", sep = "")
        
        if(file.exists(filename)) {
          file <- readRDS(filename)
          dat <- file[which(file$N == 18),]
          dat$freq <- as.character(fr)
          dat$output <- ot
          Noll <- rbind.fill(Noll, dat)
        
        }
      }
    }
  }
}

##########
#Make plot
##########
library(ggplot2)
library(ggthemes)
theme_set(theme_few(base_size=14))

x1  = factor(Noll$freq, levels = c("7", "14","21"))
Variables = factor(Noll$output, levels = c("WC030","WC3060","CC","Biomass","Yield","Stage"))
p4 <- ggplot(data = Noll, aes(x=x1, y=(1-1/collinearity))) + geom_boxplot(aes(fill=Variables)) + 
  geom_hline(yintercept = 0.933333, linetype="dashed", color = "red") + xlab("Measurement interval (d)") +
  theme(legend.position = "top") + scale_fill_manual(values = c( "#00AFBB","#0066FF", "#7CAE00","#E7B800", "#FC4E07", "#F4EDCA"),name = "Sensor measured", labels = c("WC030","WC3060","CC","BIOMASS","YIELD","STAGE", "None")) +
  scale_colour_manual(values = c( "#00AFBB","#0066FF", "#7CAE00","#E7B800", "#FC4E07", "#F4EDCA")) + ylim(0.6,1)
  
library(export)
graph2ppt(aspectr = 1, width = 6.2) 
