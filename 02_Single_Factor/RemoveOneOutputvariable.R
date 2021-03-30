graphics.off()
rm(list = ls())
options(error=stop)

library(FME)
library(plyr)

#######
#PATHS
#######
output.path.collin <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/Capture_Variation/Add_Remove_ChangeFrequency_OUTV/REMOV95MAX/" #location where collinearity output is saved
scaled_path <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/MorrisPotatoS95max/" #location where scaled sensitivity functions are saved

files <- list.files(path=scaled_path, pattern="*.rds", full.names=TRUE)
chrs <- nchar(scaled_path)

###########################
#Variables and parameters
############################

outlist <- c("Yield", "Biomass","CC","Stage","WC030","WC3060", "None")
parset <- c(7,26,12,27,16,28,9,29,32,19,17,14,4,15,31,3,6,8)

##########################
#Calculate collinearities
##########################
for (ot in outlist){
  for (fr in c(7,14,21)){#,7,14,21
    
    whatdays <- seq(1,115, by = fr)
    
    out <- NULL
    for (i in 1:length(files)){
      out <- NULL
      morris <- readRDS(files[[i]]) #read in file by file
      for(r in 1:100){ 
        #select different slices from the 4 dimensional morris elementary effects output array to calculate collinearity from
        if (ot == "None"){
          Yield <-   morris[r,parset,whatdays,4]
          Biomass <- morris[r,parset,whatdays,3]
          CC <-      morris[r,parset,whatdays,2]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
          
          vars <- rbind(t(Yield),t(Biomass),t(CC),t(WC030),t(WC3060),t(Stage))
        }
        if (ot == "Biomass"){
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,whatdays,2]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
          
          vars <- rbind(t(Yield),t(CC),t(WC030),t(WC3060),t(Stage))
        }
        if (ot == "Yield"){
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <- morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
          
          vars <- rbind(t(CC),t(Biomass),t(WC030),t(WC3060),t(Stage))
        }
        if (ot == "CC"){
          Yield <-   morris[r,parset,whatdays,4]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-  morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
          
          vars <- rbind(t(Yield),t(Biomass),t(WC030),t(WC3060),t(Stage))
        }
        if (ot == "Stage"){
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          
          vars <- rbind(t(Yield),t(CC),t(Biomass),t(WC030),t(WC3060))
        }
        if (ot == "WC030"){
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
          
          vars <- rbind(t(Yield),t(CC),t(Biomass),t(WC3060),t(Stage))
        }
        if (ot == "WC3060"){
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-   morris[r,parset,whatdays,5]
          Stage <-   morris[r,parset,whatdays,1]
          
          vars <- rbind(t(Yield),t(CC),t(Biomass),t(WC030),t(Stage))
        }
        
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

########################################
#Combine collinearities in one dataframe
########################################

library(plyr)
Noll <- NULL
outlist <- c("Biomass","CC","Stage","WC030","WC3060")#)#"Yield", "Biomass","CC","Stage","WC030","WC3060")
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

####################
#make plot
###################
library(ggplot2)
library(ggthemes)
theme_set(theme_few(base_size=14))
x1  = factor(Noll$freq, levels=c("7","14","21"))#,"7", "14","21"
x2 = factor(Noll$output, levels=c("WC030","WC3060","CC","Biomass","Yield","Stage", "None"))
p3 <- ggplot(data = Noll, aes(x=x1, y=(1-1/collinearity))) + geom_boxplot(aes(fill=x2)) +
  geom_hline(yintercept = 0.933333, linetype="dashed", color = "red") + xlab("Measurement interval (d)") + theme(legend.position = "top")+ scale_fill_manual(values = c( "#00AFBB","#0066FF", "#7CAE00","#E7B800", "#FC4E07", "#F4EDCA"),name = "Daily measured", labels = c("WC030","WC3060","CC","BIOMASS","YIELD","STAGE", "None")) +
  scale_colour_manual(values = c( "#00AFBB","#0066FF", "#7CAE00","#E7B800", "#FC4E07", "#F4EDCA"))+ ylim(0.6,1)#scale_y_continuous(trans = "log10")
p3

library(export)
graph2ppt(aspectr = 1, width = 6.2) 

