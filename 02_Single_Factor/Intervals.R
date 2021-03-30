graphics.off()
rm(list = ls())
options(error=stop)

library(FME)
library(plyr)

#######
#PATHS
#######
output.collin.path <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/Capture_Variation/Measurement_Int95MAX/Collin_18pars/" #location where collinearity results are saved
scaled_path <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/MorrisPotatoS95max/" #location where scaled sensitivity functions are saved

files <- list.files(path=scaled_path, pattern="*.rds", full.names=TRUE) #files with the rescaled sensitivity functions
chrs <- nchar(scaled_path)

###########
#parameters
###########

#order of parset: baseT1, upT2, pexup3, pexlw4, psto5, psen6,rtx7 ,rtm8, rtshp9, rtexup10, rtexlw11,cgc12, ccx13, cdc14, hi15, 
#ccs16, kc17,kcdcl18, wp19, anaer20, evardc21, hipsflo22, hingsto23, hinc24, stbio25,plan26, yld27, root28, sen29, mat30,
#hilen31,  pexhp32, pstoshp33, psenshp34
parset <- c(7,26,12,27,16,28,9,29,32,19,17,14,4,15,31,3,6,8) #parameters to select from Morris output (18 most important)

#####################################################
#Calculate collinearities and save to different files
#####################################################

for (fr in c(1:21)){
    
    whatdays <- seq(1,115, by = fr) #Select measurement days
    
    out <- NULL
    for (i in 1:length(files)){#length(files)
      out <- NULL
      morris <- readRDS(files[[i]]) #read in file by file
      for(r in 1:100){ #calculate collinearity for every replicate (trajectory)
        
        #select different slices from the 4 dimensional morris elementary effects output array to calculate collinearity from
          Yield <-   morris[r,parset,whatdays,4]
          CC <-      morris[r,parset,whatdays,2]
          Biomass <- morris[r,parset,whatdays,3]
          WC030 <-   morris[r,parset,whatdays,5]
          WC3060 <-  morris[r,parset,whatdays,6]
          Stage <-   morris[r,parset,whatdays,1]
          
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
          filename <- paste(output.collin.path,fr,substring(substring(files[i],chrs+1),1,4),gsub('[[:digit:]]+', '', substr(substring(files[i],chrs+1),1,nchar(substring(files[i],chrs+1))-4)),".rds", sep = "")
          saveRDS(out, filename)
      }
    }
  }
}

#################################################################
#Combine collinearities of different conditions in one dataframe
#################################################################

library(plyr)
Noll <- NULL
outlist <- c("Yield","Biomass","CC","Stage","WC030","WC3060")

  for (fr in c(1:21)){
    for (sol in c("Clayloam","Loam","Loamysand")){ 
      for (i in c(1975:2018)){ 
        
        filename <- paste(output.collin.path,fr,i,sol,".rds", sep = "")
        
        if(file.exists(filename)) {
          file <- readRDS(filename)
          dat <- file[which(file$N == 18),]
          dat$freq <- as.character(fr)
          Noll <- rbind.fill(Noll, dat)
        }
      }
    }
  }


###################
#Make plot
###################
library(ggplot2)
library(ggthemes)
theme_set(theme_few(base_size=14))

x1  = factor(Noll$freq, levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9","10", "11", "12","13", "14", "15", "16", "17","18","19","20","21"))
p2 <- ggplot(Noll, aes(x=x1, y=1-1/collinearity)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))  +
  geom_hline(yintercept = 0.933333, linetype="dashed", color = "red")+ ylim(0.7,1) + xlab("Measurement interval (d)")
p2

library(export)
graph2ppt(aspectr = 1.5, width = 6.2)   
