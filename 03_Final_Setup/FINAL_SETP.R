graphics.off()
rm(list = ls())
options(error=stop)


#######
#PATHS
#######
output.path.collin <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/Induce_Variation/FINAL95MAX/Collin_2year_3soil_normal_sentinel/" #location where collinearity output is saved
scaled_path <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/MorrisPotatoS95MAX/" #location where scaled sensitivity functions are saved
combsens_path <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/Induce_Variation/FINAL95MAX/SENS_3year_3soil/" #location for combined sensitivity functions

files <- list.files(path=scaled.path, pattern="*.rds", full.names=TRUE)
chrs <- nchar(combsens_path)

############################################################################
#Combine sensitivity functions for different numbers of years and soil types
############################################################################

files <- list.files(path=scaled_path, pattern="*.rds", full.names=TRUE)
parset <- c(7,26,12,27,16,28,9,29,32,19,17,14,4,15,31,3,6,8)

library(abind)

combs <- NULL

sls <- c("Loamysand", "Loam", "Clayloam")

for (i in 1){
  for(year in c(1975:2018)){
    Yearstwo <- NULL
    morris <- readRDS(paste(scaled_path,year,sls[i],".rds", sep = ""))
    morris2 <- readRDS(paste(scaled_path,year,sls[i+1],".rds", sep = ""))
    morris3 <- readRDS(paste(scaled_path,year,sls[i+2],".rds", sep = ""))
    morris4 <- readRDS(paste(scaled_path,year+1,sls[i],".rds", sep = ""))
    morris5 <- readRDS(paste(scaled_path,year+1,sls[i+1],".rds", sep = ""))
    morris6 <- readRDS(paste(scaled_path,year+1,sls[i+2],".rds", sep = ""))
    morris7 <- readRDS(paste(scaled_path,year+2,sls[i],".rds", sep = ""))
    morris8 <- readRDS(paste(scaled_path,year+2,sls[i+1],".rds", sep = ""))
    morris9 <- readRDS(paste(scaled_path,year+2,sls[i+2],".rds", sep = ""))
    combs <- abind(morris2[,parset,1:115,], morris[,parset,1:115,],morris3[,parset,1:115,],morris4[,parset,1:115,],morris5[,parset,1:115,],morris6[,parset,1:115,],morris7[,parset,1:115,],morris8[,parset,1:115,],morris9[,parset,1:115,],along = 3)
    saveRDS(combs, file = paste(combsens_path,year ,year+1,year+2,sls[i],sls[i+1], sls[i+2],".rds", sep = ""))
  }
  
}

###########################################
#Calculate collinearities for a certain setup
###########################################

library(FME)
library(plyr)

files <- list.files(path=combsens_path, pattern="*.rds", full.names=TRUE)

#parset you want to calculate collinearity from
#order of parset: baseT1, upT2, pexup3, pexlw4, psto5, psen6,rtx7 ,rtm8, rtshp9, rtexup10, rtexlw11,cgc12, ccx13, cdc14, hi15, 
#ccs16, kc17,kcdcl18, wp19, anaer20, evardc21, hipsflo22, hingsto23, hinc24, stbio25,plan26, yld27, root28, sen29, mat30,
#hilen31,  pexhp32, pstoshp33, psenshp34

whatdays1 <- seq(1,115,   by = 14)
whatdays2 <- seq(116,230, by = 14)
whatdays3 <- seq(231,345, by = 14)
whatdays4 <- seq(346,460, by = 14)
whatdays5 <- seq(461,575, by = 14)
whatdays6 <- seq(576,690, by = 14)
whatdays7 <- seq(691,805, by = 14)
whatdays8 <- seq(806,920, by = 14)
whatdays9 <- seq(921,1035, by = 14)

whatdays <- c(whatdays1, whatdays2, whatdays3, whatdays4, whatdays5,whatdays6)#, whatdays7, whatdays8,whatdays9)#, whatdays3)

out <- NULL
for (i in 1:length(files)){#length(files)
  out <- NULL
  morris <- readRDS(files[[i]]) #read in file by file
  for(r in 1:100){ 
    #select different slices from the 4 dimensional morris elementary effects output array to calculate collinearity from
    Yield <-   morris[r,,whatdays,4]
    CC <-      morris[r,,seq(1,690, by = 10),2]#seq(1,690, by = 10)
    Biomass <- morris[r,,whatdays,3]
    WC030 <-   morris[r,,1:690,5]
    WC3060 <-  morris[r,,1:690,6]
    Stage <-   morris[r,,whatdays,1]
    
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
    Coll$year <- paste(substring(substring(files[i],chrs+1),1,4),substring(substring(files[i],chrs+5),1,4), sep = "")
    Coll$replicate <- r
    out <- rbind.fill(Coll, out)
    
    if (r == 100){
      filename <- paste(output.path.collin,paste(substring(substring(files[i],chrs+1),1,4),substring(substring(files[i],chrs+5),1,4), sep = ""),gsub('[[:digit:]]+', '', substr(substring(files[i],chrs+1),1,nchar(substring(files[i],chrs+1))-4)),".rds", sep = "")
      saveRDS(out, filename)
    }
  }
}

################################################
#Plot the results of the collinearity experiment
################################################

library(plyr)
Noll <- NULL

for (i in c(1975:2018)){
      
  filename <- paste("C:/Doctoraat/Irrigatie2.0/collin/Morris/Induce_Variation/FINAL95MAX/Collin_2year_3soil_alternate/",i,i+1,"LoamysandLoamClayloam",".rds", sep = "")
      
  if(file.exists(filename)) {
    file <- readRDS(filename)
    dat <- file[which(file$N == 18),]
    dat$type <- "2Y_3S_alt"
    Noll <- rbind.fill(Noll, dat)
  }
}

library(ggplot2)

library(ggthemes)
theme_set(theme_few(base_size=14))

x1 = factor(Noll$type, levels=c("WC030","WC3060","CC","Biomass","Yield","Stage"))
p3 <- ggplot(data = Noll, aes(x=type, y=(1-1/collinearity))) + geom_boxplot(aes(fill = type)) + theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept = 0.933333, linetype="dashed", color = "red") + xlab("") +#+ ylim(0,1)#scale_y_continuous(trans = "log10")
  scale_fill_manual(values = c( "#00AFBB","#0066FF", "gold1","#E7B800", "#00AFBB","#0066FF", "gold1","#E7B800")) +
  scale_colour_manual(values = c( "#00AFBB","#0066FF", "gold1","#E7B800", "#00AFBB","#0066FF", "gold1","#E7B800")) + theme(legend.position='none') + 
  scale_x_discrete(labels=c("2Y_3S_alt" = "Alternate", "2Y_3S_alt_sent" = "Alternate + Sentinel", "2Y_3S_norm" = "Same days", "2Y_3S_norm_sent" = "Same days + sentinel", "3Y_3S_alt" = "Alternate", "3Y_3S_alt_sent" = "Alternate + Sentinel", "3Y_3S_norm" = "Same days", "3Y_3S_norm_sent" = "Same days + sentinel"))

p3

library(export)
graph2ppt(aspectr = 1, width = 6.2) 
