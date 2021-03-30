graphics.off()
rm(list = ls())
options(error=stop)

library(FME)
library(plyr)

#######
#PATHS
#######

files <- list.files(path="C:/Doctoraat/Irrigatie2.0/collin/Morris/MorrisPotato/", pattern="*.rds", full.names=TRUE)
output_folder <- "C:/Doctoraat/Irrigatie2.0/collin/Morris/MorrisPotatoS95MAX/"

#parset order
#baseT1, upT2, pexup3, pexlw4, psto5, psen6,rtx7 ,rtm8, rtshp9, rtexup10, rtexlw11,cgc12, ccx13, cdc14, hi15, 
#ccs16, kc17,kcdcl18, wp19, anaer20, evardc21, hipsflo22, hingsto23, hinc24, stbio25,plan26, yld27, root28, sen29, mat30,
#hilen31,  pexhp32, pstoshp33, psenshp34

#order of output variables
#"Stage1","Irri2","Surf3","Infilt4",
#"RO5","Drain6","CR7","Zgwt8","Ex9","E10","ESEx11","Trx12","Tr13","TrSTrx14",
#"ETx15","ET16","ETSETx17","GD18","Z19","StExp20","StSto21","StSen22","StSalt23",
#"CC24","StTr25","KcSTrS26","TrW27","WP28","Biomass29","HI30","YieldPart31","Brelative32","WPet33","WCSxSxxS34","WrSxSxxS35","Wr36","WrSSATS37","WrSFCS38",
#"WrSexpS39","WrSstoS40","WrSsenS41","WrSPWPS42","SaltIn43","SaltOut44","SaltUp45","SaltZ46",
#"ECe47","ECsw48","StSalt49","WC01 50","WC2 51","WC3 52","WC4 53","WC5 54",
#"WC6 55","ECe01 56","ECe2 57","ECe3 58","ECe4 59","ECe5 60","ECe6 61","Rain 62",
#"ETo 63","Tmin 64","Tavg 65","Tmax 66", "WC10 67", "WC030 68","WC3060 69"

#average values of parameter ranges
pa <- c(2, 26, 0.2, 0.6, 0.55, 0.725, 0.475, 0.15, 17.5, 0.07, 0.0275, 0.11, 0.975, 0.11, 82.5, 17, 1.15, 
        0.14875, 18.5, 6, 60, 2, 10, 5, 7, 12.5, 45, 50, 95, 132.5, 60, 3, 3, 3)

#########################################################################################
#Rescale with 95 percent max value of the simulation output and average paramater values
#########################################################################################
outal <- NULL
outavs <- NULL
for (i in 1:6){
  for (f in 1:length(files)){
    morris <- readRDS(files[f])
    outvs <- morris$y[,,c(1,24,29,31,68,69)]
    outal[f] <- max(outvs[,,i])/100*95
    outavs[i] <- max(outal) 
  }
}

for (f in 1:length(files)){
  morris <- readRDS(files[f])
  parscaled <- array(NA, dim = c(100,34,115,6))
  for (i in 1:34){
    parscaled[,i,,] <- morris$ee[,i,1:115,c(1,24,29,31,68,69)]*pa[[i]] 
  }
  
  varparscaled <- array(NA, dim = c(100,34,115,6))
  for (i in 1:6){
    varparscaled[,,,i] <- parscaled[,,1:115,i]/outavs[i]
  }
  filename <- paste(ouput_folder,substring(files[f],75), sep = "")
  saveRDS(varparscaled, filename)
}

