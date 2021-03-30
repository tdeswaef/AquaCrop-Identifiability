library(FME)
library(sensitivity)

################
#PATHS and files
#################
AquaCrop.path <- "C:/Doctoraat/Aquacrop/" #location where the plugin is installed

AquaCrop.data <- "AquaCropV61Nr02052018/DATA/" #contains files about weather, soil, crop etc
AquaCrop.simul <- "AquaCropV61Nr02052018/SIMUL/" #under this location Mauna loa CO2 file is usually located standard when installing AquaCrop plugin
AquaCrop.list <- "LIST/" #path where project files are generated for AquaCrop to run
AquaCrop.ouput <- "OUTP/" #output path for simulations

Morris.output.path <- "C:/Doctoraat/collin/Morris/OUTPUT/" #output path for Morris sensitivity output
Soil <- read.csv("Soil.csv", sep=";") #read in soil types file

###########################
#Variables and parameters
############################

#output variables you're interested in
output <- c("Stage","Irri","Surf","Infilt",
            "RO","Drain","CR","Zgwt","Ex","E","ESEx","Trx","Tr","TrSTrx",
            "ETx","ET","ETSETx","GD","Z","StExp","StSto","StSen","StSalt",
            "CC","StTr","KcSTrS","TrW","WP","Biomass","HI","YieldPart","Brelative","WPet","WCSxSxxS","WrSxSxxS","Wr","WrSSATS","WrSFCS",
            "WrSexpS","WrSstoS","WrSsenS","WrSPWPS","SaltIn","SaltOut","SaltUp","SaltZ",
            "ECe","ECsw","StSalt","WC01","WC2","WC3","WC4","WC5",
            "WC6","ECe01","ECe2","ECe3","ECe4","ECe5","ECe6","Rain",
            "ETo","Tmin","Tavg","Tmax", "WC10", "WC030","WC3060") 

#Parameter ranges, needs to be a list of list in the Morris function
pars <- list(baseT = c(1,3), upT = c(24,28), pexup = c(0.15,0.25), pexlw = c(0.55,0.65), 
             psto = c(0.50,0.60), psen = c(0.65,0.8),rtx = c(0.35,0.6) , 
             rtm = c(0.10,0.20), rtshp = c(10,25), rtexup = c(0.05,0.090) , rtexlw = c(0.015,0.040),
             cgc = c(0.09,0.13), ccx = c(0.95,1), cdc = c(0.09,0.13), hi = c(70,95), 
             ccs = c(10,24), kc = c(1.05,1.25),  
             kcdcl = c(0.075,0.225), wp = c(17,20), anaer = c(4,8), evardc = c(40,80),  
             hipsflo = c(1,3), hingsto = c(5,15), hinc = c(2,8), stbio = c(5,9),
             plan = c(5,20), yld = c(35, 55), root = c(25,75), sen = c(80, 110), mat = c(125, 140),
             hilen = c(50,70),  pexhp = c(1.5,4.5), pstoshp = c(1.5,4.5), psenshp = c(1.5,4.5)
)

min <- sapply(pars, function(x) x[1]) #minimal values of pars for Morris
max <- sapply(pars, function(x) x[2]) #maximal values in Morris

##################
#AquaCrop function
##################

SolveAquacrop <- function(X) {
  ############
  #Parameters
  ###########
  n <- dim(X)[[1]]
  for (i in 1:n){
    for (B in names(pars)){
      assign(B, X[i,B])
    }
    ###############
    #Cropfile
    ##############
    
    filename <- paste("Potato" , i, ".CRO", sep="")
    cat('Default Potato, Calendar (Lima, 17May95)
        6.0        : AquaCrop Version (March 2017)
        1         : File protected
        3         : root/tuber crop
        0         : Crop is transplanted
        1         : Determination of crop cycle : by calendar days
        1         : Soil water depletion factors (p) are adjusted by ETo\n',
        baseT ,       ': Base temperature (°C) below which crop development does not progress\n',
        upT ,       ': Upper temperature (°C) above which crop development no longer increases with an increase in temperature
        -9            : Total length of crop cycle in growing degree-days\n',
        pexup ,       ': Soil water depletion factor for canopy expansion (p-exp) - Upper threshold\n',
        pexlw ,       ': Soil water depletion factor for canopy expansion (p-exp) - Lower threshold\n',
        pexhp ,       ': Shape factor for water stress coefficient for canopy expansion (0.0 = straight line)\n',
        psto ,       ': Soil water depletion fraction for stomatal control (p - sto) - Upper threshold\n',
        pstoshp ,       ': Shape factor for water stress coefficient for stomatal control (0.0 = straight line)\n',
        psen ,       ': Soil water depletion factor for canopy senescence (p - sen) - Upper threshold\n',
        psenshp ,       ': Shape factor for water stress coefficient for canopy senescence (0.0 = straight line)
        0         : Sum(ETo) during stress period to be exceeded before senescence is triggered\n',
        ppol ,       ': Soil water depletion factor for pollination (p - pol) - Upper threshold\n',
        round(anaer) ,       ': Vol% for Anaerobiotic point (* (SAT - [vol%]) at which deficient aeration occurs *)
        50        : Considered soil fertility stress for calibration of stress response (%)
        25.00     : Response of canopy expansion is not considered
        25.00     : Response of maximum canopy cover is not considered
        25.00     : Response of crop Water Productivity is not considered
        25.00     : Response of decline of canopy cover is not considered
        -9        : dummy - Parameter no Longer required
        -9        : Cold (air temperature) stress affecting pollination - not considered
        -9        : Heat (air temperature) stress affecting pollination - not considered\n',
        round(stbio) ,       ': Minimum growing degrees required for full crop transpiration (°C - day)
        2         : Electrical Conductivity of soil saturation extract at which crop starts to be affected by soil salinity (dS/m)
        15        : Electrical Conductivity of soil saturation extract at which crop can no longer grow (dS/m)
        -9        : Dummy - no longer applicable
        25        : Calibrated distortion (%) of CC due to salinity stress (Range: 0 (none) to +100 (very strong))
        100       : Calibrated response (%) of stomata stress to ECsw (Range: 0 (none) to +200 (extreme))\n',
        kc ,       ': Crop coefficient when canopy is complete but prior to senescence (KcTr,x)\n',
        kcdcl ,       ': Decline of crop coefficient (%/day) as a result of ageing, nitrogen deficiency, etc.\n',
        rtm ,       ': Minimum effective rooting depth (m)\n',
        rtx ,       ' : Maximum effective rooting depth (m)\n',
        round(rtshp) ,       ': Shape factor describing root zone expansion\n',
        rtexup ,       ': Maximum root water extraction (m3water/m3soil.day) in top quarter of root zone\n',
        rtexlw ,       ': Maximum root water extraction (m3water/m3soil.day) in bottom quarter of root zone\n',
        round(evardc) ,       ': Effect of canopy cover in reducing soil evaporation in late season stage\n',
        ccs ,       ': Soil surface covered by an individual seedling at 90 % emergence (cm2)\n',
        ccs ,       ': Canopy size of individual plant (re-growth) at 1st day (cm2)\n',
        30000 ,       ': Number of plants per hectare\n',
        cgc ,   ': Canopy growth coefficient (CGC): Increase in canopy cover (fraction soil cover per day)
        -9        : Maximum decrease of Canopy Growth Coefficient in and between seasons - Not Applicable
        -9        : Number of seasons at which maximum decrease of Canopy Growth Coefficient is reached - Not Applicable
        -9.0      : Shape factor for decrease Canopy Growth Coefficient - Not Applicable\n',
        ccx ,       ': Maximum canopy cover (CCx) in fraction soil cover\n',
        cdc ,     ': Canopy decline coefficient (CDC): Decrease in canopy cover (in fraction per day)\n',
        round(plan) ,       ': Calendar Days: from transplanting to recovered transplant\n',
        round(root) ,       ': Calendar Days: from transplanting to maximum rooting depth\n',
        round(sen) ,       ': Calendar Days: from transplanting to start senescence\n',
        round(mat) ,       ': Calendar Days: from transplanting to maturity\n',
        round(yld) ,       ': Calendar Days: from transplanting to start of yield formation
        0         : Length of the flowering stage (days)
        0         : Crop determinancy unlinked with flowering
        -9        : Excess of potential fruits - Not Applicable\n',
        round(hilen)  ,      ': Building up of Harvest Index starting at root/tuber enlargement (days)\n',
        wp ,       ' : Water Productivity normalized for ETo and CO2 (WP*) (gram/m2)
        100       : Water Productivity normalized for ETo and CO2 during yield formation (as % WP*)
        50        : Crop performance under elevated atmospheric CO2 concentration (%)\n',
        round(hi) ,       ': Reference Harvest Index (HIo) (%)\n',
        round(hipsflo) ,       ': Possible increase (%) of HI due to water stress before start of yield formation
        -9.0      : No impact on HI of restricted vegetative growth during yield formation\n',
        hingsto ,       ': Coefficient describing negative impact on HI of stomatal closure during yield formation\n',
        round(hinc) ,       ': Allowable maximum increase (%) of specified HI
        -9                        : GDDays: from transplanting to recovered transplant
        -9                        : GDDays: from transplanting to maximum rooting depth
        -9                        : GDDays: from transplanting to start senescence
        -9                        : GDDays: from transplanting to maturity
        -9                        : GDDays: from transplanting to start yield formation
        -9                        : Length of the flowering stage (growing degree days)
        -9                        : CGC for GGDays: Increase in canopy cover (in fraction soil cover per growing-degree day)
        -9                        : CDC for GGDays: Decrease in canopy cover (in fraction per growing-degree day)
        -9                        : GDDays: building-up of Harvest Index during yield formation', file = paste(AquaCrop.path, AquaCrop.data, filename, sep = ""), append=F, sep = " ")
    
    
    ###############
    #Projectfiles
    ##############
    
    filename <- paste(ii,"Year", Year, Soil[SOL,1], i,".PRO", sep="")
    cat('
        6.1           : AquaCrop Version (May 2018)\n',
        as.integer((Year - 1901)*365.25 + 90.5 + 1),': First day of simulation period\n',   
        as.integer((Year - 1901)*365.25 + 181.25 + 25),': Last day of simulation period\n',     
        as.integer((Year - 1901)*365.25 + 90.5 + 1),': First day of cropping period\n',  
        as.integer((Year - 1901)*365.25 + 181.25 + 25),': Last day of cropping period    
        4             : Evaporation decline factor for stage II
        1.10          : Ke(x) Soil evaporation coefficient for fully wet and non-shaded soil surface
        5             : Threshold for green CC below which HI can no longer increase (% cover)
        70            : Starting depth of root zone expansion curve (% of Zmin)
        5.00          : Maximum allowable root zone expansion (fixed at 5 cm/day)
        -6            : Shape factor for effect water stress on root zone expansion
        20            : Required soil water content in top soil for germination (% TAW)
        1.0           : Adjustment factor for FAO-adjustment soil water depletion (p) by ETo
        3             : Number of days after which deficient aeration is fully effective
        1.00          : Exponent of senescence factor adjusting drop in photosynthetic activity of dying crop
        12            : Decrease of p(sen) once early canopy senescence is triggered (% of p(sen))
        10            : Thickness top soil (cm) in which soil water depletion has to be determined
        30            : Depth [cm] of soil profile affected by water extraction by soil evaporation
        0.30          : Considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment
        1             : CN is adjusted to Antecedent Moisture Class
        20            : salt diffusion factor (capacity for salt diffusion in micro pores) [%]
        100           : salt solubility [g/liter]
        16            : shape factor for effect of soil water content gradient on capillary rise
        12.0          : Default minimum temperature (°C) if no temperature file is specified
        28.0          : Default maximum temperature (°C) if no temperature file is specified
        3             : Default method for the calculation of growing degree days
-- 1. Climate (CLI) file\n', file = paste(AquaCrop.path, AquaCrop.list, filename, sep = ""), append=F)
    cat('      ', ii,'.CLI\n',
        AquaCrop.path, AquaCrop.data,'\n', file = paste(AquaCrop.path, AquaCrop.list, filename, sep = ""), append=T, sep = "")
    cat('      1.1 Temperature (Tnx or TMP) file
        ', ii,'.TMP\n',
        AquaCrop.path, AquaCrop.data,'\n', file = paste(AquaCrop.path, AquaCrop.list, filename, sep = ""), append=T, sep = "")
    cat('      1.2 Reference ET (ETo) file
        ', ii,'.ETo\n',
        AquaCrop.path, AquaCrop.data,'\n', file = paste(AquaCrop.path, AquaCrop.list, filename, sep = ""), append=T, sep = "")
    cat('      1.3 Rain (PLU) file
        ', ii,'.Plu\n',
        AquaCrop.path, AquaCrop.data,'\n', file = paste(AquaCrop.path, AquaCrop.list, filename, sep = ""), append=T, sep = "")
    cat('      1.4 Atmospheric CO2 concentration (CO2) file
        MaunaLoa.CO2\n',
        AquaCrop.path, AquaCrop.simul, file = paste(AquaCrop.path, AquaCrop.list, filename, sep = ""), append=T)
    cat('-- 2. Crop (CRO) file
        Potato',i,'.CRO\n', file = paste(AquaCrop.path, AquaCrop.list, filename, sep = ""), append=T, sep = "")
    cat(AquaCrop.path, AquaCrop.data,'
          -- 3. Irrigation management (IRR) file
        (None)
        (None)
-- 4. Field management (MAN) file
        (None)
        (None)
-- 5. Soil profile (SOL) file\n', file = paste(AquaCrop.path, AquaCrop.list, filename, sep = ""), append=T)
    cat('      ', paste(Soil[SOL,1], sep = ""), '.SOL\n',
        AquaCrop.path, AquaCrop.data,'\n', file = paste(AquaCrop.path, AquaCrop.list, filename, sep = ""), append=T, sep = "")
    cat('-- 6. Groundwater table (GWT) file
        (None)
        (None)
-- 7. Initial conditions (SW0) file
        (None)
        (None)
-- 8. Off-season conditions (OFF) file
        (None)
        (None)', file = paste(AquaCrop.path, AquaCrop.data.list, filename, sep = ""), append=T)
  }
    ##############################
    #Run aquacrop and read output
    ##############################
    ######
  
  system(paste(AquaCrop.path,"ACsaV60.exe",sep = "")) #Aquacrop plugin checks and runs all project files in the LIST folder
  unlink(paste(AquaCrop.path, AquaCrop.list,"*",sep = "")) #delete project files from this list
    
  #read all the output files and put data in an array with 3 dimensions (dimensions for number of simulations, for number of days, and for number of outputs of the model)

  n <- dim(X)[[1]]
  for (i in 1:n){
      readfile <- paste(AquaCrop.path, AquaCrop.ouput, ii, "Year", Year, paste(Soil[SOL,1], sep = ""), i ,"PROday.OUT", sep="")
      out <- read.table(readfile, header=FALSE, skip = 4, col.names = c("Day","Month","Year","DAP","Stage","WC(x.xx)","Rain","Irri","Surf","Infilt",
                                                                        "RO","Drain","CR","Zgwt","Ex","E","E/Ex","Trx","Tr","Tr/Trx",
                                                                        "ETx","ET","ET/ETx","GD","Z","StExp","StSto","StSen","StSalt","StWeed",
                                                                        "CC","CCw","StTr","Kc(Tr)","Trx","Tr","TrW","Tr/Trx","WP","Biomass",
                                                                        "HI","YieldPart","Brelative","WPet","WC(x.xx)","Wr(x.xx)","Z","Wr","Wr(SAT)","Wr(FC)",
                                                                        "Wr(exp)","Wr(sto)","Wr(sen)","Wr(PWP)","SaltIn","SaltOut","SaltUp","Salt(x.xx)","SaltZ","Z",
                                                                        "ECe","ECsw","StSalt","Zgwt","ECgw","WC01","WC2","WC3","WC4","WC5",
                                                                        "WC6","WC7","WC8","WC9","WC10","WC11","WC12","ECe01","ECe2","ECe3",
                                                                        "ECe4","ECe5","ECe6","ECe7","ECe8","ECe9","ECe10","ECe11","ECe12","Rain",
                                                                        "ETo","Tmin","Tavg","Tmax","CO2"), check.names = F)
      
      out <- out[, unique(colnames(out))] #delete duplicated columns
      names(out) <- gsub("[()/.]", "S", names(out))
      out$WC10 <- (out$WC01 + out$WC2)/2
      out$WC030 <- (out$WC01 + out$WC2 + out$WC3)/3 #make a variable WC030, average of first 3 depths WC
      out$WC3060 <- (out$WC4 + out$WC5 + out$WC6)/3
      
      n <- dim(X)[[1]]
        if (i == 1){
          OP <- array(NA,c(n,nrow(out),length(output)))
        }
      
          for (OT in c(1:length(output))){
            OP[i,,OT] <- out[,grep(paste("\\b",output[OT],"\\b", sep = ""), names(out))]
            }
  }

    unlink(paste(AquaCrop.path, AquaCrop.ouput,"*",sep = "")) #remove output for next simulations
    return(OP)
  }
  #######

for (ii in names(JRClist[1])){
  for (Year in c(1975:2018)){
    for (SOL in c(4)){ #nrow(Soil)
      unlink(paste(AquaCrop.path, AquaCrop.data,"*"),sep = "")
        ###############
        #Soilfile
        ###############
          filename <- paste(Soil[SOL,1],".SOL", sep="")
          cat('deep uniform very sandy soil profile
              6.0                 : AquaCrop Version (March 2017)\n',
              Soil[SOL,11],'       : CN (Curve Number)\n',
              Soil[SOL,12],'        : Readily evaporable water from top layer (mm)
              1                   : number of soil horizons
              -9                  : variable no longer applicable
              Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel   CRa        CRb           description
              ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------\n',
              Soil[SOL,2],Soil[SOL,3],Soil[SOL,4] ,Soil[SOL,5],Soil[SOL,6],Soil[SOL,7],Soil[SOL,8],Soil[SOL,9],Soil[SOL,10] ,paste(Soil[SOL,1], sep = " "), file = paste(AquaCrop.path, AquaCrop.data, filename, sep = ""), append=F, sep = "    ")
          #######
        ###############
        #Climatefiles
        ##############
        filename <- paste(ii,".TMP", sep="")
        cat('
            1  : Monthly records (1=daily, 2=10-daily and 3=monthly data)
            1  : First day of record (1, 11 or 21 for 10-day or 1 for months)
            1  : First month of record
            1975  : First year of record (1901 if not linked to a specific year)
            
            TMPmin TMPmax
            ============================\n', file = paste(AquaCrop.path, AquaCrop.data, filename, sep = ""), append=F)
        write.table(JRClist[[ii]][,c(7,6)], paste(AquaCrop.path, AquaCrop.data, filename, sep = ""), col.names=FALSE,row.names=FALSE,sep="\t",quote=FALSE, append = T)
        
        filename <- paste(ii, ".Plu", sep="")
        cat('
            1  : Monthly records (1=daily, 2=10-daily and 3=monthly data)
            1  : First day of record (1, 11 or 21 for 10-day or 1 for months)
            1  : First month of record
            1975  : First year of record (1901 if not linked to a specific year)
            
            Total Rain (mm)
            ============================\n', file = paste(AquaCrop.path, AquaCrop.data, filename, sep = ""), append=T)
        write.table(JRClist[[ii]][,c(11)], paste(AquaCrop.path, AquaCrop.data, filename, sep = ""), col.names=FALSE,row.names=FALSE,sep="\t",quote=FALSE, append = T)
        
        filename <- paste(ii, ".ETo", sep="")
        cat('
            1  : Monthly records (1=daily, 2=10-daily and 3=monthly data)
            1  : First day of record (1, 11 or 21 for 10-day or 1 for months)
            1  : First month of record
            1975  : First year of record (1901 if not linked to a specific year)
            
            Average ETo (mm/day)
            ============================\n', file = paste(AquaCrop.path, AquaCrop.data, filename, sep = ""), append=T)
        write.table(round(JRClist[[ii]][,13],1), paste(AquaCrop.path, AquaCrop.data, filename, sep =""), col.names=FALSE,row.names=FALSE,sep="\t",quote=FALSE, append = T)
        
        filename <- paste(ii, ".CLI", sep="")
        cat('Fields in Flanders 1Jan 1975 - 31Dec 2018
            6.1   : AquaCrop Version (May 2018)\n',
            ii,'.TMP\n',
            ii,'.ETo\n',
            ii,'.Plu\n',
            'MaunaLoa.CO2', file = paste(AquaCrop.path, AquaCrop.data, filename, sep = ""), append=F, sep = "")
                  
        #############
        #Morrismethod
        #############
        Morris <- morris(model = SolveAquacrop, factors = names(pars), r = 100, binf = min, bsup = max,
                         design = list(type = "oat", levels = 8, grid.jump = 1))

        filename <- paste(Morris.output.path,"Morris",ii,"Year", Year, Soil[SOL,1],".rds", sep = "")
        saveRDS(Morris, file = filename) #save as rds
        
  }
}
}

