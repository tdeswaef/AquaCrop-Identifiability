# AquaCrop-Identifiability
This documents the workflow used in the publication: [Coudron et al. 2021. Data collection design for calibration of crop models using practical
identifiability analysis. Computers and Electronics in Agriculture 190, 106457](https://doi.org/10.1016/j.compag.2021.106457)


## Requirements
R (version 4.0.1)  
Rstudio (version 1.2)
R packages FME, sensitivity, maptools, rgeos, plyr
Weather data in csv format (we used the [Gridded Agro-Meteorological Data in Europe from JRC](https://agri4cast.jrc.ec.europa.eu/dataportal/))
Field locations in kml format: see [example](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/01_Sensitivity/Proefvelden17.kml)

## Practical Guide
The workflow is arranged in three parts, referring to the three folders:
 - [01_Sensitivity](https://github.com/tdeswaef/AquaCrop-Identifiability/tree/main/01_Sensitivity)
 - [02_Single_Factor](https://github.com/tdeswaef/AquaCrop-Identifiability/tree/main/02_Single_Factor)
 - [03_Final_Setup](https://github.com/tdeswaef/AquaCrop-Identifiability/tree/main/03_Final_Setup)

### 01_Sensitivity
This folder contains the scripts for the sensitivity analysis done via the Morris method ([Morris 1991](https://www.tandfonline.com/doi/abs/10.1080/00401706.1991.10484804)). 
 - [Proefvelden17.kml](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/01_Sensitivity/Proefvelden17.kml) is the kml file for the locations of the foreseen experiments
 - [Soil.csv](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/01_Sensitivity/Soil.csv) is the soil input file for the Aquacrop model.
 - [Weather_data.R](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/01_Sensitivity/Weather_data.R) is the script to 
 - [Morris.R](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/01_Sensitivity/Morris.R) is the main script for sensitivity analysis
 - [rescale_SENS.R](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/01_Sensitivity/rescale_SENS.R) is the script to rescale the sensitivity functions for proper comparison and use in identifiability analysis

### 02_Single_Factor
This folder contains the scripts to run the identifiability analysis, based on the sensitivity functions developed in [01_Sensitivity](https://github.com/tdeswaef/AquaCrop-Identifiability/tree/main/01_Sensitivity) and modifying single factors in the experimental design. The results are depicted in Figure 5.
 - [Intervals.R](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/02_Single_Factor/Intervals.R) is the script to get identifiability results for multiple intervals and generate the basis of figure 4.
 - [RemoveOneOutputvariable.R](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/02_Single_Factor/RemoveOneOutputvariable.R) is the script for Figure 5A
 - [MeasureOneOutvarDaily.R](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/02_Single_Factor/MeasureOneOutvarDaily.R) is the script for Figure 5B
 - [soilcombinations.R](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/02_Single_Factor/soilcombinations.R) is the script for Figure 5C
 - [YearsCombinations.R](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/02_Single_Factor/YearsCombinations.R) is the script for Figure 5D

### 03_Final_Setup
This folder contains the script to run the identifiability analysis, based on the sensitivity functions developed in [01_Sensitivity](https://github.com/tdeswaef/AquaCrop-Identifiability/tree/main/01_Sensitivity) and modifying multiple factors as deemed feasible in the final experimental design. The results are depicted in Figure 6.
 - [FINAL_SETP.R](https://github.com/tdeswaef/AquaCrop-Identifiability/blob/main/03_Final_Setup/FINAL_SETP.R)
