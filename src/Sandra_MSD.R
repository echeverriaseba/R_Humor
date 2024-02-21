
# R Meeting: 21.02.24


#Data prep for MA temporal data CIGUARISK#########

#Data creation 19/02/2023
#Use: Script for the preparation of the global excel of sampling 
#Necessary excels-->IT WILL CHANGE AS THE ENTIRE EXCELS EVOLVES
##merged_samp_nut_HOBOS_vol_10_OK

#Packages needed####

library(tidyverse)
library(xlsx)
library(xlsxjars)
library(rJava)
library(Rtools)
library(readxl)
library(ggplot2)
library(sciplot)
library(writexl)
library(vioplot)
library(vegan)
library(dplyr)
library(reshape2)
library(vioplot)
library(vegan)

# Sandra's base dataframe:

merged_samp_nut_HOBOS_vol_10_OK <- readRDS("data/all_data_clean.rds")

#1. MA temporal Data extraction of all the MA 6 and 10 macroalgae sample fixed with lugol####

all_MA_temporal <- merged_samp_nut_HOBOS_vol_10_OK[merged_samp_nut_HOBOS_vol_10_OK$Type_sample == "LUG"&merged_samp_nut_HOBOS_vol_10_OK$Substratum 
                                                   == "macroalgae"&merged_samp_nut_HOBOS_vol_10_OK$Island == 
                                                     "MA"&(merged_samp_nut_HOBOS_vol_10_OK$Point == 6 | merged_samp_nut_HOBOS_vol_10_OK$Point == 10)
                                                   &(merged_samp_nut_HOBOS_vol_10_OK$Objective == 0 | merged_samp_nut_HOBOS_vol_10_OK$Objective == 2),
                                                   ] #only with macroalgae

## 1.1.Environmental global data exploration field ####

# variable.names(all_MA_temporal)
# variables_orig<-names(all_MA_temporal)
# keep<-c("Temp_field","Salinity_field","Dis_O2_perc_field","Dis_O2_mg_field","pH_field","NH4","NO3","NO2","PO4","SiO2","DayofAvgTemp","DayofTempSD","DayofTempSEM","DayofTempMax","DayofTempMin",   
#         "30TempAvg", "30TempSEM", "30TempSD", "21TempAvg", "21TempSEM", "21TempSD", "14TempAvg", "14TempSEM", "14TempSD", "7TempAvg", "7TempSEM", "7TempSD",
#         "DayofAvgLight", "DayofLightSD","DayofLightSEM","DayofLightMax","DayofLightMin","30LightAvg", "30LightSEM", "30LightSD", "21LightAvg", "21LightSEM", "21LightSD", "14LightAvg", 
#         "14LightSEM", "14LightSD", "7LightAvg", "7LightSEM", "7LightSD")
# 
# variables_orig<-variables_orig[variables_orig%in%keep]
# pdf("output/MA temporal_field.pdf")
# for(P in variables_orig){print(all_MA_temporal[,P])} #for recall and create a vector called P with my variables
# for(P in variables_orig){boxplot(all_MA_temporal[,P],main=P,xlab=P)
#   vioplot(all_MA_temporal[,P],main=P,xlab=P)}
# dev.off()


## 1.2. HOBO data: ####

variables_HOBOS<-names(all_MA_temporal)
keep_HOBOS<-c("Temp_field","Salinity_field","Dis_O2_perc_field","Dis_O2_mg_field","pH_field","DayofAvgTemp","DayofTempSD","DayofTempSEM","DayofTempMax","DayofTempMin",   
              "30TempAvg", "30TempSEM", "30TempSD", "21TempAvg", "21TempSEM", "21TempSD", "14TempAvg", "14TempSEM", "14TempSD", "7TempAvg", "7TempSEM", "7TempSD",
              "DayofAvgLight", "DayofLightSD","DayofLightSEM","DayofLightMax","DayofLightMin","30LightAvg", "30LightSEM", "30LightSD", "21LightAvg", "21LightSEM", "21LightSD", "14LightAvg", 
              "14LightSEM", "14LightSD", "7LightAvg", "7LightSEM", "7LightSD")
variables_HOBOS<-variables_HOBOS[variables_HOBOS%in%keep_HOBOS]

# Example loop:

pdf("outputs/Sandra/MA temporal_field with HOBOS.pdf")
for (P in variables_HOBOS) {
  # Remove rows with NA in the "DayofAvgTemp" column
  filtered_DayofAvgTemp_nona <- all_MA_temporal %>% filter(!is.na(DayofAvgTemp))
  # Create boxplot
  hist(filtered_DayofAvgTemp_nona[,P], main=P,xlab=P)
  boxplot(filtered_DayofAvgTemp_nona[, P], main = P, xlab = P)
  vioplot(filtered_DayofAvgTemp_nona[, P], main = P, xlab = P)
}
dev.off()

# 2. PCA environmentals removing NA rows ####

summary(all_MA_temporal)

filtered_data_PCA_HOBOS <- all_MA_temporal %>% 
  filter(!is.na(DayofAvgTemp) & !is.na(`30TempAvg`)&!is.na(pH_field)) %>% 
  select(all_of(keep_HOBOS))

summary(filtered_data_PCA_HOBOS)

PCA<-rda(filtered_data_PCA_HOBOS,scale=TRUE)
print(PCA)
summary(PCA)
biplot(PCA)
text(PCA, display = "species", cex = 0.7)

# 3. ABUNDANCES information####

#nMDS

cell_abundances <- names(all_MA_temporal)
keep_abundances <- c("Label","Island", "Point","Date","Day", "Month", "Year","Macrophyte", "Gamb_macro", "Fuku_macro", "Ostreop_macro", "Proro_macro", "Coolia_macro")
cell_abundances <- cell_abundances[cell_abundances %in% keep_abundances]

# 4. R_Humor meeting ####
# Note: until here this script is Sandra's original. Now onwards it's meeting's work.

# gsub() Function: look for characters or names and then replace for new names.

cell_abundances <- gsub("<LOQ", 0, cell_abundances) # Replacing all <LOQ for zeros. This could be applied to specific columns as well.







