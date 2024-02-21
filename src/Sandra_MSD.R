
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
# library(Rtools)
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

variables_HOBOS <- names(all_MA_temporal)
keep_HOBOS <- c("Temp_field","Salinity_field","Dis_O2_perc_field","Dis_O2_mg_field","pH_field","DayofAvgTemp","DayofTempSD","DayofTempSEM","DayofTempMax","DayofTempMin",   
              "30TempAvg", "30TempSEM", "30TempSD", "21TempAvg", "21TempSEM", "21TempSD", "14TempAvg", "14TempSEM", "14TempSD", "7TempAvg", "7TempSEM", "7TempSD",
              "DayofAvgLight", "DayofLightSD","DayofLightSEM","DayofLightMax","DayofLightMin","30LightAvg", "30LightSEM", "30LightSD", "21LightAvg", "21LightSEM", "21LightSD", "14LightAvg", 
              "14LightSEM", "14LightSD", "7LightAvg", "7LightSEM", "7LightSD")
variables_HOBOS<-variables_HOBOS[variables_HOBOS%in%keep_HOBOS]

# Example loop:

# pdf("outputs/Sandra/MA temporal_field with HOBOS.pdf")
for (P in variables_HOBOS) {
  # Remove rows with NA in the "DayofAvgTemp" column
  filtered_DayofAvgTemp_nona <- all_MA_temporal %>% filter(!is.na(DayofAvgTemp))
  # Create boxplot
  hist(filtered_DayofAvgTemp_nona[,P], main=P,xlab=P)
  boxplot(filtered_DayofAvgTemp_nona[, P], main = P, xlab = P)
  vioplot(filtered_DayofAvgTemp_nona[, P], main = P, xlab = P)
}
# dev.off()

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

# nMDS:

cell_abundances <- all_MA_temporal
keep_abundances <- c("Label","Island", "Point","Date","Day", "Month", "Year","Macrophyte", "Gamb_macro", "Fuku_macro", "Ostreop_macro", "Proro_macro", "Coolia_macro")
cell_abundances <- cell_abundances[names(cell_abundances) %in% keep_abundances]

# 4. R_Humor meeting - nMDS ####
# Note: until here this script is Sandra's original. Now onwards it's meeting's work.
# PCA assumes linear relation between variables, using Pearson. nMDS deals better with communities, doesn't assume these linear relations. It is also common to perform a CA after an nMDS. 

cell_abundances <- cell_abundances[!is.na(cell_abundances$Gamb_macro),]

labels <- c("Gamb_macro", "Fuku_macro", "Ostreop_macro", "Proro_macro", "Coolia_macro")

# gsub() Function: look for characters or names and then replace for new names.

for (L in labels) {
  cell_abundances[,L] <- gsub("<LOQ", 0, cell_abundances[,L])
  cell_abundances[,L] <- gsub("no cuento", NA, cell_abundances[,L])
  cell_abundances[,L] <- as.numeric(cell_abundances[,L])                          
} # Replacing all <LOQ for zeros. This could be applied to specific columns as well.

# An nMDS compares communities and species within these communities simultaneously. 
# First we have to define distances. Euclidean is the most common for this cases, gives same weights to all distances. Others are "dissimilarities" (gives more weight to those species not shared among communities) and "similarities", the other way around. This depends on your objective, for example if we are looking for common patterns of fish species all across Europe then we should use "similarities". If we think species are not homogenously distributed then maybe its better to use dissimilarities to identify what are the differences and sampling points that are different.

# Calculate Euclidean dissimilarity matrix and MSD:
dis_euc <- vegdist(cell_abundances[,names(cell_abundances) %in% labels], method = "euclidean", binary = FALSE, na.rm = TRUE)
mds_euc <- metaMDS(na.omit(cell_abundances[, names(cell_abundances) %in% labels]), autotransform = FALSE, distance = "euclidean", na.rm = TRUE, k = 2, trymax = 99)

plot(mds_euc)

PLOT <- cbind(na.omit(cell_abundances[, names(cell_abundances) %in% labels]), mds_euc$points) # making a matrix with our original cell_abundances matrix and adding the scores
PLOT <- merge(cell_abundances[, !names(cell_abundances) %in% labels], PLOT, by = "row.names")

names(mds_euc)

mds_euc$points # Points are the "scores" of each sampling
mds_euc$species # With these coordinates ("scores" of species) we can assign species to the red points within the plot. Now we can analyse which are the species most dominant in all sampled communities. 

# Note: now it's possible to go further with a trajectory analysis.
plot(PLOT$Month)
plot(PLOT$MDS1, PLOT$Month)
plot(PLOT$MDS1, PLOT$MDS2, col = PLOT$Month, pch = 19)

sddfj