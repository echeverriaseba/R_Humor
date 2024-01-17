
############# PCA Tests - Sophia's Data ##############

library(vegan)
install.packages("vioplot")
library(vioplot)

# Note: vegan package has issues with missing values, because it deletes the rows.

Sites <- read.csv("data/1_Sites.csv", fileEncoding="latin1", na.strings=c("","NA"))
Nutrients <- read.csv("data/1_Nutrients.csv", fileEncoding="latin1", na.strings=c("","NA"))

# 1. Data exploration ####

## 1.1. Outlier identification

variables <- names(Nutrients) # we start workig with the Nutrients dataframe
print(variables)

keep <- c( "NH4.ugL", "NO3.ugL", "NO2.ugL", "PO4.ugL", "SiO2.ugL") # selecting variables we should include
variables <- variables[variables %in%  keep]
print(variables)

for (P in variables) {print(P)} # example of looping process
for (P in variables) {print(Nutrients[,P])}
for (P in variables) {print(Nutrients[,P, drop = FALSE])} # The "drop" argument switches from vector to column print
for (P in variables) {hist(Nutrients[,P], main = P, brakes = 6)
                      boxplot(Nutrients[,P], main = P, xlab = P)
                      vioplot(Nutrients[,P], main = P, xlab = P) # vioplot: gives info about where points are concentrated (e.g. 2 peaks)
                      } # Histograms of all variables in "keep"

# Note: with these plots we decide to do a logarithm. No variable has to be excluded.  

## 1.2. Data transformation ####

Nut <- NULL # creating an empty object, then to be filled. This will then be our new transformed dataframe

for (P in variables) {log.P <- log(Nutrients[,P])
                      Name <- paste0("log.",P)
                      hist(log.P, main = Name, xlab = Name)
                      boxplot(log.P, main =Name, xlab = Name)
                      vioplot(log.P, main = Name, xlab = Name)
                      COL <- colnames(Nut)
                      Nut <- cbind(Nut, log.P) # matrix, not dataframe, only has numbers
                      colnames(Nut) <- c(COL, Name) # instead of names(), 
}


