
############# PCA Tests - Sophia's Data ##############

library(vegan)
library(vioplot)

# Meeting 17.01.2024 - Initial PCA work ####

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

Nut <- NULL # creating an empty object, then to be filled with the loop below. This will then be our new transformed dataframe.

pdf("outputs/Sophia/Sophia_nuts_dataExploration.pdf")

for (P in variables) {log.P <- log(Nutrients[,P]) # creates a vector for each variable with all values log transformed.
                      Name <- paste0("log.",P) # adds "log." as a prefix to each column's name
                      hist(log.P, main = Name, xlab = Name)
                      boxplot(log.P, main =Name, xlab = Name)
                      vioplot(log.P, main = Name, xlab = Name)
                      COL <- colnames(Nut) # because when defining log.P, a vector was created, and column neames is lost. This recover them.
                      Nut <- cbind(Nut, log.P) # matrix, not dataframe, only has numbers
                      colnames(Nut) <- c(COL, Name) # instead of names(), because names() works for dataframes and Nut is a matrix.
}

dev.off()

# 2. Building up a PCA ####

PCA <- rda(Nut, scale = TRUE) # Correspondence Analysis and Redundancy Analysis. Scale = TRUE: Defines Correlations instead of Covariance.

pdf("outputs/Sophia/Sophia_PCA.pdf")

biplot(PCA) # prints the PCA as a plot. Plots in red variables and in black the sites.

dev.off()

print(PCA) # returns the Eigenvalues
summary(PCA) # Information about the Importance of components, look for: how to interpret this table.


# Meeting: 01.02.2024 - Including factors to PCA ####

## We use Invasive species as an example:

print(names(PCA)) #To access to all the stuff that has the PCA, the summary doesn't give you the same information
PCA[1] #Para decirle cual de todas esas cosas quieres que te muestre, if you ask for the result of "CA" give you all the scores but is also in the summary of the PCA. You can use the names but it is better to use the name of the function
PCA[8]
PCA$CA$u[,c(1,2)]
PLOT<-cbind(sites,PCA$CA$u[,c(1,2)])
PLOT$River#you need to convert the levels to a factor so there are no longer a text but a factor and then you convert them to a numeric variable so you can order by the number asigned
as.factor(PLOT$River)
CALL<-as.numeric(as.factor(PLOT$River))
plot(PLOT$PC1,PLOT$PC2,col=CALL)
plot(PLOT$PC1,PLOT$PC2,col=PLOT$Invasive.spe)
legend(0.2,-0.3,legend=c("non-invasive","invasive"),col=PLOT$Invasive.spe)

# Identifying points in the PCA:

windows(open=TRUE)
dev.new()
plot(PLOT$PC1,PLOT$PC2,col=PLOT$Invasive.spe)
identify(PLOT$PC1,PLOT$PC2,label=PLOT$Invasive.spe)#to identify if its invasive or non-invasive
plot(PLOT$PC1,PLOT$PC2,col=PLOT$River)
identify(PLOT$PC1,PLOT$PC2,label=PLOT$River,plot=TRUE)

# Note: When working with plots it works with layers, if you want to give more importance to one object vs another you need to write it in the 
# correct order printing one layer before another because it creates layers over the one printed before.
# Basic R plot options: i) plot for scatter plot, ii) histogram for bars, iii) Boxplot

CALL<-PLOT$Invasive.spe+1
plot(PLOT$PC1,PLOT$PC2,col=CALL)

