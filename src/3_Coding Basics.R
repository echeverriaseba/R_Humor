#############   21st March 2024   ##############
############# Today: Coding Basics ##############   


## General notes
#  choose general names for objects! Then you do not to modify too much when using the script for different data sets
#  


## Apply - used for summaries ####

temp <- X1_Nutrients

apply(temp, 1, mean) # error because not just numeric data

apply(temp[,5:8], 1, mean) #selecting only numeric columns
# mean of each of selected rows

apply(temp[,5:8], 2, mean) # mean of each of selected columns

apply(temp[,5:8], 2, sd) # standard deviation of each of selected columns

apply(temp[,5:8], 2, se) # Error because standard error is not a basic function

apply(temp[,5:8], 2, function(x){sd(x)/sqrt(length(x))})  # SE function needs to be defined
                          # X works as a 'Joker', here X stands for column

apply(temp[,5:8], 2, function(x){sd(x)/sqrt(length(x))/temp$NH4.ugL}) #dividing by a specific column





## Apply for lists/ LApply converts data frame to lists ########
## faster then 'for', well suited for complex functions/ big data sets
## gives you the results/ not such a blackbox then 'for'
## Take into account, that when you write functions for lists, X is an object in the list, not a column
## do not put more than three functions.. 


result <- lapply(temp[,5:8], mean) #same result but different object

do.call(rbind, result)   # working with objects of a list

as.list(temp)




## Working with Class/ Column names instead of data frames (Numbers of columns) 
## usefull when you work with e.g. column #2, 5, 19 etc. 

name <- names(temp[,5:8]) # creating a vector of names for columns 5,6,7,8
# if you want to select not-continuos column create a vector with the names of the columns

result <-lapply(name, function(x){print(x)})       # trick to see what we are working with        

result <-lapply(name, function(x){mean(temp[[x]])}) 

temp2 <- temp[,!names(temp)%in%name]  # data without name-data






## Exercise: Writing an universal code, not depending on a specific data frame

ROWS <- 1:nrow(temp) # creating an index
  # All caps because: R never writes in all caps, so you can see easily its and object you created


seq(1,23) # same as above, easier to modify, for example: 

seq(1,24, by=2) # selects every second column


dates <- temp$Data
print(dates)

unique(temp$Data)  # Selecting only unique values


result <- lapply(ROWS, function(x){mean(as.numeric(temp[x,5:8]))})  # ATTENTION: 'x,' because now we are working with ROws

do.call(rbind, result)


paste0(temp[,"Data"],temp[,"Site"]) # see combinations of sites and dates


### !!! Error here somewhere... Sandra please fill


## Past good to create labels

temp[,"new_column"] <- c(temp[,"Data"], temp[,"Site"])
temp[,"new_column"] <- do.call(rbind, lapply(ROWS, function(x){c(paste0(temp[x,"Data"]), (temp[x,"Site"]))})) #create new column with combinations with lapply




                 