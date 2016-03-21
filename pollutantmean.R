# Assignment 2.2.1: 
# Write a function named 'pollutantmean' that calculates the mean of a
# pollutant (sulfate or nitrate) across a specified list of monitors. The
# function 'pollutantmean' takes three arguments: 'directory', 'pollutant',
# and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that
# monitors' particulate matter data from the files in the directory specified
# in the 'directory' argument and returns the mean of the pollutant across all
# of the monitors, ignoring any missing values coded as NA.
#
# Parameters:
# (1) 'directory' is a character vector of length 1 indicating the location of
# the CSV files. (2) 'pollutant' is a character vector of length 1 indicating
# the name of the pollutant for which we will calculate the mean; either
# "sulfate" or "nitrate". (3) 'id' is an integer vector indicating the monitor
# ID numbers to be used.
#
# Output: Return the mean of the pollutant across all monitors list # in the
# 'id' vector (ignoring NA values) NOTE: Do not round the result!

pollutantmean <- function(directory, pollutant, id = 1:332) {

    # Create a list of files...
    datafilelist <- list.files(directory, full.names=TRUE)
    
    # Initilise an empty data frame...
    dataset <- data.frame()
    
    # Parse the files and rbind them together (see notes below)...
    for (datafile in datafilelist) {                                
                dataset <- rbind(dataset, read.csv(datafile))
        }
        
    # Subsets the rows that match the 'ID' arguments...
    # NB: This is pretty inefficient as it requires a second rbind pass. It
    # would be far better to somehow include the ("ID" == id) test in the first
    # rbind pass and thereby reduce both execution time and memory utilisation
    datasubset <- data.frame()
    for (valid in id) {
        datasubset <- rbind(datasubset, dataset[which(dataset[, "ID"] == valid),])                           
    }    
    
    # Calculate the Sulfate and Nitrate means...
    mean.sulfate <- mean(datasubset[, "sulfate"], na.rm=TRUE)      
    mean.nitrate <- mean(datasubset[, "nitrate"], na.rm=TRUE)      
    
    # Select required result and output...
    if (pollutant == "sulfate"){
        print(mean.sulfate)
    } else if (pollutant == "nitrate"){
        print(mean.nitrate)
    } else {
        print("Invalid Pollutant Selection")
    }
}

# The approach used here for building the data frame is submoptimal. It
# works, but generally speaking, you don't want to build data frames or
# vectors by copying and re-copying them inside of a loop. If you've got a
# lot of data it can become very, very slow. For a better approach, check
# out Hadley Wickam's excellent material on functionals within R:
# http://adv-r.had.co.nz/Functionals.html.


