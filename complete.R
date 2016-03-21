# Assignment 2.2.2: 
# Write a function that reads the directory full of files and reports the number
# of completely observed cases in each data file. The function should return a
# data frame where the first column is the name of the file and the second
# column is the number of complete cases. 
#
# Parameters:
# (1) 'directory' is a character vector of length 1 indicating the location of
# the CSV files. (2) 'id' is an integer vector indicating the monitor ID
# numbers to be used.
#
# Output: Return a data frame of the form presented below, where 'id' is the
# monitor ID number and 'nobs' is the number of complete cases
    # id nobs
    # 1  117
    # 2  1041
    # ...

complete <- function(directory, id = 1:332) {

    # Create a list of files...
    datafilelist <- list.files(directory, full.names=TRUE)
     
    # Initilise an empty data frame...
    dataset <- data.frame()
     
    # Parse the files and rbind them together (see notes below)...
    for (datafile in datafilelist) {                                
        dataset <- rbind(dataset, read.csv(datafile))
    }
        
    # Subsets the rows that match the 'ID' argument...
    # NB: This is pretty inefficient as it requires a second rbind pass. It
    # would be far better to somehow include the ("ID" == id) test in the first
    # rbind pass and thereby reduce both execution time and memory utilisation
    datasubset <- data.frame()
    for (valid in id) {
        datasubset <- rbind(datasubset, dataset[which(dataset[, "ID"] == valid),])                           
    }    
    
    # Extract all complete cases...
    good.data <- datasubset[complete.cases(datasubset),]

    # Build Output Data Table...
    dataoutput <- data.frame()
    for (valid in id) {
        nobs <- dim(good.data[which(good.data[, "ID"] == valid),])
        datapartial <- data.frame(id=valid,nobs= nobs[1])
        dataoutput <- rbind(dataoutput, datapartial)
    }  
    print(dataoutput)
}

# The approach used here for building the data frame is submoptimal. It
# works, but generally speaking, you don't want to build data frames or
# vectors by copying and re-copying them inside of a loop. If you've got a
# lot of data it can become very, very slow. For a better approach, check
# out Hadley Wickam's excellent material on functionals within R:
# http://adv-r.had.co.nz/Functionals.html.es