# Assignment 2.2.3: 
# Write a function that takes a directory of data files and a threshold for
# complete cases and calculates the correlation between sulfate and nitrate for
# monitor locations where the number of completely observed cases (on all
# variables) is greater than the threshold. The function should return a vector
# of correlations for the monitors that meet the threshold requirement. If no
# monitors meet the threshold requirement, then the function should return a
# numeric vector of length 0. For this function you will need to use the 'cor'
# function in R which calculates the correlation between two vectors. 
# [See the help page for this function via '?cor']
#
# Parameters:
# (1) 'directory' is a character vector of length 1 indicating the location of
# the CSV files. (2) 'threshold' is a numeric vector of length 1 indicating the
# number of completely observed observations (on all variables) required
# to compute the correlation between nitrate and sulfate; the default is 0
#
# Output:
# Return a numeric vector of correlations. Do not round the result!

corr <- function(directory, threshold = 0) {

    # Create a list of files...
    datafilelist <- list.files(directory, full.names=TRUE)
     
    # Initilise an empty data frame...
    dataset <- data.frame()
     
    # Parse the files and rbind them together, excluding monitors that do not meet
    # the threshold value for the number of complete observations...
    for (datafile in datafilelist) {                                
        datatmp <- data.frame()
        datatmp <- read.csv(datafile)  
        good.datatmp <- datatmp[complete.cases(datatmp),]
        # print(c(datafile, dim(good.datatmp)[1]))    ## Print for debugging
        if (dim(good.datatmp)[1] > threshold){
            dataset <- rbind(dataset, good.datatmp)
        }
    }
    
    # Perform Correlations and Build the Output Table...
    dataoutput <- data.frame()
    if (dim(dataset)[1] > 0){
        id <- unique(dataset[,4])
        for (valid in id) {
            testdata <- dataset[which(dataset[, "ID"] == valid),]
            correff <- cor(cbind(testdata[,2],testdata[,3]))
            datapartial <- data.frame(cor1=correff[1,2],cor2=correff[2,1])
            dataoutput <- rbind(dataoutput, datapartial)
        }  
        datafinal <- as.vector(dataoutput[,1])
    } else {
        datafinal <- vector()    
    }
}
    