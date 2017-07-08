pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'polutant is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the 
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
        
    filenums <- lapply(id, formatC, width=3, format="d", flag="0")
    filenames <- lapply(filenums, paste, ".csv", sep="")
    
    pollutantlist <- numeric()
    
    for (filename in filenames){
        filepath <- file.path(directory, filename)
        data <- read.csv(filepath)
        pollutantlist <- c(pollutantlist, data[[pollutant]])
    }
    
    mean(pollutantlist, na.rm=TRUE)
}