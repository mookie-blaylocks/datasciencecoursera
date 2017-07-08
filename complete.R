complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    count_complete_obs <- function(filename) {
        filepath <- file.path(directory, filename)
        data <- read.csv(filepath)
        complete_obs <- data[complete.cases(data),]
        nrow(complete_obs)
    }
    
    filenums <- lapply(id, formatC, width=3, format="d", flag="0")
    filenames <- lapply(filenums, paste, ".csv", sep="")
    
    nobs <- unlist(lapply(filenames, count_complete_obs))
    data.frame(id, nobs)
}