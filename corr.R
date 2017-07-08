corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0.
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    correlation <- function(filename) {
        filepath <- file.path(directory, filename)
        data <- read.csv(filepath)
        clean_data <- data[complete.cases(data),]
        cor(clean_data$sulfate, clean_data$nitrate)
    }
    
    complete_obs <- complete("specdata")
    obs_over_threshold <- complete_obs[complete_obs$nobs > threshold,]
    
    filenums <- lapply(obs_over_threshold$id, formatC, width=3, 
                       format="d", flag="0")
    filenames <- lapply(filenums, paste, ".csv", sep="")
    
    correlations <- unlist(lapply(filenames, correlation))
    if (length(correlations) == 0)
        return(numeric(0))
    correlations
}