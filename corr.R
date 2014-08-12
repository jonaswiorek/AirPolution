corr <- function(directory, threshold = 0){
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        files <- list.files(directory, full.names=TRUE)
        idnobs <- complete(directory)        
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        cr <- numeric() ## create a numeric vector of correlations       
        for (i in 1:length(idnobs[,1])) {
                ## calculate correlations for the monitors that meet the 
                ## threshold requirement and add it to the vector of 
                ## correlations
                if (idnobs[i,2] > threshold) {
                        monitor <- read.csv(files[i])
                        monitor <- monitor[!is.na(monitor[,2]) & !is.na(monitor[,3]),]
                        ## cor returns NA when there is only one observation, 
                        ## and fail if x has length zero.
                        if (length(monitor[,1]) > 1 ) {
                                ##cr <- c(cr,round(cor(monitor[,2], monitor[,3]),5))
                                cr <- c(cr,cor(monitor[,2], monitor[,3]))
                        }
                }
        }
        
        ## Return a numeric vector of correlations
        cr
        
}