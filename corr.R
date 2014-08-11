corr <- function(directory, threshold = 0){
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
  
        files <- list.files(directory, full.names=TRUE)
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        for(i in 1:332) {
          monitor <- read.csv(files[i])
          cr <- cor(monitor[,2], monitor[,3])
        } 
        
        ## Return a numeric vector of correlations
        cr
  
}