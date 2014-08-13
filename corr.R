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
        for (i in 1:nrow(idnobs)) {
                ## calculate correlations for the monitors that meet the 
                ## threshold requirement and add it to the vector of 
                ## correlations
                if (idnobs[i,'nobs'] > threshold) {
                        monitor <- read.csv(files[i])
                        ##monitor <- monitor[!is.na(monitor[,'sulfate']) & !is.na(monitor[,'nitrate']),]
                        ## cor returns NA when there is only one observation, 
                        ## and fail if x has length zero.
                        if (nrow(monitor) > 1 ) {
                                ## cr <- c(cr,round(cor(monitor[,'sulfate'], monitor[,'nitrate']),5))
                                ## cr <- c(cr,cor(monitor[,'sulfate'], monitor[,'nitrate']))
                                ## If use has the value "pairwise.complete.obs" 
                                ## then the correlation between each pair of 
                                ## variables is computed using all complete 
                                ## pairs of observations on those variables.
                                cr <- c(cr,cor(monitor[,'sulfate'], monitor[,'nitrate'], use= 'pairwise.complete.obs'))
                        }
                }
        }
        
        ## Return a numeric vector of correlations
        cr
        
}