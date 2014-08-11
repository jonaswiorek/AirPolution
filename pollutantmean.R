pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        files <- list.files(directory, full.names=TRUE)
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        if (pollutant == 'sulfate') {
                col <- 2
        } else if (pollutant == 'nitrate') {
                col <- 3
        } else {
                col <- NA
        }
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        pollutantvector <- numeric()
        
        for(i in 1:length(id)) {
                monitor <- read.csv(files[id[i]])
                ## Create pollutant
                ## pollutantmean <- mean(monitor[!is.na(monitor[,col]),col])
                pollutantvector <- append(pollutantvector,monitor[,col])
        }       
        
        ## Remove NA from pollutant
        pollutantvector <- pollutantvector[!is.na(pollutantvector)]  
        ## Return the mean of pollutant
        round(mean(pollutantvector), digits=3)
        
}        