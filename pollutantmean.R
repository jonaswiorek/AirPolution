pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files        
        files <- list.files(directory, full.names=TRUE)
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        monitor <- data.frame()
        
        for(i in 1:length(id)) {
                monitor <- rbind(monitor, read.csv(files[id[i]]))
        }       
        
        ## Remove NA from pollutant
        ## and return the mean of pollutant
        round(mean(monitor[,pollutant],na.rm=TRUE), digits=3)
        
}        