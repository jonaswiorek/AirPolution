complete <- function(directory, id=1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        files <- list.files(directory, full.names=TRUE)
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        nobs <- integer(length(id))
        
        for(i in 1:length(id)) {
                monitor <- read.csv(files[id[i]])
                ## caclualte the number of complete cases (of both sulfate and nitrate)
                nobs[i] <- length(monitor[!is.na(monitor[,2]) & !is.na(monitor[,3]),1])
        } 
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        ## create data fram with two columns with id and nobs
        idnobs <- data.frame(cbind(id,nobs))
        colnames(idnobs) <- c('id','nobs')
        idnobs
}