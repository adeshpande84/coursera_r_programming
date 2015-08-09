pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    specdataPath <- paste0(getwd(),"/",directory)
    numReadings <- 0
    totalPollutant <- 0
    
    for(i in id) {
        
        if(i<10) {
            file = paste0("00",i,".csv")
        } else if (i>=10 & i<100) {
            file = paste0("0",i,".csv")
        } else {
            file = paste0(i,".csv")
        }
        
   
        x <- read.csv(paste0(specdataPath,"/",file))
        y <- x[[pollutant]]
        good <- !is.na(y)
        
        numReadings = numReadings + length(y[good])
        totalPollutant = totalPollutant + sum(y[good])
        
    }
    
    totalPollutant/numReadings
    
}
