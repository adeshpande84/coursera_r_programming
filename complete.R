complete <- function(directory, id = 1:332) {

    specdataPath <- paste0(getwd(),"/",directory)
    j <- 1

    numrows <- length(id)
    idn <- numeric(numrows)
    nobs <- numeric(numrows)
    for(i in id) {
        
        if(i<10) {
            file = paste0("00",i,".csv")
        } else if (i>=10 & i<100) {
            file = paste0("0",i,".csv")
        } else {
            file = paste0(i,".csv")
        }

        x <- read.csv(paste0(specdataPath,"/",file))
        y <- complete.cases(x)
        idn[j] <- i
        s <- sum(y)
        nobs[j] <- s
        
        j = j+1   
    }
    
    data.frame(id=idn,nobs)
    
    
}