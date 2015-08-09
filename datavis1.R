library(ggplot2)
library(reshape2)

datavis1 <- function() {
    temperatures <- read.csv("ExcelFormattedGISTEMPData2CSV.csv", colClasses = "character", stringsAsFactors = FALSE)
    decades <- (as.numeric(temperatures$Year) %% 10 == 0)
    
    head(temperatures[decades,])
    
    temperatures.m <- melt(temperatures[decades,], id.vars='Year')
    colnames(temperatures.m) <- c("Year", "variable", "Temperature")
    #max(temperatures.m$Temperature)
    #head(temperatures.m)
    #temperatures.m$Temperature <- as.numeric(temperatures.m$Temperature)
    ggplot(temperatures.m, aes(x=Year, y=Temperature, colour=variable, group=variable)) +
        
        geom_line(data=temperatures.m[temperatures.m$variable=='Glob',]) +
        geom_point(data=temperatures.m[temperatures.m$variable=='Glob',], shape=3) +
    
     
        
        geom_line(data=temperatures.m[temperatures.m$variable=='NHem',]) +
        geom_point(data=temperatures.m[temperatures.m$variable=='NHem',], shape=3)
    
    
}
