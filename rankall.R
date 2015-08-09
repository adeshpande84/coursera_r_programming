foldername <- "rprog-data-ProgAssignment3-data/"
rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    outcomes <- read.csv(paste0(foldername,"outcome-of-care-measures.csv"),colClasses = "character")
    ## check if state and outcome are valid
    
    
    if(tolower(outcome) == 'heart attack') {
        outcomefullname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    } else if(tolower(outcome) == 'heart failure') {
        outcomefullname <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    } else if(tolower(outcome) == 'pneumonia') {
        outcomefullname  <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    } else {
        stop('invalid outcome')
        #exit
    }
    #names(outcomes)
    if(!(outcomefullname %in% names(outcomes))) {
        stop('invalid column')
    }
    
    states = outcomes$State
    states = sort(unique(states))
    
    numrows <- length(states)
    #idn <- numeric(numrows)
    hospital <- numeric(numrows)
    statesf <- numeric(numrows)
    ## For each state, find the hospital of the given rank
    j <- 1
    for(state in states) {
        y <- subset(outcomes, State == state,select=c('Hospital.Name', outcomefullname))
        y <- suppressWarnings(y[order(as.numeric(y[[outcomefullname]]),y[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ])
        
        if(num == "best")
            num <- 1
        else if(num=="worst")
            num <- nrow(y)
        
        hospital[j] <- y[num,"Hospital.Name"]
        statesf[j] <- state
        j = j+1   
        
    }
    data.frame(hospital,state=statesf)
    
}