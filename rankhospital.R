foldername <- "rprog-data-ProgAssignment3-data/"
rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    outcomes <- read.csv(paste0(foldername,"outcome-of-care-measures.csv"),colClasses = "character")
    ##check if state and outcome are valid
    state <- toupper(state)
    if(!(state %in% outcomes$State)) {
        stop('invalid state')
        #exit
    }
    
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
    #outcomes.state <- outcomes[outcomes$State==state,]
    
    #sorted.data.state <- suppressWarnings(outcomes.state[order(as.numeric(outcomes.state[[outcomefullname]]),outcomes.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ])
    
    
    
    y <- subset(outcomes, State == state & outcomes[,outcomefullname] != 'Not Available',select=c('Hospital.Name', outcomefullname))
    
    y <- suppressWarnings(y[order(as.numeric(y[[outcomefullname]]),y[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ])
    
    if(num == "best")
        num <- 1
    else if(num=="worst")
        num <- nrow(y)
    
    y[num,"Hospital.Name"]
    
}
