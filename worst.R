foldername <- "rprog-data-ProgAssignment3-data/"
worst <- function(state,outcome) {
    ## Read outcome data
    outcomes <- read.csv(paste0(foldername,"outcome-of-care-measures.csv"),colClasses = "character")
    ##check if state and outcome are valid
    state <- toupper(state)
    if(!(state %in% outcomes$State)) {
        stop('invalid state')
        #exit
    }
    
    if(tolower(outcome) == 'heart attack') {
        outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    } else if(tolower(outcome) == 'heart failure') {
        outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    } else if(tolower(outcome) == 'pneumonia') {
        outcome  <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    } else {
        stop('invalid outcome')
        #exit
    }
    #names(outcomes)
    if(!(outcome %in% names(outcomes))) {
        stop('invalid column')
    }
    
    outcomes.state <- outcomes[outcomes$State==state,]
    idx <- suppressWarnings(which.max(as.double(outcomes.state[,outcome])))
    outcomes.state[idx,"Hospital.Name"]
    
}