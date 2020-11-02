#------------------------------------------------------------------------------
## Finding the best hospital is a state

## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death rate

best <- function(state, outcome) {
        # state = 2-character abbreviated name of a state
        # outcome = three options
        
        # load data 
        data_outcome <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        vec_state    <- data_outcome$State
        outcomes     <- c("heart attack", "heart failure", "pneumonia")
        
        ## Error input
        if(sum(is.element(unique(vec_state),state)) == 0){
                return(print(paste("Error : '", state, "' invalid state")))
        }
        if(sum(outcomes == outcome) == 0){
                return(print(paste("Error : '", outcome, "' invalid outcome")))
        }
        
        ## vector Names, lowest 30-day mortality
        ind_state     <- is.element(vec_state,state)
        
        if(outcome == "heart attack")  vec_mortality <- data_outcome[, 13]
        if(outcome == "heart failure") vec_mortality <- data_outcome[, 19]
        if(outcome == "pneumonia")     vec_mortality <- data_outcome[, 25]
        
        ind_hospital  <- which.min(as.numeric(vec_mortality[ind_state]))
        name_hospital <- data_outcome$Hospital.Name[ind_state][ind_hospital]
        return(name_hospital)
        
}

#-------------------------------------------------------------------------------
## Ranking hospitals by outcome in a state

## Check that state and outcome are valid
## Return hospital name in that state with the given rank 30-day death rate

rankhospital <- function(state, outcome, num = "best") {
        # state = 2-character abbreviated name of a state.
        # outcome = three options
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        # load data 
        data_outcome <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        vec_state    <- data_outcome$State
        outcomes     <- c("heart attack", "heart failure", "pneumonia")
        
        ## Error input
        if(sum(is.element(unique(vec_state),state)) == 0){
                return(print(paste("Error : '", state, "' invalid state")))
        }
        if(sum(outcomes == outcome) == 0){
                return(print(paste("Error : '", outcome, "' invalid outcome")))
        }
        if (num == "best") num = 1
        
        ind_state     <- is.element(vec_state,state)
        if(outcome == "heart attack")  vec_mortality <- as.numeric(
                                                        data_outcome[, 11][ind_state])
        if(outcome == "heart failure") vec_mortality <- as.numeric(
                                                        data_outcome[, 17][ind_state])
        if(outcome == "pneumonia")     vec_mortality <- as.numeric(
                                                        data_outcome[, 23][ind_state])
        
        ind_repeated   <- order(vec_mortality,data_outcome$Hospital.Name[ind_state])
        ind_nas        <- !is.na(vec_mortality[ind_repeated])
        hospital_names <- data_outcome$Hospital.Name[ind_state][ind_repeated][ind_nas]
        
        if (num == "worst") num <- length(hospital_names)
        if (num > length(hospital_names)) return(NA)
        return(hospital_names[num])

}

#-------------------------------------------------------------------------------
## Ranking hospitals in all states

## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name

rankall <- function(outcome, num = "best") {
        # load data 
        data_outcome <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        outcomes     <- c("heart attack", "heart failure", "pneumonia")
        ## Error input
        if(sum(outcomes == outcome) == 0){
                return(print(paste("Error : '", outcome, "' invalid outcome")))
        }
        
        vec_state    <- sort(unique(data_outcome$State))        
        vec_hospital <- vector(mode = "character", length = length(vec_state))
        if (num == "best") num = 1

        for(ss in 1:length(vec_state)){
                
                ind_state     <- is.element(data_outcome$State,vec_state[ss])
                if(outcome == "heart attack")  vec_mortality <- as.numeric(
                        data_outcome[, 11][ind_state])
                if(outcome == "heart failure") vec_mortality <- as.numeric(
                        data_outcome[, 17][ind_state])
                if(outcome == "pneumonia")     vec_mortality <- as.numeric(
                        data_outcome[, 23][ind_state])
                
                ind_repeated   <- order(vec_mortality,data_outcome$Hospital.Name[ind_state])
                ind_nas        <- !is.na(vec_mortality[ind_repeated])
                hospital_names <- data_outcome$Hospital.Name[ind_state][ind_repeated][ind_nas]
                
                if (num == "worst"){
                        vec_hospital[ss] <- hospital_names[length(hospital_names)]
                        next
                } 
                
                if (num > length(hospital_names)){
                        vec_hospital[ss] <- NA
                } else{
                        vec_hospital[ss] <- hospital_names[num]}
        }
        
        return(data.frame("hospital" = vec_hospital, "state" = vec_state))
}
