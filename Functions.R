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
        if(outcome == "heart attack"){
                ind_state     <- is.element(vec_state,state)
                vec_mortality <- data_outcome[, 13]
                ind_hospital  <- which.min(as.numeric(vec_mortality
                                                     [ind_state]))
                name_hospital <- data_outcome$Hospital.Name[ind_state][ind_hospital]
                return(name_hospital)
        }
        if(outcome == "heart failure"){
                ind_state     <- is.element(vec_state,state)
                vec_mortality <- data_outcome[, 19]
                ind_hospital  <- which.min(as.numeric(vec_mortality
                                                      [ind_state]))
                name_hospital <- data_outcome$Hospital.Name[ind_state][ind_hospital]
                return(name_hospital)
        }
        if(outcome == "pneumonia"){
                ind_state     <- is.element(vec_state,state)
                vec_mortality <- data_outcome[, 25]
                ind_hospital  <- which.min(as.numeric(vec_mortality
                                                      [ind_state]))
                name_hospital <- data_outcome$Hospital.Name[ind_state][ind_hospital]
                return(name_hospital)
        }
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
        vec_mortality  <- vec_mortality[ind_repeated]
        ind_nas        <- !is.na(vec_mortality)
        hospital_names <- data_outcome$Hospital.Name[ind_state][ind_repeated][ind_nas]
        
        if (num == "worst") num <- length(hospital_names)
        if (num > length(hospital_names)) return(NA)
        return(hospital_names[num])

}

