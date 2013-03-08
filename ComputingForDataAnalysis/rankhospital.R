rankhospital <- function(state, outcome, num = "best") {
    if (outcome == "heart attack"){ 
        coln <- 11
    }
    else if (outcome == "heart failure"){
        coln <- 17
    }
    else if (outcome == "pneumonia"){
        coln <- 23
    }
    else{
        stop("invalid outcome")
    }

## Read outcome data
    ocd <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ocd[,coln] <- as.numeric(ocd[,coln])
    ocd <- ocd[complete.cases(ocd[,coln]),]

## Check that state and outcome are valid
    if (!state %in% ocd$State){
        stop("invalid state")
    }

# create a temp table of only state being evaluated
    eval_state <- ocd[ocd$State == state ,]
    
    sorted_h_names<- eval_state[order(eval_state[,coln],eval_state[,2]),2]
    nhos <- length(sorted_h_names)

    if (num=="best"){
    	return(sorted_h_names[1])
    }    
    else if (num=="worst"){
        return(sorted_h_names[nhos]) #FixMe: fails if tie in last place
    }
    else {
	 if (as.numeric(num) > nhos){ return("NA")}
	 else {
             return(sorted_h_names[as.numeric(num)])
         }
    }    
}
