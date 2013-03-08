rankall <- function(outcome, num = "best") {
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


# Create a data fram with state abbreviations
    result <- as.data.frame(unique(ocd$State))
    colnames(result)[1] <- "state"
# add a char column to the df
    result$hospital <- rep("NA",nrow(result))
    
    #print(colnames(result))    

    result <- result[,c("hospital","state")]
    
    #print(unique( result$state ))
    for (state in unique(result$state)){
# create a temp table of only state being evaluated
        eval_state <- ocd[ocd$State == state ,]
        eval_state[,coln] <- as.numeric(eval_state[,coln])
        
        # drop NAs
        eval_state <- eval_state[complete.cases(eval_state[,coln]),]
        #print( eval_state[order(eval_state[,coln]),coln] )

        sorted_h_names<- eval_state[order(eval_state[,coln],eval_state[,2]),2]
        nhos <- length(sorted_h_names)

        if (num=="best"){
            result[result$state==state,"hospital"]<-sorted_h_names[1]
        }    
        else if (num=="worst"){
            #print(sorted_h_names[nhos-1])
            #print(sorted_h_names[nhos])
    	    result[result$state==state,"hospital"]<-sorted_h_names[nhos]

        }
        else {
	    if (as.numeric(num) > nhos){ 
    	        result[result$state==state,"hospital"]<-NA
            }
	    else {
    	        result[result$state==state,"hospital"]<-sorted_h_names[num]
            }
        }
    }
    # sort the results by state name
    result <- result[order(result$state),]
    return(result)
}
