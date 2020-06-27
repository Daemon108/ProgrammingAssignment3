rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      
      possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
      
      if(!outcome %in% possible_outcomes) stop("invalid outcome")
      
      outcome_of_care <- read.csv("outcome-of-care-measures.csv",
                                  colClasses = "character")
      
      states<-unique(outcome_of_care$State)
      states<-states[order(states)]
      if(!(state %in% states)) stop("invalid state")
      
      if(outcome == "heart attack") {
            selcol <- 11L
      } else if(outcome == "heart failure") {
            selcol <- 17L
      } else {  # pneumonia
            selcol <- 23L
      }
      selection <- outcome_of_care[which(outcome_of_care$State == state),
                                   c(selcol, 2)]
      suppressWarnings(selection[,1] <- as.numeric(selection[,1]))
      selection <- selection[complete.cases(selection),]
      
      num_of_hosp <- nrow(selection) 
      if(num == "best") {
            hosprank <- 1L
      } else if(num == "worst") {
            hosprank <- num_of_hosp
      } else if(num <= num_of_hosp) {
            hosprank <- as.integer(num)
      } else {
            return(NA)
            
      }
      
      selection[do.call(order,selection),][hosprank,2]
}