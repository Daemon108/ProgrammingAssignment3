rankall <- function(outcome, num = "best") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## For each state, find the hospital of the given rank
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name

      possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
      
      if(!outcome %in% possible_outcomes) stop("invalid outcome")
      
      outcome_of_care <- read.csv("outcome-of-care-measures.csv",
                                  colClasses = "character")
      states<-unique(outcome_of_care$State)
      states<-states[order(states)]
      if(outcome == "heart attack") {
            selcol <- 11L
      } else if(outcome == "heart failure") {
            selcol <- 17L
      } else {  # pneumonia
            selcol <- 23L
      }
      suppressWarnings(outcome_of_care[,selcol] <-
                             as.numeric(outcome_of_care[,selcol]))
      rankall <- NULL
      for(state in states) {
            selection <- outcome_of_care[which(outcome_of_care$State == state),
                                         c(selcol,2)]
            selection <- selection[complete.cases(selection),]
            
            num_of_hosp <- nrow(selection) 
            if(num == "best") {
                  hosprank <- 1L
            } else if(num == "worst") {
                  hosprank <- num_of_hosp
            } else if(num <= num_of_hosp) {
                  hosprank <- as.integer(num)
            } else {
                  hosprank <- NA
            }   
            
            if(!is.na(hosprank)) {
                  hospital = selection[do.call(order,selection),][hosprank,2]
            } else {
                  hospital = NA
            }
            
            rankall <- rbind(rankall, data.frame(hospital, state))
      }
      rownames(rankall)<-rankall$state
      
      rankall
}