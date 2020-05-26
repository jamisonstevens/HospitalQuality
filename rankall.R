rankall <- function(outcome, num = "best") {
  ## Read outcome data
  workingdirectory <- "C:/Users/student/Documents/Learning/R/HospitalQuality"
  setwd(workingdirectory)
  
  filename <- paste(workingdirectory, "/outcome-of-care-measures.csv", sep = "")
  outcomefile <- read.csv(filename, colClasses = "character")
  
  ## Check that outcome is valid
  possibleoutcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% possibleoutcomes)) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  states <- c()
  staterankings <- data.frame()
  for(statename in outcomefile$State) {
    if(!(statename %in% states)) {
      states <- c(states, statename)
      staterank <- rankhospital(statename, outcome, num)
      staterankings <- rbind(staterankings, c(staterank, statename))
    }
  }
  names(staterankings) <- c("Hospital Name", "State")
  staterankings <- staterankings[order(staterankings$State),]
  return(staterankings)
}