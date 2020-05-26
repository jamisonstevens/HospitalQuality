best <- function(state, outcome) {
  ## Read outcome data
  workingdirectory <- "C:/Users/student/Documents/Learning/R/HospitalQuality"
  setwd(workingdirectory)
  
  filename <- paste(workingdirectory, "/outcome-of-care-measures.csv", sep = "")
  outcomefile <- read.csv(filename, colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!(state %in% outcomefile[,7])) {
    stop("invalid state")
  }
  possibleoutcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% possibleoutcomes)) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if(identical(outcome, "heart attack")) {
    col <- 11
  }
  else if(identical(outcome, "heart failure")) {
    col <- 17
  }
  else {    ## outcome is equal to "pneumonia"
    col <- 23
  }
  
  simplifiedcsv <- data.frame("Hospital Name" = outcomefile$Hospital.Name, "State" = outcomefile$State, "Outcome" = outcomefile[ ,col])
  statecsv <- simplifiedcsv[(simplifiedcsv$State == state),]
  
  minmortalityrate <- min(suppressWarnings(as.numeric(statecsv$Outcome)), na.rm = TRUE)
  
  minmortalitydata <- statecsv[suppressWarnings(as.numeric(statecsv$Outcome)) == minmortalityrate,]
  minmortalityhospitals <- minmortalitydata$Hospital.Name
  minmortalityhospitals <- na.omit(minmortalityhospitals)
  
  if(length(minmortalityhospitals) > 1) {
    minmortalityhospitals <- sort(minmortalityhospitals)
  }
  
  return(minmortalityhospitals[1])
}