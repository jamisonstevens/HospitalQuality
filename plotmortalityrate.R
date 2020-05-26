plotmortalityrate <- function() {
  workingdirectory <- "C:/Users/student/Documents/Learning/R/HospitalQuality"
  setwd(workingdirectory)
  
  filename <- paste(workingdirectory, "/outcome-of-care-measures.csv", sep = "")
  outcome <- read.csv(filename, colClasses = "character")
  
  outcome[,11] <- as.numeric(outcome[,11])
  hist(outcome[,11])
}
