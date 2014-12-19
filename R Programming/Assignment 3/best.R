best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state and outcome are valid
  states <- unique(data[, 7])
  if (!state %in% states) {
    stop("invalid state") 
  }
  
  possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% possibleOutcomes) {
    stop("invalid outcome") 
  }
  
  ## Find the lowest rates for the chosen outcome
  columns <- c(11, 17, 23)
  colNum <- columns[match(outcome, possibleOutcomes)]
  colNum <- colNum[!is.na(colNum)]
  
  dataState <- data[data$State == state, ]
  idx <- which.min(as.double(dataState[,colNum]))
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  dataState[idx, "Hospital.Name"]
}