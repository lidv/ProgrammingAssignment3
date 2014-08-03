best <- function(state, outcome) {
  
  ## Read outcome data
  rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(rawdata$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!state %in% rawdata$State) stop("invalid state")
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  data <- rawdata[rawdata$State == state, ]
  data[, c(11, 17, 23)] <- sapply(data[, c(11, 17, 23)], as.numeric)
  data <- data[order(data[, 2]), ]
  
  if (outcome == "heart attack") {
    best <- data[which.min(data[, 11]), "Hospital.Name"]
  }
  else if (outcome == "heart failure") {
    best <- data[which.min(data[, 17]), "Hospital.Name"]
  }
  else {
    best <- data[which.min(data[, 23]), "Hospital.Name"]
  }
  
  best
}