rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[c(2, 7, 11, 17, 23)]
  data[, c(3, 4, 5)] <- sapply(data[, c(3, 4, 5)], as.numeric)
  
  ## Check that state and outcome are valid
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  states <- unique(data$State)
  states <- sort(states)
  
  ranks <- data.frame(hospital=NA, state=NA)
  
  for (i in 1:length(states)) {
    ranks[i, ] <- c(rankhospital(states[i], outcome, num), states[i])
  }
  
  ranks
}