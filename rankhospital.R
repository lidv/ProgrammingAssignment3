rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  indices <- c(11, 17, 23)
  
  if (!state %in% data$State) stop("invalid state")
  if (!outcome %in% outcomes) stop("invalid outcome")
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  i <- indices[match(outcome, outcomes)]
  hospitals <- data[data$State == state, c(2, i)]
  hospitals[, 2] <- as.numeric(as.character(hospitals[, 2]))
  hospitals <- na.omit(hospitals)
  names(hospitals) <- c("name", "deaths")
  
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- nrow(hospitals)
  } else {
    num <- as.numeric(num)
    if (is.na(num)) {
      stop("invalid num")
    } else if (num > nrow(hospitals)) {
      return(NA)
    }
  }
  
  return(as.character(hospitals[order(hospitals$deaths, hospitals$name),1][num]))
}
