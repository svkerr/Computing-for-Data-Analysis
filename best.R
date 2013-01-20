#best.R
best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  outcome_vec <- c("heart attack", "heart failure", "pneumonia")    # establish set of valid outcomes
  
  if (!(state %in% data$State)) stop("Invalid State")            # check for valid state
  
  if (!(outcome %in% outcome_vec)) stop("Invalid Outcome")           # check for valid outcome
}

sort(data[,11])                # this sorts from low to high death rates


