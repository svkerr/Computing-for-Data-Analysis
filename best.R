#best.R
best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  outcome_vec <- c("heart attack", "heart failure", "pneumonia")    # establish set of valid outcomes
  
  if (!(state %in% data$State)) stop("Invalid State")            # check for valid state
  
  if (!(outcome %in% outcome_vec)) stop("Invalid Outcome")           # check for valid outcome
  
  red_data <- subset(data, select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                    Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                    Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
                                    subset = (State == state) )

  if(outcome == "heart attack") {outcome_mod = red_data[,2]}
  if(outcome == "heart failure") {outcome_mod = red_data[,3]}
  if(outcome == "pneumonia") {outcome_mod = red_data[,4]}
  
  red_data_order <- red_data[order(outcome_mod, na.last = NA) ,]
  
  return(as.character(red_data_order[1,1]))
  
}




