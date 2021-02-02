best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!state %in% data$State) {
    stop("Invalid state.")
  }
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("Invalid outcome.")
  }
  
  ## Generating mortality column names for each outcome
  keys <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  outcome <- keys[outcome]
  
  ## Return hospital name in that state with lowest 30-day death rate
  dataPerState <- split(data, data$State)
  dataOurState <- dataPerState[[state]]
  ordered <- dataOurState[order(dataOurState$Hospital.Name),]
  
  numeric <- suppressWarnings(as.numeric(ordered[, outcome]))
  good <- complete.cases(numeric)
  clean <- ordered[good, ]
  days <- numeric[good]
  index <- match(min(days), days)
  clean[index, 2]
  
}