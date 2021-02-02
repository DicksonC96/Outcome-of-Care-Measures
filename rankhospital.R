rankhospital <- function(state, outcome, num = "best") {

  ## Read outcome data
  csv <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  
  ## Check that state and outcome are valid
  if(!state %in% csv$State) {stop("Invalid state.")}
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {stop("Invalid outcome.")}
  else {
    keys = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    key <- keys[outcome]
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  by_state <- split(csv, csv$State)
  data <- by_state[[state]]
  rates <- as.numeric(data[, key])
  good <- complete.cases(rates)
  clean <- data[good, ]
  ordered <- clean[order(rates[good], clean$Hospital.Name),]
  
  if(grepl("^[0-9]+$", num)) {
    if(as.numeric(num) > nrow(ordered)) { result <- NA }
    else {result <- ordered[as.numeric(num), 2]}
  }
  else if (num == "best") {result <- ordered[1, 2]}
  else if (num == "worst") {result <- ordered[nrow(ordered),2]
  }
  result
}
