rankall <- function(outcome, num = "best") {
  
  dataAll <- data.frame(hospital = character(), state = character())
  
  ## Read outcome data
  csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {stop("Invalid outcome.")}
  else {
    keys = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    key <- keys[outcome]
  }
  
  ## For each state, find the hospital of the given rank
  # Filter out hospital by outcome, then split with state, order by rates and hospitalnames
  by_state <- split(csv, csv$State)
  for (stat in names(by_state)) {
    target_state <- by_state[[stat]]
    rates <- suppressWarnings(as.numeric(target_state[, key]))
    good <- complete.cases(rates)
    clean <- target_state[good,]
    clean_rates <- rates[good]
    data <- clean[order(clean_rates, clean['Hospital.Name']),]
    
    if (grepl("^[0-9]+$", num)) {n <- num}
    else if (num == "best") {n <- 1}
    else if (num == "worst") {n <- length(clean_rates)}

    dataPart <- data.frame(hospital = data[n, 2], state = stat, row.names = stat)
    dataAll <- rbind(dataAll, dataPart)
  }
  
  dataAll 
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}