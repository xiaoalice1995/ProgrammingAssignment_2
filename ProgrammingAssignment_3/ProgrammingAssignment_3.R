outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])


best <- function (state,outcome){
  fc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #check that state is valid
  if(! state %in% fc$State) stop("invalid state")
  
  #check valid outome parameter
  valid_outcomes = c("heart attack", "heart failure", "pneumonia")
  if(! is.element(outcome,valid_outcomes)) stop("invalid outcome")
  
  ## Filter: keep only the ones in the state we are seeking
  data <- fc[fc$State == state,]
  
  #get the column name in file for the outcome input parameter
  outcome_column_name <- NULL
  if (outcome == "heart attack") {
    outcome_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    outcome_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else {
    outcome_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  #transform the column that we want to find the min to numeric (was read as a character)
  data[,outcome_column_name] <- as.numeric(data[,outcome_column_name] )
  
  #calculate the minimum value for the outcome parameter
  min_value <-min(data[[outcome_column_name]],na.rm=TRUE)
  
  # OPTION 1) get the row for (data for hospital) with the minumum score that we were seeking
  #select the rows with the min value (only one check possible + we need to return the full row)   
  #best_hospital <- data[data[,outcome_column_name] == min_value,]
  
  #sort and get the first one in case that there are several with the minumim score.
  # best_hospital <- sort(best_hospital[,"Hospital.Name"])
  
  # OPTION 2) utilize subset function 
  # the subset function allows to select as many conditions as we want + select to return the field of interest 
  best_hospital <- subset(data[,"Hospital.Name"], !is.na(data[,outcome_column_name]) & data[,outcome_column_name] == min_value )
  
  #sort and get the first one in case that there are several with the minumim score.
  best_hospital <- sort(best_hospital)
  
  # return the first hospital in case that are several with the lowest score
  as.character(best_hospital[1])
}


best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")


rankhospital <- function (state,outcome,num){ 
  fc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #check that state is valid
  if(! state %in% fc$State) stop("invalid state")
  
  #check valid outome parameter
  valid_outcomes = c("heart attack", "heart failure", "pneumonia")
  if(! is.element(outcome,valid_outcomes)) stop("invalid outcome")
  
  ## Filter: keep only the ones in the state we are seeking
  data <- fc[fc$State == state,]
  
  #get the column name in file for the outcome input parameter
  outcome_column_name <- NULL
  if (outcome == "heart attack") {
    outcome_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    outcome_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else {
    outcome_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  #transform the column that we want to find the min to numeric (was read as a character)
  data[,outcome_column_name] <- as.numeric(data[,outcome_column_name] )
  
  #exclude na values 
  hospitals_without_na <- subset(data , !is.na(data[,outcome_column_name]))
  
  #use "order" to sort it, first by outcome and then by hospital name
  sorted_data <- hospitals_without_na[ order(hospitals_without_na[,outcome_column_name], hospitals_without_na[,"Hospital.Name"]),]
  
  if (num == "best") 
  {
    source("best.R")
    best(state, outcome)
  } 
  else if (num == "worst") 
  {
    tail(sorted_data[,"Hospital.Name"], n = 1)
  }
  else 
  {
    if(num >nrow((sorted_data))) NA
    else
    {
      sorted_data[,"Hospital.Name"][num]
    }
  }
}


rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)


rankall <- function (outcome,num = "best"){ 
  fc <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # get all states existent in file
  states <- unique(fc[,"State"])
  
  #check valid outome parameter
  valid_outcomes = c("heart attack", "heart failure", "pneumonia")
  if(! is.element(outcome,valid_outcomes)) stop("invalid outcome")
  
  #get the column name in file for the outcome input parameter
  outcome_column_name <- NULL
  if (outcome == "heart attack") {
    outcome_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    outcome_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else {
    outcome_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  dataset <- do.call("rbind",lapply( states,
                                     FUN=function(state)
                                     { #exclude na values and select only the ones from the current state
                                       hospitals_in_state <- subset(fc , !is.na(fc[,outcome_column_name]) & fc[,"State"] == state)
                                       
                                       #use "order" to sort it, first by outcome and then by hospital name
                                       sorted_data_for_state <- hospitals_in_state[ order(as.numeric(hospitals_in_state[,outcome_column_name]), hospitals_in_state[,"Hospital.Name"]),]
                                       hospital <- NULL
                                       if (num == "best") 
                                       { 
                                         hospital <- head(sorted_data_for_state[,"Hospital.Name"], n = 1)
                                       } 
                                       else if (num == "worst") 
                                       { #num_rows = nrow(sorted_data_for_state)
                                         #hospital <- sorted_data_for_state[,"Hospital.Name"][num_row]
                                         
                                         hospital <- as.character(tail(sorted_data_for_state[,"Hospital.Name"], n = 1))
                                       }
                                       else 
                                       {  if(num >nrow((sorted_data_for_state))) hospital <-NA
                                       else
                                       {
                                         hospital <- sorted_data_for_state[,"Hospital.Name"][num]
                                       }
                                       }
                                       c(hospital,state)
                                     } 
  )
  )
  # return it a a data frame
  colnames(dataset)<- c("hospital","state")
  dataframe <- as.data.frame(dataset)
  dataframe <- dataframe[ order(dataframe[,"state"]),]
  
  
}


r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
