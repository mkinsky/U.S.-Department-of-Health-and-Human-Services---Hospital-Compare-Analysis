rankall <- function(outcome, num = "best") {
  wd <- "C:\\Education\\R\\Homework\\rprog-data-ProgAssignment3-data"
  setwd(wd)
  
  #Read csv file
  df.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  
  #Load libraries
  library(Hmisc)
  library(doBy)
  
  #Set outcome vectors
  outcome.vector.char <- c("heart attack", "heart failure", "pneumonia")  
  outcome.heart.attack <- 1
  outcome.heart.failure <- 2
  outcome.pneumonia <- 3  
  outcome.vector.int <- c(outcome.heart.attack:outcome.pneumonia)
  outcome.selection <- match(outcome, outcome.vector.char)
  
  #Check for valid outcome
  if (!(outcome.selection %in% outcome.vector.int))
    stop ("invalid outcome")
  
  #Set column positions
  col.hospital.name <- 2
  col.hospital.state <- 7
  col.heart.attack <- 11
  col.heart.failure <- 17
  col.pneumonia <- 23
  
  #Subset df.outcome to include the specified columns
  col.reqd <- NULL
  if (outcome.selection == outcome.heart.attack)
    col.reqd <- 11
  
  if (outcome.selection == outcome.heart.failure)
    col.reqd <- 17
  
  if (outcome.selection == outcome.pneumonia)
    col.reqd <- 23
  df.outcome <- as.data.frame(df.outcome[c(col.hospital.name,col.hospital.state,col.reqd)])
  df.outcome$rank <- as.numeric(0)
  colnames(df.outcome) <- c("hospital", "state", "death.rate", "rank")
  
  #Create df.state
  df.state <- as.data.frame(unique(df.outcome$state))
  if (nrow(df.state) == 0)
    stop ("invalid state")
  colnames(df.state) <- c("state")
  
  #Cleanup df.outcome
  df.outcome <- subset(df.outcome, df.outcome$death.rate != "Not Available")  #Remove NA
  df.outcome$death.rate <- as.numeric(df.outcome$death.rate) #Force death.rate col to be numeric
  df.outcome <- df.outcome[order(df.outcome$state, df.outcome$death.rate, df.outcome$hospital), ] #Sort the dataset
  df.outcome$rank <- with( df.outcome, ave( state, state, state, FUN=seq))
  
  #Check num for valid values
  df.ranking <- as.data.frame(0)
  if (typeof(num) == "integer" | typeof(num) == "double") {
    if (num <= max(df.outcome$rank)) {
      df.ranking <- num
    } else {
      return(NA)
    }
  }
  
  if (typeof(num) == "character") {
    if (num == "best") {
      df.ranking <- 1 
    }
    else if (num == "worst") {
      df.outcome <- merge(df.state, df.outcome, by = "state", all.x = T)
      df.outcome[is.na(df.outcome$death.rate), 3:4] <- 10000
      df.outcome <- ddply(df.outcome, 'state', function(x) x[x$death.rate==max(x$death.rate),])
      df.outcome <- ddply(df.outcome, 'state', function(x) x[x$rank==min(x$rank),])
      df.outcome <- df.outcome[order(df.outcome$state), ] #Sort the dataset
      df.return <- df.outcome[,2:1]
      colnames(df.return) <- c("hospital", "state")
      return(df.return)
    }
  }
  
  #Subset df.outcome to contain those records which equal num
  df.outcome <- subset(df.outcome, rank == df.ranking)
  df.outcome <- merge(df.state, df.outcome, by = "state", all.x = T) #merge df.outcome with df.state to ensure all states are represented in df.outcome
  df.outcome <- df.outcome[order(df.outcome$state), ] #Sort the dataset
  
  df.return <- df.outcome[,2:1]
  colnames(df.return) <- c("hospital", "state")
  return(df.return)
}
#9BGPwzPMER