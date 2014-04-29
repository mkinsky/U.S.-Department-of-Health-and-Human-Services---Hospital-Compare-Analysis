best <- function(state, outcome) {
  #Set Working Directory
  wd <- "C:\\Education\\R\\Homework\\rprog-data-ProgAssignment3-data"
  setwd(wd)  
  rm(wd)
  
  #Load libraries
  library(Hmisc)
  library(doBy)
  
  #Set outcome vectors
  outcome_vector_char <- c("heart attack", "heart failure", "pneumonia")  
  outcome_heart_attack <- 1
  outcome_heart_failure <- 2
  outcome_pneumonia <- 3  
  outcome_vector_int <- c(outcome_heart_attack:outcome_pneumonia)
  outcome_selection <- match(outcome, outcome_vector_char)
  
  #Check for valid outcome
  if (!(outcome_selection %in% outcome_vector_int))
    stop ("invalid outcome")
  
  #Set column positions
  heart_attack_col <- 11
  heart_failure_col <- 17
  pneumonia_col <- 23
  hospital_name_col <- 47
  hospital_state_col <- 52
  
  #Create merged data frame
  df_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  df_hospital <- read.csv("hospital-data.csv", colClasses = "character")
  df_outcome_hospital <- merge(df_outcome, df_hospital, by = "Provider.Number")
  rm(df_outcome)
  rm(df_hospital)
  
  #write.csv(df_outcome_hospital,file="outcome_hospital.csv")
  
  #Subset df_outcome_hospital to include the specified columns
  if (outcome_selection == outcome_heart_attack)
    df_outcome_hospital_req_cols <- as.data.frame(df_outcome_hospital[c(hospital_state_col, hospital_name_col, heart_attack_col)])
  else if (outcome_selection == outcome_heart_failure)
      df_outcome_hospital_req_cols <- as.data.frame(df_outcome_hospital[c(hospital_state_col, hospital_name_col, heart_failure_col)])
  else
    df_outcome_hospital_req_cols <- as.data.frame(df_outcome_hospital[c(hospital_state_col, hospital_name_col, pneumonia_col)])
  
  colnames(df_outcome_hospital_req_cols) <- c("State", "Hospital_Name", "Death_Rate")
  rm(df_outcome_hospital)
  
  #Reorder column positions
  hospital_state_col <- 1
  hospital_name_col <- 2
  death_rate_col <- 3
  
  df_state_match <- subset(df_outcome_hospital_req_cols, df_outcome_hospital_req_cols[hospital_state_col] == state)
  
  #Check for valid state
  if (nrow(df_state_match) == 0)
    stop ("invalid state")
  
  df_state_match_without_na <- subset(df_state_match, df_state_match[death_rate_col] != "Not Available")
  df_state_match_without_na[, death_rate_col] <- as.numeric(df_state_match_without_na[, death_rate_col])
  df_min_death_rates <- as.data.frame(summaryBy(Death_Rate~State, data=df_state_match_without_na, FUN=c(min)))
  colnames(df_min_death_rates) <- c("State", "Min_Death_Rate")
  
  df_hospital_names <- subset(x = df_state_match_without_na, subset = df_state_match_without_na$Death_Rate == df_min_death_rates$Min_Death_Rate, select = hospital_name_col)  
  df_hospital_names_sorted <- sort(as.character(df_hospital_names$Hospital_Name))
  
  return(df_hospital_names_sorted[1])
}