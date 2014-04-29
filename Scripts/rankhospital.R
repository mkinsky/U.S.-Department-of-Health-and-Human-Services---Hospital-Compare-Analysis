rankhospital <- function(state, outcome, num) {
  #Set Working Directory
  wd <- "C:\\Education\\R\\Homework\\rprog-data-ProgAssignment3-data"
  setwd(wd)  
  rm(wd)
    
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
  rank_col <- 4
  
  df_state_match <- subset(df_outcome_hospital_req_cols, df_outcome_hospital_req_cols[hospital_state_col] == state)
  
  #Check for valid state
  if (nrow(df_state_match) == 0)
    stop ("invalid state")
  
  df_state_match_without_na <- subset(df_state_match, df_state_match[death_rate_col] != "Not Available")
  df_state_match_without_na[, death_rate_col] <- as.numeric(df_state_match_without_na[, death_rate_col])  
  df_state_match_without_na[, hospital_name_col] <- as.character(df_state_match_without_na[, hospital_name_col])  
  df_state_match_without_na_sorted <- df_state_match_without_na[order(df_state_match_without_na$Death_Rate, df_state_match_without_na$Hospital_Name), ]
  df_state_match_without_na_sorted$Rank <- with( df_state_match_without_na_sorted, ave( State, State, State, FUN=seq))
  df_state_match_without_na_sorted[, rank_col] <- as.numeric(df_state_match_without_na_sorted[, rank_col])  
  df_state_match_without_na_sorted_rowcount <- nrow(df_state_match_without_na_sorted)
  
  rm(df_state_match)
  rm(df_state_match_without_na)
  
  return_result_set <- ""
  df_ranking <- 0
  #Check num for valid values
  if (typeof(num) == "integer" | typeof(num) == "double") {
    if (num <= df_state_match_without_na_sorted_rowcount) {
      df_ranking <- num    
    } else {
      return_result_set <- "NA" 
    }
  }
    
  if (typeof(num) == "character") {
    if (num == "best") {
      df_ranking <- 1 
    }
    else if (num == "worst") {
      df_ranking <- df_state_match_without_na_sorted_rowcount 
    }
  }
      
  if (df_ranking > 0) {
    df_hospital_rank <- subset(df_state_match_without_na_sorted, Rank == df_ranking)
    return_result_set <- df_hospital_rank$Hospital_Name
  }
  rm(df_state_match_without_na_sorted)
  
  if (return_result_set == "NA")
      return(NA)
  else        
    return(as.character(return_result_set))
}