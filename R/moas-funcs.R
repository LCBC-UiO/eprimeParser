
get_moas <- function(moas){
  if(is.data.frame(moas)){
    MOAS = moas 
  }else if(file.exists(moas)){
    MOAS = rio::import(moas)
  }else{
    stop("MOAS not supplied. Either provide path to MOAS.RData, or a pre-loaded MOAS.")
  }
  
  # Make sure ID is integer, easier handling
  MOAS %>% 
    dplyr::mutate(CrossProject_ID = as.integer(as.character(CrossProject_ID))) %>% 
    dplyr::select(CrossProject_ID,
                  Subject_Timepoint,
                  Project_Wave,
                  Project_Name,
                  Project_Wave_ID,
                  Test_Date,
                  Age,
                  Sex,
                  Birth_Date) %>% 
    dplyr::distinct()
}

find_in_moas <- function(subject, date, MOAS){
  MOAS <- get_moas(MOAS)
  
  # Xref ID and date with MOAS
  if (as.integer(subject) < 1000000) {
    
    # deal with leading zeros
    subject <- leading_zero(subject, width = 3)
    
    inMOAS <- MOAS %>% 
      dplyr::filter(Project_Wave_ID == subject,
                    Test_Date == date) 
    
  } else {
    
    inMOAS <- MOAS %>% 
      dplyr::filter(CrossProject_ID == as.numeric(subject),
                    Test_Date == date)
  }
  
  inMOAS %>% 
    dplyr::select(CrossProject_ID, Subject_Timepoint,Project_Name, Project_Wave)
}


