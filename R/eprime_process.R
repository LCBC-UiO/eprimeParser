#' Process an E-prime file for LCBC
#' 
#' Runs the processing of E-prime files for LCBC. 
#' This will check the existance of files, any
#' previous logs for duplicated entries etc.
#' 
#' @inheritParams eprime_parse
#' @inheritParams eprime_find
#' @param efile_path path to E-prime txt file
#' @param start_message string message to add to log for this action
#' @param return_options logical if options generated for the file should
#' be returned to user
#'
#' @importFrom dplyr mutate select arrange desc filter
#' @importFrom rprime read_eprime FrameList keep_levels to_data_frame drop_levels
#' @importFrom utils head tail write.csv
#' @importFrom readr read_tsv write_tsv cols col_double col_character
#' @importFrom purrr is_empty
#' @importFrom magrittr '%>%'
#' @export
eprime_process_file <- function(efile_path, in_dir, task, MOAS, paths, logs, 
                                start_message = "Starting processing for",
                                quietly = FALSE, 
                                return_options = FALSE){
  
  efile <- barename(efile_path)
  
  # Define distinct files for this single file processing
  file_log <- gsub("FILE", efile, logs$file)
  
  msg = paste("\n#", start_message, efile)
  log_it(msg, type = "message", logs = list(logs$session,
                                            file_log),  
         quietly = quietly)
  
  # Read in etxt file and create data.frame
  eData <- etxt_clean(efile_path, task)
  
  subject <- eData$Project_Wave_ID[1]
  date <- eData$SessionDate[1]
  
  
  # check 2: MOAS ---- 
  msg = paste0("\n\n## Check 2: subject ID in MOAS\nChecking if subID (", subject, 
               ") matches with Project_Wave_ID/CrossProject_ID and Test_Date in MOAS")
  log_it(msg, type = "message", logs = list(logs$session,
                                            file_log), 
         quietly = quietly)
  
  inMOAS <- find_in_moas(subject, date, MOAS)
  
  if( nrow(inMOAS) > 0 ){
    
    # check success ----
    msg = paste0("Matched ", subject, " to ", inMOAS$CrossProject_ID[1], " on ", date, " in the MOAS")
    log_it(msg, type = "ok",
           logs = list(logs$session, file_log), 
           quietly = quietly)
    
    # eData out
    eData = eData %>% 
      dplyr::mutate(CrossProject_ID = inMOAS$CrossProject_ID[1]) %>% 
      dplyr::left_join(inMOAS, by="CrossProject_ID") %>% 
      dplyr::select(CrossProject_ID, Project_Name, Project_Wave, 
                    Subject_Timepoint, SessionDate, SessionTime, dplyr::everything())
    
    out_tsv <-  paste0(paths$tsv,"/", efile, ".tsv")
    
    log_it(paste0("Writing clean data to ", out_tsv), 
           logs = list(logs$session, file_log), 
           quietly = quietly)
    
    readr::write_tsv(eData, path = out_tsv)
    
    # etxt out 
    copy_file(in_file = efile_path, 
              out_file = paste0(paths$etxt,"/", efile, ".txt"),
              paths = paths, 
              logs = logs,
              log_file = file_log, 
              quietly = quietly)
    
    # Check if session is correct, fix it if not
    new_file <- check_session(efile_path, inMOAS$Subject_Timepoint[1], 
                              logs = list(logs$session, file_log),
                              quietly = quietly)
    
    if(!is.null(new_file)){
      update_filenames(efile_path, new_file, paths, list(logs$session, file_log),
                       quietly = quietly)
    }
    
    log_it("File completed processing.  ", 
           logs = list(logs$session, file_log), quietly = quietly )
    
    STATUS <- "processed"
    COMMENT <- "complete"
    
  } else {
    
    # check fail ----
    msg = "**error:** subject ID and test date do not correspond with MOAS\nChecking MOAS for possible subID's based on Test_Date"
    log_it(msg, type = "error", 
           logs = list(logs$session, file_log),
           quietly = quietly)
    
    STATUS <- "needs resolution"
    COMMENT <- "check logs"
    
    # Get ID and date alternatives from MOAS
    MOAS_subj_alts <- check_MOAS_subj(MOAS, subject)
    if(nrow(MOAS_subj_alts) == 0){
      msg <- paste0("**Warning:** No ID ", subject, " found in the MOAS.\n  ID is either mistyped or ahead of the MOAS in time.")
      log_it(msg, type = "warning", 
             logs = list(logs$session, file_log),
             quietly = quietly)
      
      COMMENT <- "no ID match - check logs"
    }else{
      msg <- paste0("ID matches are being added to the logs")
      log_it(msg, type = "warning", 
             logs = list(logs$session, file_log),
             quietly = quietly)
      
      log_it(MOAS_subj_alts, type = "warning", 
             logs = list(logs$session, file_log),
             quietly = quietly)
      
      COMMENT <- "Alternatives in logs"
    }
    
    msg = paste("Checking for ID's tested on", date)
    log_it(msg, type = "message", logs = list(logs$session, file_log),
           quietly = quietly)
    
    MOAS_date_alts <- check_MOAS_date(MOAS, date)
    if(nrow(MOAS_date_alts) == 0){
      msg <- paste0("**Warning:** No data found for date ", date, " in the MOAS.\n  This data is ahead of the MOAS in time.")
      log_it(msg, type = "warning", 
             logs = list(logs$session, file_log),
             quietly = quietly)
      
      STATUS <- "unprocessed"
      COMMENT <- "awaiting new MOAS"
    }else{
      msg <- paste0("Date matches are being added to the logs")
      log_it(msg, type = "warning", 
             logs = list(logs$session, file_log),
             quietly = quietly)
      
      log_it(MOAS_date_alts, type = "warning", 
             logs = list(logs$session, file_log),
             quietly = quietly)
      
      COMMENT <- "Alternatives in logs"
    }
    
    if(nrow(MOAS_date_alts)>0 | nrow(MOAS_subj_alts)>0){
      
      #cat("writing error files")
      msg = paste0("Writing Date and subID alternatives to ", paths$error,
                   "\nMoving ", efile, " to ", paths$error)
      log_it(msg, type = "message", logs = list(logs$session, file_log, logs$error),
             quietly = quietly)
      
      t <- file.rename(efile_path, paste0(paths$error, "/",  efile, ".txt")) 
      
    }
  }
  
  
  # Write to status log
  t <- update_status(status_path = logs$status,
                     condition = files_date_time == gsub("\\.txt", "", basename(efile_path)),
                     comment_string = COMMENT,
                     status_string = STATUS)
  
  if(return_options & nrow(inMOAS) == 0 ){
    rbind.data.frame(MOAS_subj_alts, MOAS_date_alts)
  }
}


#' Process all files
#' 
#' Calls eprime_process_file and runs through each file listed in the
#' provided data.frame to process.
#'
#' @param ff_df data frame of files to process from status.log
#' @inheritParams eprime_parse
#' @inheritParams eprime_find
#'
#' @export
eprime_process_all <- function(ff_df, in_dir, task, MOAS, paths, logs, quietly = FALSE){
  for (i in 1:nrow(ff_df)) {
    efile <- paste0(paths$raw_etxt,"/", ff_df$files_date_time[i], ".txt")
    eprime_process_file(efile_path = efile, 
                        in_dir, task, MOAS, paths, logs,  
                        quietly = quietly)
  }
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("CrossProject_ID", 
                                                        "Test_Date",
                                                        "Project_Wave_ID", 
                                                        "timecheck", 
                                                        "SessionTime",
                                                        "SessionDate",
                                                        "files_date_time", 
                                                        ".",
                                                        "efile"))

