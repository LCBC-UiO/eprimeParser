# Single file ----

#' Attempt manual resolve on a file that is not completed
#' 
#' Will attempt to process the file automatically 
#' before prompting for manual input to resolve the issues 
#' that are stopping the file from being processed.
#'
#' @param files_date_time The unique file name for file that 
#' needs processing (found in status.log)
#' @param file_date the date of the session for the file that
#' needs processing (found in status.log)
#' @param gmail_user Email with access to the LCBC test calendar
#' on google
#' @param .choice never to be set by the user. Created for unit testing only
#' @inheritParams eprime_find
#' @inheritParams eprime_parse
#'
#' @export
eprime_resolve_file <- function(files_date_time, file_date, in_dir, 
                                task, paths, logs,  MOAS, 
                                gmail_user = NULL,
                                quietly = FALSE, 
                                .choice = NULL){
  
  if(!dir.exists(paths$main)) stop(paste("There are no files in" , paths$main, ". 
                                         There is nothing to resolve."))
  
  MOAS <- get_moas(MOAS)
  
  efile_path <- paste0(paths$error,"/", files_date_time, ".txt")
  
  efile_orig <- basename(efile_path)
  efile_orig <- gsub(paste0(getExtension(efile_orig),"|[.]"), "", efile_orig)
  
  file_log_orig <- gsub("FILE", efile_orig, logs$file)
  
  opts <- eprime_process_file(efile_path = efile_path, 
                              in_dir, task, MOAS, paths, logs,
                              start_message = "Attempting to reprocess",
                              return_options = TRUE,
                              quietly = quietly) %>% 
    dplyr::mutate(selection = paste(CrossProject_ID, Test_Date, sep=": "))
  
  # If there are options, show them and prompt an answers
  if(nrow(opts) > 0){
    msg <- "Entering manual processing"
    log_it(msg, logs = c(logs$session, file_log_orig), quietly = quietly)
    
    new_info <- check_manual_moas(.choice, opts, efile_orig)
    
    if(is.null(new_info)){
      
      # If choose to check calendar, run through that
      if(!is.null(gmail_user)){
        
        # Unit test hack, do not touch
        if(!is.null(.choice)) .choice = 1
        
        opts <- check_calendar(date = date, userName = gmail_user)
      
      }
      
      # If chosen to do nothing, exit
      if(is.null(new_info)){
        msg = paste("\n", "Chosen action is to not do anything. Leaving file as is to check again later.")
        log_it(msg, type = "message", logs = list(logs$session,
                                                  file_log),
               quietly = quietly)
        stop_quietly()
      } # is.null(new_info)
      
    } # is.null(new_info)
    
    msg = paste("\n#", "Altering files to correspond to manually inputted information")
    log_it(msg, type = "message", logs = list(logs$session,
                                              file_log_orig),
           quietly = quietly)
    
    msg <- paste("Correct ID is selected to be", new_info$new_id, ", and session is corrected to be", new_info$new_ses)
    log_it(msg, type = "message", logs = list(logs$session,
                                              file_log_orig),
           quietly = quietly)
    
    # Define distinct files for this single file processing
    efile_new <- strsplit(efile_orig, "_")[[1]]
    
    orig_id <- gsub("sub-", "", efile_new[1])
    orig_ses <- gsub("ses-|ses-0", "", efile_new[2])
    
    efile_new[1] <- paste0("sub-", new_info$new_id)
    efile_new[2] <- paste0("ses-", leading_zero(new_info$new_ses, 2))
    efile_new <- paste0(paste0(efile_new, collapse="_"))
    
    file_log_new <- gsub("FILE", efile_new, logs$file)
    
    msg = paste("\n", "changing file names from", efile_orig, "to", efile_new)
    log_it(msg, type = "message", logs = list(logs$session,
                                              file_log_orig),
           quietly = quietly)
    
    # Change log name
    t <- file.rename(file_log_orig, file_log_new)
    
    # Find all files in error folder that correspond, and run through them (txt and edat basically)
    fs <- list.files(paths$error, pattern = efile_orig, full.names = TRUE)
    
    # some dont get moved to error, double check in raw
    if(!any(grepl("txt", fs))){
      tt <- list.files(paths$raw_etxt, pattern = efile_orig, full.names = TRUE)
      copy_file(tt, gsub(paths$raw_etxt, paths$error, tt), paths, logs, file_log_orig)
      fs <- list.files(paths$error, pattern = efile_orig, full.names = TRUE)
    }
    
    d <- sapply(fs, function(x)  file.rename(x, gsub(efile_orig, efile_new, x)))
    fs <- gsub(efile_orig, efile_new, fs)
    
    # Move them to data folder
    ffs <- fs[grep("txt", fs)]
    new_ffs <- gsub(paste(paths$error, paths$etxt, sep="|"), paths$etxt, ffs)
    
    move_file(ffs, new_ffs, 
              paths, logs, file_log_new)
    
    # Move edat if that exists
    if(any(grepl("edat", fs))){
      ffs <- fs[grep("edat", fs)]
      new_ffs <- gsub(paste(paths$error, paths$edat, sep="|"), paths$etxt, ffs)
      
      move_file(ffs, new_ffs, 
                paths, logs, file_log_new)
    }
    
    
    msg = paste("\n", "**Note** that edat and etxt files will not have their content altered to reflect the new information. 
                  Only the cleaned tsv and file names will reflect the manually inputted information.  ")
    log_it(msg, type = "warning", logs = list(logs$session,
                                              file_log_orig),
           quietly = quietly)
    
    # Alter tsv file 
    fs <- list.files(paths$etxt, pattern = efile_new, full.names = TRUE)
    
    # Read in etxt file and create data.frame
    eData <- etxt_clean(fs, task) %>% 
      dplyr::mutate(CrossProject_ID = as.numeric(new_info$new_id),
                    Subject_Timepoint = as.numeric(new_info$new_ses)) %>% 
      dplyr::left_join(MOAS %>% dplyr::select(1:4, Test_Date),
                       by = c("CrossProject_ID", "Subject_Timepoint")) %>% 
      dplyr::select(Experiment, CrossProject_ID, Subject_Timepoint, 
                    dplyr::contains("Project"), Test_Date, dplyr::everything())
    
    out_file <- paste0(paths$tsv,"/", efile_new, ".tsv")
    msg = paste("\n", "Writing cleaned tsv to ", out_file)
    log_it(msg, type = "ok", logs = list(logs$session,
                                         file_log_orig),
           quietly = quietly)
    
    readr::write_tsv(eData, out_file, na="")
    
    # Update info in the status log
    update_status_filename(logs$status, 
                           files_date_time == efile_orig, 
                           files_date_time_new = efile_new)
    STATUS <- "processed"
    COMMENT <- "complete-edited"
    t <- update_status(status_path = logs$status,
                       condition = files_date_time == efile_new,
                       comment_string = COMMENT,
                       status_string = STATUS)
    
    
  }
}

# All files ----
#' Attempt manual resolve on all files that are not completed
#' 
#' Will check the status.log for files that need resolution,
#' and attempt to resolve their issues. This inclused attempting
#' to process the files automatically before prompting for manual 
#' input to resolve the issues that are stopping the file from
#' being processed.
#'
#' @inheritParams eprime_find
#' @inheritParams eprime_parse
#' @inheritParams eprime_resolve_file
#'
#' @export
eprime_resolve_all <- function(in_dir, out_dir, task, MOAS,
                               gmail_user = "athanasiamo@gmail.com", 
                               .choice = NULL){
  
  k <- sapply(c(in_dir, out_dir),
              check_tilde_paths)
  
  if(!dir.exists(paste0(out_dir,"/", task))){
    stop(paste0("Directory ", paste0(out_dir,"/", task), " does not exists. No data to resolve."))
  }
  
  # Initiate processing----
  MOAS <- get_moas(MOAS)
  
  # Get logs paths
  logs <- eprime_setup_logs(out_dir, task)
  
  # Get directory paths
  paths <- eprime_setup_paths(out_dir, task, logs)
  
  # Get status.log, and get all that are unprocessed
  to_resolve <- readr::read_tsv(logs$status, col_types = readr::cols()) %>% 
    dplyr::filter(status %in% "needs resolution")
  
  if(nrow(to_resolve) > 0){
    for (i in 1:nrow(to_resolve)) {
      eprime_resolve_file(files_date_time = to_resolve$files_date_time[i], 
                          file_date = to_resolve$date[i], 
                          in_dir, task, paths, logs, MOAS, gmail_user,
                          .choice = .choice
      )
    }
  }else{
    ok("There are no files to resolve.")
  }
  
  
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("status", "MOAS",
                                                        "gmail_user", "file_log",
                                                        "Experiment"))
