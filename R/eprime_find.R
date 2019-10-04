#' Find eprime files
#'
#' @inheritParams eprime_parse
#' @param paths list of paths from [\code{eprime_setup_dirs}]
#' @param logs list of paths from [\code{eprime_setup_logs}]
#' 
#' @export
#' @importFrom dplyr bind_rows mutate filter
#' @importFrom readr guess_encoding read_tsv write_tsv
eprime_find <- function(in_dir,
                        task,
                        paths, 
                        logs,
                        quietly = FALSE){
  
  log_it(paste("Searching for", task, "files in", in_dir, "."),
         type = "message", logs = list(logs$session), quietly = quietly)
  
  # Files ----
  # Find all txt files in in_dir for this task
  ff <- list.files(in_dir, 
                   pattern=paste0(task,".*txt"), 
                   recursive = T, 
                   full.names = T) 
  
  if(length(ff) == 0){
    msg <- paste0("No files found for task '", task, "'. Please check if this is a task with data.")
    log_it(msg, type = "error", logs = logs$session, quietly)
    stop_quietly()
  }
  
  # Initiate the txt files, and remove _known_ untrue files
  # And duplicates from copying files around
  ff_df <-  etxt_initiate(ff, task = task, in_dir = in_dir) %>% 
    filter(!grepl("Copy", files_orig),
           !grepl("/old/", files_orig_path),
           id != 9999999, # Tests ids
           id != 1, # Tests ids
           !duplicated(files_date_time) # File duplications from moving files around
           )
  
  # Start running checks ----
  # Check against existing log
  ff_df <- check_existing(ff_df, logs, quietly)
  
  write_status(ff_df, logs)
  
  ff_df
}



if(getRversion() >= "2.15.1")  utils::globalVariables(c("files_orig", "files_orig_path",
                                                        "id", "files_date_time"))

