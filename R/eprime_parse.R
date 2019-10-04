#' Parse LCBC E-prime files
#' 
#' Will look for new eprime files, and start
#' logging information about them.
#' 
#' @param in_dir Directory with E-prime files
#' @param out_dir directory to output the data to
#' @param task Which task, one of ["Antisaccade", "n-back", "Attention"]
#' @param MOAS Either string with path to, or as data.frame, the MOAS dataframe
#' @param quietly logical if log text should be printed in the console
#'
#' @export
#' @importFrom dplyr mutate select distinct
eprime_parse <- function(in_dir = "/Users/athanasm/LCBC/Projects/Cross_projects/computer_tasks",
                          out_dir = "/Users/athanasm/Desktop/tasks",
                          task = "n-back",
                          MOAS = "/Users/athanasm/LCBC/Projects/Cross_projects/MOAS/Data/MOAS.RData",
                          quietly = FALSE
){
  
  k <- sapply(c(in_dir, out_dir),
              check_tilde_paths)
  
  # Initiate processing----
  MOAS <- get_moas(MOAS)
  
  # Set up logs, and get their paths
  logs <- eprime_setup_logs(out_dir, task, quietly = quietly)
  
  # Set up directories, and get their paths
  paths <- eprime_setup_paths(out_dir, task, logs, quietly = quietly)
  
  # Search for files and do initial checks
  to_process <- eprime_find(in_dir = in_dir, 
                            task = task,
                            paths = paths,
                            logs = logs, 
                            quietly = quietly)
  
  # Do initial check and copy over files to raw folder
  eprime_setup_files(ff_df = to_process,
                     in_dir = in_dir, 
                     task = task,
                     paths = paths,
                     logs = logs,
                     quietly = quietly)
  
  ## Checks ----
  # Check against number of expected lines
  to_process <- check_nlines(to_process, task, paths = paths,
                             logs = logs[c("session", "file", "fatal", "status")],
                             quietly = quietly)
  
  # Find duplicated file names, ignore extra information  
  to_process <- check_duplicates(to_process, task, paths = paths,
                                 logs = logs[c("session", "file", "error", "status")],
                                 quietly = quietly)
  
  # Remove full_path column. no longer needed
  to_process <- to_process %>% 
    arrange(date)
  
  # PROCESS ----
  # read eprime / etxt files in copy dir
  eprime_process_all(to_process, in_dir, task, MOAS, paths, logs, quietly = quietly)
  
  # Arrange statusfile so that they are ordered by status column
  status <- read_tsv(logs$status, col_types = cols()) %>% 
    arrange(status, date) 
  write_tsv(status, logs$status, na = "")
  
  
  log_it(paste0("\n\nFINISHED\nSee", logs$session, "for full output."), 
         logs = logs$session, quietly = quietly)
  
  if(any(status$status %in% c("error", "needs resolution", "unprocessed"))){
    log_it(paste0("\nThere are files with issues that might be resolved by manual edits\nSee ", logs$session, " for full output."), 
           logs = logs$session, quietly = quietly)
  }
  
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("etxt", "toc",
                                                        "CrossProject_ID",
                                                        "Subject_Timepoint",
                                                        "Project_Wave",
                                                        "Project_Name",
                                                        "Project_Wave_ID",
                                                        "Test_Date",
                                                        "Age",
                                                        "Sex",
                                                        "Birth_Date",
                                                        "timecheck"))
