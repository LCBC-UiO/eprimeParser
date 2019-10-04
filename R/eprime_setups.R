#' Set up directories for E-prime parsing
#' 
#' Sets up the necessary folder structure, if it
#' does not already exists, for the eprime
#' parser to work
#' 
#' @inheritParams eprime_find
#' @inheritParams eprime_parse
#'
#' @return list of paths
#' @export
eprime_setup_paths <- function(out_dir, task, logs, quietly){
  
  paths <- as.list(paste(out_dir, task, 
                         c("data-raw/etxt",
                           "data-raw/edat",
                           "data-raw/error",
                           "data-raw/fatal",
                           "data/etxt",
                           "data/edat",
                           "data/tsv",
                           ""),
                         sep = "/"))
  names(paths) = c("raw_etxt", "raw_edat", "error", "fatal", "etxt", "edat", "tsv", "main")
  
  exs <- !unlist(lapply(paths, dir.exists))
  if(any(exs)){
    log_it("Setting up necessary folder structure", 
           type = "message", logs = logs$session, quietly = quietly)
    
    lapply(paths[exs], dir.create, recursive = TRUE)
  } 
  
  # # Create another layer for the error folders
  # exs <- !unlist(lapply(paths, dir.exists))
  # if(any(exs)){
  #   t <- as.list(paste(out_dir, task, 
  #                      c("data-raw/error/etxt",
  #                        "data-raw/error/edat"), sep = "/"))
  #   
  #   lapply(t[exs], dir.create, recursive = TRUE)
  # } 
  # 
  paths
}


#' Set up logs for eprime parser
#' 
#' Initiates and creates paths to necessary
#' log-files for the E-prime parser
#'
#' @inheritParams eprime_find
#' @inheritParams eprime_parse
#'
#' @return list of log paths
#' @export
eprime_setup_logs <- function(out_dir, task, quietly = FALSE){
  
  logs <- as.list(paste(out_dir,task, "logs",
                        c("fatal.log",
                          "error.log",
                          "status.log",
                          paste0("files/FILE.md"),
                          paste0("sessions/session-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),".md")),
                        sep = "/"))
  names(logs) = c("fatal", "error", "status", "file", "session")
  
  # Create session log, setup the structure if necessary
  if(!dir.exists(dirname(logs$session))){
    dir.create(dirname(logs$session), recursive = TRUE)
    first_time="Setting up logs for the first time"
  }
  file.create(logs$session)
  
  # Setup individual file log folder
  if(!dir.exists(dirname(logs$file)))
    dir.create(dirname(logs$file), recursive = TRUE)
  
  log_it(paste("# Starting to process on", Sys.time()),
         logs = list(logs$session), quietly = quietly)
  if(exists("first_time")) log_it(first_time, 
                                  logs = list(logs$session),
                                  quietly = quietly)
  
  logs
  
}


#' Setup the raw files in the system
#' 
#' Initiates logs for each new file, and
#' copies the raw data into the folder
#' structure
#'
#' @inheritParams eprime_process_all
#' @inheritParams eprime_find
#' @inheritParams eprime_parse
#' @inheritParams eprime_setup_logs
#'
#' @export
eprime_setup_files <- function(ff_df, in_dir, task, logs, paths, quietly = FALSE){
  
  log_it(paste("Initiating lifetime logs for each file."),
         type = "message", logs = logs["session"],
         quietly = quietly)
  
  for(i in 1:nrow(ff_df)){
    msg <- paste0("# Initiating lifetime log for file on ", Sys.time(),"\n")
    cat(msg, file = gsub("FILE", ff_df$files_date_time[i], logs$file), 
        append = TRUE, sep = "\n")
  }
  
  copy_raw(ff_df, in_dir, paths, logs, quietly)
}
