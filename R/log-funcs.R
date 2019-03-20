parser_error_log <- function(SEARCHKEY, errordir,
                             lifecyclelog, sub, logs,
                             checkMOASdate, checkMOAS
                             ){
  #---log error
  errorinfo = paste(SEARCHKEY, basename(errordir), sep = " ; " )
  cat(errorinfo, file = paste(errordir, "INFO-efile.csv", sep = "/"), append = T, sep = "\n")
  
  #---copy session log
  file.copy(lifecyclelog, paste(errordir, "session.log", sep="/"))
  
  #---write alternatives
  utils::write.csv(checkMOASdate, paste(errordir, "DATE-check.csv", sep = "/"))
  utils::write.csv(checkMOAS, paste(errordir, "ID-check.csv", sep = "/"))
  
  #---write PRECLEAN data
  datafile = paste0(sub$subPC, "_PRECLEAN-data.csv")
  utils::write.csv(sub$subcsv, paste(errordir, datafile, sep = "/"), row.names = F)
  
  #---update logs
  msg = paste("updating ERROR-check.log:", basename(errordir))
  log_it(msg, type = "error", logs = list(logs$sessionlog,
                                          lifecyclelog,
                                          logs$errorlog))
  
  # msg = paste("updating ALL_FILES.log:", basename(errordir))
  # cat("\n",msg)
  # cat(msg, file = sessionlog, append = T, sep = "\n")
  # cat(msg, file = lifecyclelog, append = T, sep = "\n")
  # 
  # ALLlines = readLines(filelog)
  # whichline = grep(SEARCHKEY, ALLlines)
  # line = ALLlines[whichline]
  # 
  # msg = paste0(line, " ; ", basename(errordir))
  # newloglines = ALLlines
  # newloglines[whichline] = msg
  # #--overwrite
  # writeLines(newloglines, filelog) 
}

#' Set up logs for eprime parser
#' 
#' Initiates and creates paths to necessary
#' log-files for the E-prime parser
#'
#' @param path Path to where the logs should be
#'
#' @return list of log paths
#' @export
eprime_setup_logs <- function(path){
  logs <- as.list(paste(path, 
                        c("COMPLETE.log", "ERROR-check.log",
                          "SESSION.log", "ALL_FILES.log"),
                        sep = "/"))
  names(logs) = c("donelog", "errorlog", "sessionlog", "filelog")  
  
  
  # SESSION.log ----
  file.create(logs$sessionlog)
  
  #---COMPLETE.log
  loglines <- check_create_log(logs$donelog)
  
  #---ERRORS.log
  errorlines <- check_create_log(logs$errorlog)
  
  list(logs = logs, errorlines = errorlines, loglines = loglines)
}


check_create_log <- function(file){
  if (file.exists(file)) {
    tmp = readLines(file)
  } else {
    file.create(file)
    tmp = readLines(file)
  }
  tmp
}

