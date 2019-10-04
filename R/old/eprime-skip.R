#' #' @importFrom purrr is_empty
#' eprime_skip_done <- function(efile, paths, logs){
#'   
#'   msg = paste0("SEARCHKEY", " found in ", basename(logs$donelog), ": confirming duplicate...")
#'   message(msg); cat(msg, file = logs$sessionlog, append = T, sep = "\n")
#'   
#'   #---check file in COMPLETE_etxt/
#'   if (file.exists(paste(paths$outpath_etxt, efile, sep="/"))) {
#'     
#'     filestampcheck = etxt_fcn(paste(paths$outpath_etxt, efile, sep="/"))
#'     
#'     if (!any(lapply(filestamp, purrr::is_empty))) {
#'       
#'       msg = paste(efile, "EXISTS in", paths$outpath_etxt,"\nConfirmed duplicate: already processed.\nFile will be safely removed from", 
#'                   paths$path_etxt, "\nSkipping")
#'       message(msg); cat(msg, file = logs$sessionlog, append = T, sep = "\n")
#'       
#'       file.remove(paste(paths$path_etxt, efile, sep="/"))
#'       
#'     } else {
#'       msg = paste("ERROR:", efile, "already exists in", paths$outpath_etxt, "but the file is not duplicate. Check this.\nSkipping")
#'       message(msg); cat(msg, file = logs$sessionlog, append = T, sep = "\n")
#'     }
#'     
#'   } else {
#'     
#'     msg = paste("ERROR", efile, "not in", paths$outpath_etxt, "but in COMPLETE.log. Check this and copy/preprocess if necessary.\nSkipping")
#'     message(msg); cat(msg, file = logs$sessionlog, append = T, sep = "\n")
#'   }
#'   
#'   skip = TRUE
#'   skip
#' }
#' 
#' 
#' 
#' eprime_skip_error <- function(efile, paths, logs){
#'   
#'   msg = paste0("SEARCHKEY", " found in ", basename(logs$errorlog), ": confirming duplicate...")
#'   message(msg); 
#'   cat(msg, file = logs$sessionlog, append = T, sep = "\n")
#'   
#'   
#'   #---Xcheck timedate in errorlog
#'   errorlines = readLines(logs$errorlog)
#'   line = errorlines[grep(SEARCHKEY, errorlines)]
#'   tmpstr = strsplit(line," ; ")
#'   
#'   errordir = paste(paths$path_error, tmpstr[[1]][5], sep="/")
#'   
#'   
#'   #---Xcheck timedate in errorlog
#'   if (file.exists(paste(paths$errordir, efile, sep="/"))) {
#'     
#'     fileread = paste(paths$errordir, "INFO-efile.csv", sep="/")
#'     
#'     if (file.exists(fileread)) {
#'       cat("reading in INFO")
#'       info = readLines(fileread)
#'       checks = c(strsplit(info," ; ")[[1]][1] == subcheck,
#'                  strsplit(info," ; ")[[1]][2] == datecheck,
#'                  strsplit(info," ; ")[[1]][3] == timecheck)
#'       
#'     } else {
#'       cat("reading info from file")
#'       filestampcheck = etxt_fcn(paste(paths$errordir, efile, sep="/"))
#'       checks = c(filestampcheck[[1]] == subcheck,
#'                  filestampcheck[[2]] == datecheck,
#'                  filestampcheck[[3]] == timecheck)
#'     }
#'     
#'     if (all(checks)) {
#'       
#'       log_it(paste(efile, "EXISTS in", errordir,"\nConfirmed duplicate: under error handling.\nFile will be safely removed from",
#'                    paths$path_etxt, "\nSkipping"), 
#'              type = "message", 
#'              logs = logs$sessionlog)
#'       
#'       message(msg); cat(msg, file = logs$sessionlog, append = T, sep = "\n")
#'       
#'       file.remove(paste(paths$path_etxt, efile, sep="/"))
#'     }
#'     
#'   } else {
#'     
#'     log_it(paste("SEARCHKEY found in", basename(paths$errordir),"but file not in", errordir,
#'                  "\nMoving", efile, "to", paths$errordir, "\nWill not process. Skipping"), 
#'            type = "warning", 
#'            logs = logs$sessionlog)
#'     
#'     file.rename(paste(paths$path_etxt, efile, sep="/"), paste(paths$errordir, efile, sep="/")) 
#'     
#'     edat_info = edat_fcn(efile)
#'     
#'     if (!purrr::is_empty(edat_info)) { 
#'       log_it(paste("Moving", edat_info, "to", paths$errordir, "\nWill not process. Skipping"), 
#'              type="error", 
#'              logs = list(logs$sessionlog)
#'       )
#'       
#'       file.rename(paste(paths$path_edat, edat_info, sep="/"), 
#'                   paste(paths$errordir, edat_info, sep="/")) 
#'     }
#'     
#'   }
#'   
#'   skip = TRUE
#'   skip
#' }
#' 
#' 
#' #' @importFrom dplyr select arrange
#' #' @importFrom utils head
#' eprime_skip_moas <- function(sessionlog, MOAS, filestamp){
#'   #---skip if Date too recent for MOAS version
#'   recent = MOAS %>% 
#'     dplyr::select(Test_Date) %>% 
#'     unique() %>% 
#'     dplyr::arrange(dplyr::desc(Test_Date)) %>%
#'     utils::head(5)
#'   mostrecent = recent[1,1]
#'   
#'   if (as.integer(as.Date(filestamp$datecheck)-mostrecent) > 1) {
#'     log_it(paste0("Date: ", filestamp$datecheck, " is too recent. Last date logged in MOAS is ", mostrecent,
#'                   ". Update MOAS version or wait to process.\nSkipping"),
#'            type = "error",
#'            logs = list(logs$sessionlog)
#'     )
#'     
#'     skip_recent = TRUE
#'     skip = TRUE
#'     
#'   } else {
#'     skip_recent = FALSE
#'     skip = FALSE
#'   }
#'   
#'   list(skip = skip, skip_recent = skip_recent)
#' }
#' 
#' eprime_skip_check <- function(efile, SEARCHKEY, loglines, errorlines, 
#'                               filestamp, logs, MOAS, paths){
#'   skip_done <- ifelse(any(grep(SEARCHKEY, loglines)), TRUE, FALSE)
#'   skip_error <- ifelse(any(grep(SEARCHKEY, errorlines) ), TRUE, FALSE) 
#'   
#'   skips <- eprime_skip_moas(MOAS = MOAS, sessionlog = logs$sessionlog, filestamp = filestamp)
#'   
#'   skip = !any(!c(skip_error, skip_done, skips$skip_recent))
#'   
#'   if (skip_done) skip <- eprime_skip_done(efile, paths, logs)
#'   if (skip_error) skip <- eprime_skip_error(efile, paths, logs)
#'   
#'   skip
#' }
#' 
#' 
#' 
#' if(getRversion() >= "2.15.1")  utils::globalVariables(c("filestamp", "SEARCHKEY",
#'                                                         "skip_recent", "logs"))
