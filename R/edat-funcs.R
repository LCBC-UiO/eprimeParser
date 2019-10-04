edat_find <- function(path, pattern, paths, logs, log_file, quietly = FALSE){
  edat_file <- list.files(path, pattern=pattern)
  if(length(edat_file)>0){
    msg <- paste0("Moving companion edat to error folder.\n")
    log_it(string = msg, type = "error", 
           logs = c(logs["error"], log_file), quietly = quietly)
    
    in_file <- paste(paths$raw_edat, edat_file, sep="/")
    out_file <- paste(paths$error, edat_file, sep="/")
    
    copy_file(in_file, out_file, paths, logs, log_file, quietly = quietly)
  }
}
