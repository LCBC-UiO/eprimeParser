
#' Set up directories for E-prime parsing
#' 
#' Sets up the necessary folder structure, if it
#' does not already exists, for the eprime
#' parser to work
#'
#' @param path Path to where the directories should be
#' placed
#'
#' @return list of paths
#' @export
eprime_setup_dirs <- function(path){
  
  #---set dir struct (follows directory structure in README.txt)
  paths = as.list(
    paste(path, c("1-ID_dataframes","2-COMPLETE_etxt","3-COMPLETE_edat","4-ERROR_check",
                  "5-COPY_etxt","6-COPY_edat","7-FATAL","8-LOGS"),
          sep = "/")
  )
  names(paths) = c("outpath", "outpath_etxt", "outpath_edat", 
                   "path_error", "path_etxt", "path_edat", 
                   "path_fatal", "path_lifecyclelogs")
  
  exs <- !unlist(lapply(paths, dir.exists))
  if(any(exs)){
    cat(crayon::white("Setting up necessary folder structure"))
    lapply(paths[exs], dir.create, recursive = TRUE)
  } 
  
  paths
}


eprime_check_info <- function(file, filestamp){
  info = readLines(file)
  list(
    filesub = strsplit(info," ; ")[[1]][1] == filestamp$subcheck,
    filedate = strsplit(info," ; ")[[1]][2] == filestamp$datecheck,
    filetime = strsplit(info," ; ")[[1]][3] == filestamp$timecheck
  )
}

fix_ids <- function(subPC){
  if (nchar(as.integer(subPC)) != nchar(subPC)) {
    
    if (nchar(as.integer(subPC)) < 3) {
      #add
      sub_alt = subPC
      zeropad = 3 - nchar(as.integer(subPC))
      subPC = paste0(strrep("0", zeropad), as.integer(subPC))
    } else {
      #remove
      sub_alt = subPC
      cut = nchar(subPC) - nchar(as.integer(subPC))
      subPC = substr(subPC, 1 + cut, nchar(subPC))
    }
    
    # subPC_altered = T
  }
  
  subPC
  # subPC_altered = T
}


rename_destfile <- function(destfile, efile, paths, logs, lifecyclelog){
  if (!file.exists(destfile)) {
    
    msg = paste("Moving", efile, "to", paths$outpath_etxt)
    log_it(msg, type = "message", logs = list(logs$sessionlog,
                                            lifecyclelog))          
    
    file.rename(paste(paths$path_etxt, efile, sep="/"), destfile) 
    
  } else {
    
    msg = paste("Cannot move", efile, "to", destfile, 
                "\nFile exists. Check this. Quitting")
    log_it(msg, type = "error", logs = list(logs$sessionlog,
                                            lifecyclelog))
    stop()
  }
}

move_destfile <- function(data, destfile, logs, lifecyclelog){
  if (!file.exists(destfile)) {
    msg = paste("writing data to", destfile)
    log_it(msg, type = "message", logs = list(logs$sessionlog,
                                            lifecyclelog))
    utils::write.csv(data, destfile, row.names = F)
    
  } else {
    
    msg = paste(destfile,"exists: Check this. Quitting")
    log_it(msg, type = "error", logs = list(logs$sessionlog,
                                            lifecyclelog))
    stop()
  }
}


log_it <- function(string, type = "message", logs = NULL){
  
  if(!is.null(logs)){
    # add to logs
    t <- paste0(type, ": ", string)
    
    lapply(logs, function(x) 
      cat(t, file = x, append = T, sep = "\n")
    )
  }
  
  # Print it
  msg <- switch(type,
                "message" = crayon::white((string)),
                "warning" = crayon::blue((string)),
                "error" =   crayon::red((string))
  )
  
  cat(msg)
}

