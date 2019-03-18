
eprime_setup_dirs <- function(path){
  
  #---set dir struct (follows directory structure in README.txt)
  dirstruct = paste0(path,
                     c("1-ID_dataframes","2-COMPLETE_etxt","3-COMPLETE_edat","4-ERROR_check",
                       "5-COPY_etxt","6-COPY_edat","7-FATAL","8-LOGS")
  )
  
  
  
  for (d in dirstruct) {
    if (!dir.exists(d)) {
      cat(paste("\n\ncreating", d),sep = "\n")
      dir.create(d)
    }
  }
  
  #---paths
  paths = list(
    outpath = paste(path, dirstruct[1],sep="/"),
    outpath_etxt = paste(path, dirstruct[2],sep="/"),
    outpath_edat = paste(path, dirstruct[3],sep="/"),
    path_error = paste(path,dirstruct[4],sep="/"),
    path_etxt = paste(path,dirstruct[5],sep="/"),
    path_edat = paste(path,dirstruct[6],sep="/"),
    # path_etxt = paste(path,"tmp_copy_etxt",sep="/"),
    # path_edat = paste(path,"tmp_copy_edat",sep="/"),
    path_fatal = paste(path,dirstruct[7],sep="/")
  )
  #---logs
  logs = list(
    path_lifecyclelogs = paste(path,dirstruct[8],sep="/"),
    donelog = paste(path,"COMPLETE.log",sep="/"),
    errorlog = paste(path,"ERROR-check.log",sep="/"),
    sessionlog = paste(path,"SESSION.log",sep="/"),
    filelog = paste(path,"ALL_FILES.log",sep="/")
  )
  
  # SESSION.log ----
  file.create(logs$sessionlog)
  
  #---COMPLETE.log
  if (file.exists(logs$donelog)) {
    loglines = readLines(logs$donelog)
  } else {
    file.create(logs$donelog)
    loglines = readLines(logs$donelog)
  }
  
  
  #---ERRORS.log
  if (file.exists(errorlog)) {
    errorlines = readLines(errorlog)
  } else { 
    file.create(errorlog)
    errorlines = readLines(errorlog)
  }
  
  
  return(list(paths = paths, logs = logs))
}
