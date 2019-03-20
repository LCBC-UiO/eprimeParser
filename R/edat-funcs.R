edat_clean <- function(efile_path, task, efile){
  #---load file & parse contents
  exp_lines = rprime::read_eprime(efile_path)
  experiment_data = rprime::FrameList(exp_lines)
  #preview_levels(experiment_data)
  #preview_frames(experiment_data)
  
  
  #---extract header
  header = rprime::keep_levels(experiment_data, 1)
  header = utils::tail(rprime::to_data_frame(header), 1)
  header_cols =  c("Experiment","Subject","SessionDate","SessionTime")
  header = header[header_cols]
  
  #---remove header
  data = rprime::drop_levels(experiment_data, 1)
  #preview_levels(data)
  
  #--full data extracted
  df_data = rprime::to_data_frame(data)
  
  #---subset df
  data_cols = switch(task ,
                     "n-back" = eprime_task_cols$`n-back`,
                     "Attention" = eprime_task_cols$Attention,
                     "Antisaccade" = eprime_task_cols$Antisaccade
  )
  
  df_data = df_data[data_cols]
  
  
  if (task == "n-back" || task == "Antisaccade") {
    #---add Block col
    df_data = dplyr::mutate(df_data, "Block" = as.integer(rownames(df_data)))
  }
  
  #convert strings to int
  #df_data = readr::type_convert(df_data)
  #str(data)
  
  
  #---create outfile
  subcsv = suppressWarnings(data.frame(header, df_data)) 
  colnames(subcsv)[names(subcsv) == "Subject"] = "Subject_PRECLEAN"
  subcsv = subcsv %>% 
    dplyr::mutate(SessionDate_PRECLEAN = subcsv$SessionDate,
                  Filename = efile,
                  SessionDate = as.Date(SessionDate, format="%m-%d-%Y") )
  
  #PC = "PRECLEAN" 
  subPC = suppressWarnings(as.character(subcsv$Subject[1]))
  
  list(subPC = subPC, 
       subcsv = subcsv)
  
}


edat_fcn <- function(path, efile) {
  
  list.files(path, 
             pattern = gsub("txt", "", efile))
  
  # #change extension to .edat and search
  # strcut <- substr(efile, 1, nchar(efile) -4)
  # strcut_edat <- paste0(strcut, ".edat")
  # regexp <- grep(strcut_edat, edats)
  # 
  # #get true edat
  # edatstr <- edats[regexp]
  # edatnum <- as.data.frame(regexp)
  # 
  # list(strcut = strcut,
  #      strcut_edat = strcut_edat,
  #      regexp = regexp,
  #      edatstr = edatstr,
  #      edatnum = edatnum)
}


edat_check <- function(edat_info, path_edat, efile,
                       sessionlog, lifecyclelog){
  
  if (!is_empty(edat_info)) {
    msg = paste("edat file found:", edat_info)
    edat_same = T
    
  } else {
    msg = paste0("cannot find edat file companion to ", efile, " in ", path_edat)
    edat_same = F
  }
  
  log_it(msg, type=ifelse(edat_same,"message","error"), 
         logs=list(logs$sessionlog,
                   lifecyclelog))
  
  edat_same
}


edat_move <- function(edat_info, outfile, efile, paths, logs, lifecyclelog, SEARCHKEY){
  if (length(edat_info) > 1) { 
    
    log_it(paste("\nMultiple files found (", 
                 length(edat_info), ") containing name", 
                 gsub("txt", "edat?", efile), 
                 ". Will not move to", paths$outpath_edat), 
           type="warning", 
           logs = list(logs$sessionlog,
                       lifecyclelog))
    
  } else if (!is_empty(edat_info)) {
    
    log_it(paste("Moving", edat_info, "to", paths$outpath_edat), 
           type="message", 
           logs = list(logs$sessionlog,
                       lifecyclelog))        
    
    file.rename(paste(paths$path_edat, edat_info, sep="/"), 
                paste(paths$outpath_edat, edat_info ,sep="/")) 
    
    log_it(paste(SEARCHKEY, outfile, edat_info, sep=" ; "), 
           type="message", 
           logs = list(logs$sessionlog,
                       lifecyclelog,
                       logs$donelog))      
  }
}



if(getRversion() >= "2.15.1")  utils::globalVariables(c("SessionDate"))
