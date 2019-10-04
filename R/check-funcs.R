check_session <- function(filename, session, logs, quietly = FALSE){
  ff <- basename(filename)
  f_ses <- strsplit(ff, "_")[[1]][2]
  f_ses_num <- as.numeric(gsub("ses-", "", f_ses))
  
  if(f_ses_num != session){
    msg <- "Session in file name does not correspond to Subject_Timepoint. 
    Updating file name to reflect correct session."
    log_it(msg, "warn", logs, quietly = quietly)
    
    session_string <- paste0("ses-", sprintf("%02d", session))
    gsub(f_ses, session_string, filename)
  }
  
}

check_tilde_paths <- function(string){
  if(grepl("~", string)){
    stop("Don't use tilde ('~') in path names, please provide the full path")
  }
}

# Etxt checks ----
check_duplicates <- function(ff_df, task, logs, paths, quietly = FALSE){
  dups <- dplyr::filter(ff_df, duplicated(files_orig))
  dups <- dplyr::filter(ff_df, files_orig %in% dups$files_orig)
  
  if(nrow(dups) != 0){
    log_it(paste("Found duplicates in file names. Check these for mispunched IDs or session.\nThese are logged in",
                 logs$error),
           type = "error", logs = c(logs$session), quietly = quietly)
    
    update_status(logs$status,
                  files_orig %in% dups$files_orig, 
                  "error", 
                  "duplicate file found")
    
    log_it(paste0("Starting to copy duplicated files from ", paths$raw_etxt, " to ", paths$error),
           logs = c(logs$session, logs$error), quietly = quietly)
    
    for(i in 1:nrow(dups)){
      
      log_file <- gsub("FILE", dups$files_date_time[i], logs$file )
      
      msg <- paste0(dups$files_date_time[i], 
                    ": **Initial error**: this file has a duplicate name-sake. Copying to error folder.\n")
      log_it(string = msg, type="error", logs = c(logs["error"], log_file), quietly = quietly)
      
      in_file <- paste0(paths$raw_etxt,"/", dups$files_date_time[i], ".txt")
      out_file <- gsub(paths$raw_etxt, paths$error, in_file)
      
      copy_file(in_file, out_file, paths, logs, log_file, quietly = quietly)
      
      edat_find(paths$raw_edat, dups$files_date_time[i], 
                paths, logs, log_file, quietly)
    }
  }
  
  filter(ff_df, !files_orig %in% dups$files_orig)
}

check_nlines <- function(ff_df, task, logs, paths, quietly = FALSE){
  
  update_status(logs$status, 
                nlines < eprime_tasks[[task]]$nlines,
                "fatal", "too few lines")
  
  tt <- filter(ff_df, nlines < eprime_tasks[[task]]$nlines)
  
  if(nrow(tt)>0){
    msg <- paste0(Sys.Date()," - Some data has too few lines to process, tagging them as fatal and removing from processing stream." )
    log_it(msg, type="error", logs = logs[c("session", "fatal")], quietly = quietly)
    
    for(i in 1:nrow(tt)){
      log_file <- gsub("FILE", tt$files_date_time[i], logs$file )
      
      msg <- paste0(tt$files_date_time[i], ": **Fatal error** - this file has too few lines to process (",
                    tt$nlines[i]," of ", eprime_tasks[[task]]$nlines, " minimum). Moving to fatal folder.\n")
      log_it(msg, type="error", logs = c(logs["fatal"], log_file), quietly = quietly)
      
      in_file <- paste0(paths$raw_etxt,"/", tt$files_date_time[i], ".txt")
      out_file <- gsub(paths$raw_etxt, paths$fatal, in_file)
      
      copy_file(in_file, out_file, paths, logs, log_file, quietly)
      
      edat_find(paths$raw_edat, tt$files_date_time[i], 
                paths, logs, log_file, quietly)
    }
  }
  
  filter(ff_df, nlines >= eprime_tasks[[task]]$nlines)
}

check_existing <- function(ff_df, logs, quietly = FALSE){
  if(file.exists(logs$status)){
    in_process <- suppressMessages(readr::read_tsv(logs$status))
    
    ff_df <- dplyr::filter(ff_df, 
                           !files_orig_path %in% in_process$files_orig_path) 
    
    if(nrow(ff_df) == 0){
      log_it(paste("There are no new files to process. Exiting process.\n"),
             type = "ok", logs = list(logs$session),
             quietly = quietly)
      stop_quietly()
    }else{
      log_it(paste0("Found **", nrow(ff_df), "** new files to process."),
             type = "message", logs = list(logs$session),
             quietly = quietly)
    }
  }else{
    log_it(paste0("Found **", nrow(ff_df), "** files to process for initial processing."),
           type = "message", logs = list(logs$session),
           quietly = quietly)
  }
  ff_df
}

# Edat checks ----
check_edat <- function(edat_file, orig_file, efile, logs, quietly = FALSE){
  
  if (length(edat_file) == 1) {
    msg = paste("edat companion file found:", basename(edat_file))
    edat_same = T
    
  }else if(length(edat_file) >1){
    msg = paste0("found multiple edat file companions to ", orig_file)
    edat_same = F
    
  }else{
    msg = paste0("cannot find edat file companion to ", orig_file)
    edat_same = F
  }
  
  log_it(msg, type=ifelse(edat_same,"ok","error"), 
         logs=list(logs$session,
                   gsub("FILE", efile, logs$file)),
         quietly = quietly
  )
  
  edat_same
}


# MOAS checks ----

check_MOAS_subj <- function(MOAS, subject){
  if (as.integer(subject) < 1000000) {
    MOAS %>% 
      dplyr::filter(Project_Wave_ID == subject)  
  } else {
    MOAS %>% 
      dplyr::filter(CrossProject_ID == as.numeric(subject))  
  }
}

check_MOAS_date <- function(MOAS, date){
  MOAS %>% 
    dplyr::filter(Test_Date == date)
}

# Manual checks ---
check_manual_moas <- function(.choice, opts, efile_orig, quietly = FALSE ){
  
  if(!quietly){
    print(opts)
  }
  
  nchoices <- c(opts$selection, "None, do nothing")
  choice <- choose_option(.choice, choices = nchoices, 
                          title = paste0("Which option corresponds to the data entry:\n",
                                         efile_orig))
  
  # If chosen one of the data options
  if(choice != length(nchoices) & choice != 0){
    return(list(
      new_id = opts$CrossProject_ID[choice],
      new_ses = opts$Subject_Timepoint[choice]
    ))
  }else{
    return(invisible())
  }
}

check_manual_calendar <- function(.choice, opts, efile_orig, MOAS, logs, quietly = FALSE ){
  
  if(!quietly){
    print(opts)
  }
  
  nchoices <- c(opts$event, "None, do nothing")
  choice <- choose_option(.choice, choices = nchoices, 
                          title = paste0("Which option corresponds to the data entry:\n",
                                         efile_orig))
  
  if(choice != length(nchoices) & choice != 0){
    new_id <- readline("Because calendar entries have varying text, please confirm the ID:")  
    
    while(!new_id %in% strsplit(opts$event[choice], "[[:punct:]]|[[:space:]]")[[1]]){
      note(opts$event[choice])
      new_id <- readline("Typed ID is not found in the calendar event, please confirm the ID:")
    }
    
    alts <- dplyr::filter(MOAS, CrossProject_ID %in% new_id)
    
    if(!quietly){
      print(alts)
    }
    
    nalts <- c(alts$Subject_Timepoint, "None, do nothing")
    new_ses <- choose_option(.choice, choices = nalts, 
                             title = paste0("The calendar has no Subject_Timepoint information, please match it to the above:",
                                            efile_orig))
    
    if(new_ses == length(nalts)){
      msg <- "You chose no session to match, assuming you cannot match data. Exiting manual matching for this file"
      log_it(msg, type = "error", logs = c(logs$session, file_log_orig))
      stop_quietly()
    }
    
    return(list(
      new_id = new_id,
      new_ses = new_ses
    ))
  }else{
    return(invisible())
  }
}


# GCal checks ----

check_calendar <- function(date = NULL,
                           userName = "athanasiamo@gmail.com"){
  
  if(is.null(date)){
    stop("A date is required to use this function. Please provide one.")
  }
  
  # Make sure date is in correct format
  date = lubridate::parse_date_time(date, c("dmY", "Ymd", "mdY"))
  
  gcalendr::calendar_events("80opa8h5fncd86moj3d4ohpke4@group.calendar.google.com", 
                            days_in_past = as.numeric(difftime(lubridate::today(), date)), 
                            days_in_future = 0) %>% 
    dplyr::filter(start_date == date) %>% 
    dplyr::select(summary, start_date, html_link, ical_uid) %>% 
    dplyr::rename(event = summary) %>% 
    dplyr::as_tibble()
}



if(getRversion() >= "2.15.1")  utils::globalVariables(c("start_date", "html_link",
                                                        "ical_uid", "files_orig", "files_orig_path", 
                                                        "nlines", "eprime_update_status",
                                                        "file_log_orig"))
