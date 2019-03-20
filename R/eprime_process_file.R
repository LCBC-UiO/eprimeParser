
#' Process an E-prime file for LCBC
#' 
#' Runs the processing of E-prime files for LCBC. 
#' This will check the existance of files, any
#' previous logs for duplicated entries etc.
#' 
#' @param efile path to E-prime txt file
#'
#' @param paths list of paths from [\code{eprime_setup_dirs}]
#' @param logs list of paths from [\code{eprime_setup_logs}]
#' @param errorlines errorlines from [\code{eprime_setup_logs}]
#' @param loglines loglines from [\code{eprime_setup_logs}]
#' @inheritParams eprime_parser
#'
#' @importFrom dplyr mutate select arrange desc filter
#' @importFrom rprime read_eprime FrameList keep_levels to_data_frame drop_levels
#' @importFrom utils head tail write.csv
#' @importFrom magrittr '%>%'
#' @export
eprime_process_file <- function(efile, MOAS, task, paths, logs,
                                errorlines, loglines){
  
  efile_path <- paste(paths$path_etxt, efile, sep="/")
  
  msg <- paste0("\n\n############## Checking ", efile,"...")
  log_it(msg, type = "message", logs = list(logs$sessionlog))
  
  # ======= check duplicate: Xcheck etxt info =======
  etxt_info = etxt_fcn(efile_path)
  
  # rm special characters for grep
  allowed <- c("_","-"," ",".")
  efile_str <- gsub(paste0(".*?($|'|", 
                           paste(paste0("\\", allowed), collapse = "|"),
                           "|[^[:punct:]]).*?"), "\\1", efile)
  efile_str <- gsub(" ","_", efile_str)
  
  #---lifecycle logs
  lifecyclelog = paste(paths$path_lifecyclelogs, 
                       paste0(substr(efile_str, 1, nchar(efile) -4), "_", 
                              gsub("-","",etxt_info$datecheck), "_", 
                              gsub(":","",etxt_info$timecheck), ".log"), 
                       sep = "/")
  if (!file.exists(lifecyclelog))  file.create(lifecyclelog)
  
  #======= check duplicate: logs =======
  #--logchecks
  SEARCHKEY <- paste(efile_str, etxt_info$datecheck, etxt_info$timecheck, sep = " ; ")
  msg <- paste("SEARCHKEY =", SEARCHKEY);
  log_it(msg, type = "message", logs = list(logs$sessionlog))
  
  # Check if there is reason to skip processing the file
  skip <- eprime_skip_check(efile, SEARCHKEY, loglines, errorlines, 
                            etxt_info, logs, MOAS, paths)
  
  
  ####################### PROCESS IF NOT DUPLICATE ######################
  if (!skip) {
    
    msg = "processing..."
    log_it(msg, type = "message", logs = list(logs$sessionlog,
                                              lifecyclelog))
    
    #edat file only made on completion
    
    #====================== check 1: edat ======================   
    msg = paste("## Check #1: checking if .edat exists for", efile)
    log_it(msg, type = "message", logs = list(logs$sessionlog,
                                              lifecyclelog))
    
    edat_info = edat_fcn(paths$path_edat, efile)
    
    edat_same <- edat_check(edat_info, paths$path_edat, efile,
                            logs$sessionlog, lifecyclelog)
    
    sub <- edat_clean(efile_path, task, efile)
    
    #---rearrange date to fit MOAS
    #sub$subcsv$SessionDate = datecheck
    
    
    #====================== check 2: MOAS ====================== 
    msg = paste0("## Check #2: checking if subID (", sub$subPC, 
                ") matches with Project_Wave_ID/CrossProject_ID and Test_Date in MOAS")
    log_it(msg, type = "message", logs = list(logs$sessionlog,
                                              lifecyclelog))
    
    #---Xref ID and date with MOAS
    if (as.integer(sub$subPC) < 1000000) {
      
      #---deal with leading zeros
      sub$subPC <- fix_ids(sub$subPC)
      
      inMOAS = MOAS %>% 
        dplyr::filter(Project_Wave_ID == sub$subPC,
                      Test_Date == etxt_info$datecheck) 
      
    } else {
      
      inMOAS = MOAS %>% 
        dplyr::filter(CrossProject_ID == as.numeric(sub$subPC),
                      Test_Date == etxt_info$datecheck) 
    }
    
    inMOAS <- inMOAS %>% 
      dplyr::select(CrossProject_ID) %>% 
      unique() %>% 
      unlist()
    
    if (!is_empty(inMOAS)) {
      
      #====================== X check success ====================== 
      msg = paste0("TRUE MATCH : ", inMOAS)
      log_it(msg, type = "message", logs = list(logs$sessionlog,
                                                lifecyclelog))
      
      #---etxt out
      destfile = paste(paths$outpath_etxt, efile, sep="/")
      rename_destfile(destfile, efile, paths, logs, lifecyclelog)
      
      #---sub$subcsv out
      sub$subcsv = sub$subcsv %>% 
        dplyr::mutate(Subject = Subject_PRECLEAN,
                      CrossProject_ID = inMOAS)
      
      outfile = paste0(inMOAS, "_", 
                       gsub("-","",etxt_info$datecheck), "_", 
                       gsub(":","",etxt_info$timecheck), "_", 
                       task, ".csv")
      
      destfile = paste(paths$outpath, outfile, sep="/")
      move_destfile(sub$subcsv, destfile, logs, lifecyclelog)
      

      if (edat_same) {
        
        edat_move(edat_info, outfile, efile, 
                  paths, logs, lifecyclelog, SEARCHKEY)
        
      } else {
        msg = paste(SEARCHKEY, outfile, sep=" ; ")
        log_it(msg, type = "message", logs = list(logs$sessionlog,
                                                  lifecyclelog,
                                                  logs$donelog))
      }
      
      #---update logs
      # msg = paste("updating ALL_FILES.log:", outfile)
      # cat("\n",msg)
      # cat(msg, file = sessionlog, append = T, sep = "\n")
      # cat(msg, file = lifecyclelog, append = T, sep = "\n")
      # 
      # ALLlines = readLines(filelog)
      # whichline = grep(SEARCHKEY, ALLlines)
      # line = ALLlines[whichline]
      # 
      # msg = paste0(line, " ; ", outfile)
      # newloglines = ALLlines
      # newloglines[whichline] = msg
      # #--overwrite
      # writeLines(newloglines, filelog)
      
    } else if (is_empty(inMOAS)) {
      
      
      #====================== X check fail ====================== 
      msg = "NOMATCH: subID and Test Date do not correspond with MOAS\nChecking MOAS for possible subID's based on Test_Date"
      log_it(msg, type = "message", logs = list(logs$sessionlog,
                                                lifecyclelog))
      
      #---Get ID and date alternatives from MOAS
      checkMOAS = if (as.integer(sub$subPC) < 1000000) {
        MOAS %>% 
          dplyr::filter(Project_Wave_ID == sub$subPC)  
      } else {
        MOAS %>% 
          dplyr::filter(CrossProject_ID == as.numeric(sub$subPC))  
      }
      
      msg = paste("MOAS Test_Date may be wrong: Checking for ID's tested on", etxt_info$datecheck)
      log_it(msg, type = "message", logs = list(logs$sessionlog,
                                                lifecyclelog))
      
      checkMOASdate = MOAS %>% 
        dplyr::filter(Test_Date == etxt_info$datecheck)
      
      
      #---create errordir to check alternatives
      errordir = gsub(task, "err", efile) %>% 
        gsub("\\.txt", "", .) %>% 
        paste(paths$path_error, ., sep="/")
      
      if(!dir.exists(errordir))  dir.create(errordir)
      
      
      #cat("writing error files")
      msg = paste0("Writing Date and subID alternatives to ", errordir,
                  "\nMoving ", efile, " to ", errordir)
      log_it(msg, type = "message", logs = list(logs$sessionlog,
                                                lifecyclelog))
      
      file.rename(paste(paths$path_etxt, efile, sep="/"), paste(errordir, efile, sep ="/"))
      
      if(edat_same) {
        
        msg = paste("moving", edat_info, "to", errordir)
        log_it(msg, type = "message", logs = list(logs$sessionlog,
                                                  lifecyclelog))
        
        file.rename(paste(paths$path_edat, edat_info, sep="/"), paste(errordir, edat_info, sep ="/"))
      }
    }
  }
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("CrossProject_ID", 
                                                        "Test_Date",
                                                        "Project_Wave_ID", 
                                                        "timecheck", 
                                                        "Subject_PRECLEAN",
                                                        "."))

