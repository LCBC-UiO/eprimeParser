#================================================#
# Project: LCBC eprime parser
# Purpose: extract etxt/edat files and Xref with MOAS to generate .csv per Sub
# outputs potential ID's if Xref does not match based on test date
# 
# Date: 15.10.18
# Author: James Michael Roe
#================================================#

rm(list=ls())

#====================== inputs to change
task = "Attention"
base = "/Volumes/ENCRYPTED"
path = file.path(base,task)
#path = "H:/n-back"
setwd(path)
#====================== inputs to change

#---load packages
list.of.packages = c("tidyverse", "rprime","devtools","lubridate","tictoc")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
#google calendar
#devtools::install_github("jdeboer/gcalendar")
library("tidyverse", quietly = T)
library("rprime", quietly = T)
library("lubridate", quietly = T)
library("tictoc", quietly = T)
#library(gcalendar, quietly = T)
#---

tic()

#---set dir struct (follows directory structure in README.txt)
dirstruct = c("1-ID_dataframes","2-COMPLETE_etxt","3-COMPLETE_edat","4-ERROR_check",
              "5-COPY_etxt","6-COPY_edat","7-FATAL","8-LOGS")


for (d in dirstruct) {
  if (!dir.exists(d)) {
    cat(paste("\n\ncreating", d),sep = "\n")
    dir.create(d)
  }
}

#---paths
outpath = paste(path, dirstruct[1],sep="/")
outpath_etxt = paste(path, dirstruct[2],sep="/")
outpath_edat = paste(path, dirstruct[3],sep="/")
path_error = paste(path,dirstruct[4],sep="/")
path_etxt = paste(path,dirstruct[5],sep="/")
path_edat = paste(path,dirstruct[6],sep="/")
# path_etxt = paste(path,"tmp_copy_etxt",sep="/")
# path_edat = paste(path,"tmp_copy_edat",sep="/")
path_fatal = paste(path,dirstruct[7],sep="/")
#---logs
path_lifecyclelogs = paste(path,dirstruct[8],sep="/")
donelog = paste(path,"COMPLETE.log",sep="/")
errorlog = paste(path,"ERROR-check.log",sep="/")
sessionlog = paste(path,"SESSION.log",sep="/")
filelog = paste(path,"ALL_FILES.log",sep="/")

#---SESSION.log
file.create(sessionlog)

#---COMPLETE.log
if (file.exists(donelog)) {
  loglines = readLines(donelog)
} else {
  file.create(donelog)
  loglines = readLines(donelog)
}
#---ERRORS.log
if (file.exists(errorlog)) {
  errorlines = readLines(errorlog)
} else { 
  file.create(errorlog)
  errorlines = readLines(errorlog)
}


#======= FUNCTIONS =======
#---read efiles
#fileread = paste(path_etxt, efile, sep="/")
readfile_fcn = function(fileread) {
  
  encodetype = readr::guess_encoding(fileread, n_max = 1000)[1,1]
  
  if (encodetype == "ASCII") {
    
    tmpread = readLines(fileread, n = 30)
    tmp1 = tmpread[grep('Subject', tmpread)]
    tmp2 = tmpread[grep('SessionDate', tmpread)]
    tmp3 = tmpread[grep('SessionTime', tmpread)]
    
    tmpstr = strsplit(tmp2,": ")[[1]][2]
    tmpstr = strsplit(tmpstr,"-")
    tmpstr1 = tmpstr[[1]][3]
    tmpstr2 = tmpstr[[1]][1]
    tmpstr3 = tmpstr[[1]][2]
    
    subcheck = strsplit(tmp1,": ")[[1]][2] #sub
    datecheck = paste(tmpstr1,tmpstr2,tmpstr3,sep="-") #date
    timecheck = strsplit(tmp3,": ")[[1]][2] #time
    
    filestamp = list(subcheck,datecheck,timecheck,encodetype)
    return(filestamp)
    
  } else if (encodetype == "UTF-16LE") { 
    
    tmpread = scan(file(fileread, encoding = "UTF-16LE"), what = character(), nlines=30) 
    
    tmpstr = strsplit(tmpread[grep('SessionDate', tmpread) + 1], "-")
    tmpstr1 = tmpstr[[1]][3]
    tmpstr2 = tmpstr[[1]][1]
    tmpstr3 = tmpstr[[1]][2]
    
    subcheck = tmpread[grep('Subject', tmpread) + 1] #sub
    datecheck = paste(tmpstr1,tmpstr2,tmpstr3,sep="-") #date
    timecheck = tmpread[grep('SessionTime', tmpread) + 1] #time
    
    filestamp = list(subcheck,datecheck,timecheck,encodetype)
    return(filestamp)
  }
}


#---search for matching edat
edat_fcn = function(efile) {
  
  edats = list.files(path_edat)
  
  #change extension to .edat and search
  strcut = substr(efile, 1, nchar(efile) -4)
  strcut_edat = paste0(strcut, ".edat")
  regexp = grep(strcut_edat, edats)
  
  #get true edat
  edatstr = edats[regexp]
  edatnum = as.data.frame(regexp)
  
  edat_info = list(strcut,strcut_edat,regexp,edatstr,edatnum)
  return(edat_info)
}


#---load MOAS
if (file.exists(paste(base,"MOAS_safe.RData",sep="/"))) {
  load(paste(base,"MOAS_safe.RData",sep="/"))
  MOAS = MOAS
} else if (file.exists(paste(base,"MOAS.RData",sep="/"))) {
  load(paste(base,"MOAS.RData",sep="/"))
  MOAS = MOAS %>% select(CrossProject_ID,
                         Subject_Timepoint,
                         Project_Wave,
                         Project_Name,
                         Project_Wave_ID,
                         Test_Date,
                         Age,
                         Sex,
                         Birth_Date) %>% distinct()
  save(MOAS,file = paste(base,"MOAS_safe.RData",sep="/"))
  file.remove(paste(base,"MOAS.RData",sep="/"))
  #---remove all rows with NA for Test_Date
  #MOAS = MOAS[!is.na(MOAS$Test_Date),]
}


#======= PROCESS =======
#---read eprime / etxt files in copy dir
etxts = list.files(path_etxt, pattern = "\\.txt$")
edats = list.files(path_edat)
#etxts = rev(etxts) #reverse inputs
etxts = sample(etxts) #randomise inputs
end = length(etxts)

for (i in 1:end) {

  efile = etxts[i]
  
  msg_start = paste0("\n\n############## Checking ",i,"/",end," : ",efile,"...")
  cat(msg_start)
  cat(msg_start, file = sessionlog, append = T, sep = "\n")
  
  
  #======= check duplicate: Xcheck etxt info =======
  fileread = paste(path_etxt, efile, sep="/")
  filestamp = readfile_fcn(fileread)
  subcheck = filestamp[[1]]
  datecheck = filestamp[[2]]
  timecheck = filestamp[[3]]
  
  
  #rm special characters for grep
  allowed = c("_","-"," ",".")
  efile_str = gsub(paste0(".*?($|'|", paste(paste0("\\", allowed), collapse = "|"), "|[^[:punct:]]).*?"), "\\1", efile)
  efile_str = gsub(" ","_", efile_str)
  
  
  
  #======= check duplicate: logs =======
  #--logchecks
  SEARCHKEY = paste(efile_str, datecheck, timecheck, sep = " ; ")
  msg = paste("SEARCHKEY =", SEARCHKEY)
  cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
  
  
  skip_done = grep(SEARCHKEY, loglines)
  skip_error = grep(SEARCHKEY, errorlines)  
  
  
  #---skip if Date too recent for MOAS version
  recent = MOAS %>% select(Test_Date) %>% unique() %>% arrange(desc(Test_Date)) %>% head(5)
  mostrecent = recent[1,1]
  
  if (as.integer(as.Date(datecheck)-mostrecent) > 1) {
    msg = paste0("Date: ", datecheck, " is too recent. Last date logged in MOAS is ", mostrecent,
               ". Update MOAS version to process.\nSkipping")
    cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
    
    skip_recent = 1 
    skip = 1
  } else {
    skip_recent = integer(0)
  }
  
  if ( (!length(skip_done)) && (!length(skip_error)) && (!length(skip_recent)) ) {
    skip = integer(0)
  }
  
  if (length(skip_done)) {
    
    skip = 1
    
    msg = paste0("SEARCHKEY", " found in ", basename(donelog), ": confirming duplicate...")
    cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
    
    #---check file in COMPLETE_etxt/
     if (file.exists(paste(outpath_etxt, efile, sep="/"))) {
      
      filestampcheck = readfile_fcn(paste(outpath_etxt, efile, sep="/"))
      filesub = filestampcheck[[1]] == subcheck
      filedate = filestampcheck[[2]] == datecheck
      filetime = filestampcheck[[3]] == timecheck
      
      
      if (filesub & filedate & filetime == T) {
        
        msg = paste(efile, "EXISTS in", outpath_etxt,"\nConfirmed duplicate: already processed.\nFile will be safely removed from", 
                  path_etxt, "\nSkipping")
        cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
            
        file.remove(paste(path_etxt, efile, sep="/"))
        
      } else {
        msg = paste("ERROR:", efile, "already exists in", outpath_etxt, "but the file is not duplicate. Check this.\nSkipping")
        cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
      }
        
    } else {
        
      msg = paste("ERROR", efile, "not in", outpath_etxt, "but in COMPLETE.log. Check this and copy/preprocess if necessary.\nSkipping")
      cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
    }
  }
  
  
  if (length(skip_error)) {
    
    skip = 1
    
    msg = paste0("SEARCHKEY", " found in ", basename(errorlog), ": confirming duplicate...")
    cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
    
    
    #---Xcheck timedate in errorlog
    errorlines = readLines(errorlog)
    line = errorlines[grep(SEARCHKEY, errorlines)]
    tmpstr = strsplit(line," ; ")
    
    errordir = paste(path_error, tmpstr[[1]][5], sep="/")
    
    
    #---Xcheck timedate in errorlog
    if (file.exists(paste(errordir, efile, sep="/"))) {
      
      fileread = paste(errordir, "INFO-efile.csv", sep="/")
      
      if (file.exists(fileread)) {
        cat("reading in INFO")
        info = readLines(fileread)
        filesub = strsplit(info," ; ")[[1]][1] = subcheck
        filedate = strsplit(info," ; ")[[1]][2] = datecheck
        filetime = strsplit(info," ; ")[[1]][3] = timecheck
      
      } else {
        cat("reading info from file")
        filestampcheck = readfile_fcn(paste(errordir, efile, sep="/"))
        filesub = filestampcheck[[1]] = subcheck
        filedate = filestampcheck[[2]] = datecheck
        filetime = filestampcheck[[3]] = timecheck
      }
      
      if (filesub & filedate & filetime == T) {
        
        msg = paste(efile, "EXISTS in", errordir,"\nConfirmed duplicate: under error handling.\nFile will be safely removed from",
                   path_etxt, "\nSkipping")
        cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
        
        file.remove(paste(path_etxt, efile, sep="/"))
      }
      
    } else {

      msg = paste("WARNING: SEARCHKEY", "found in", basename(errordir),"but file not in", errordir)
      cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
      msg = paste("Moving", efile, "to", errordir, "\nWill not process. Skipping")
      cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
      
      file.rename(paste(path_etxt, efile, sep="/"), paste(errordir, efile, sep="/")) 
      
      
      edat_info = edat_fcn(efile)
      
      if (length(edat_info[[4]])) { 
        msg = paste("Moving", edat_info[[4]], "to", errordir, "\nWill not process. Skipping")
        cat("\n", msg); cat(msg, file = sessionlog, append = T, sep = "\n")
        
        file.rename(paste(path_edat, edat_info[[4]], sep="/"), paste(errordir, edat_info[[4]], sep="/")) 
      }
        
    }
  }

  
  #---lifecycle logs
  lifecyclelog = paste(path_lifecyclelogs, paste0(substr(efile_str, 1, nchar(efile) -4), "_", gsub("-","",datecheck), "_", 
                                                  gsub(":","",timecheck), ".log"), sep = "/")
  
  if (!file.exists(lifecyclelog)) {
    file.create(lifecyclelog)
  }
  
  
  ####################### PROCESS IF NOT DUPLICATE ######################
  if (!length(skip)) {
    
    msg = "processing..."
    cat("\n", msg)
    cat(msg, file = sessionlog, append = T, sep = "\n")
    cat(msg_start, msg, file = lifecyclelog, append = T, sep = "\n")
    
    #edat file only made on completion
    
    #====================== check 1: edat ======================   
    msg = paste("## Check #1: checking if .edat exists for", efile)
    cat("\n", msg)
    cat(msg, file = sessionlog, append = T, sep = "\n")
    cat(msg, file = lifecyclelog, append = T, sep = "\n")
    
    edat_info = edat_fcn(efile)
    strcut_edat = edat_info[[2]]
    regexp = edat_info[[3]]
    edatstr = edat_info[[4]]
    edatnum = edat_info[[5]]
    
    
    if (length(regexp)) {
      msg = paste("edat file found:", edatstr, sep=" ")
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      
      edat_same = T
      
    } else {
      msg = paste0("cannot find ", strcut_edat, "* in ", path_edat)
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      
      edat_same = F
    }
      
    
    #---load file & parse contents
    exp_lines = read_eprime(paste(path_etxt, efile, sep="/"))
    experiment_data = FrameList(exp_lines)
    #preview_levels(experiment_data)
    #preview_frames(experiment_data)
    
    
    #---extract header
    header = keep_levels(experiment_data, 1)
    header = to_data_frame(header) %>% tail(1)
    header_cols =  c("Experiment","Subject","SessionDate","SessionTime")
    header = header[header_cols]
    
    
    #---remove header
    data = drop_levels(experiment_data, 1)
    #preview_levels(data)
    
    
    #--full data extracted
    df_data = to_data_frame(data)
    
    #---subset df
    if (task == "n-back") {
      data_cols = 
        c("Running",
          "Pic",
          "CR",
          "StimTest.ACC",
          "StimTest.RT",
          "StimTest.RESP",
          "StimTest.CRESP",
          "Stim.ACC",
          "Stim.RT",
          "Stim.RESP",
          "Stim.CRESP")
    } else if (task == "Attention") {
      data_cols = 
        c("Running",
          "WarningType",
          "FlankerType",
          "TargetType1",
          "TargerDirection",
          "Sample",
          "PracSlideTarget.ACC",
          "PracSlideTarget.RT",
          "PracSlideTarget.RESP",
          "PracSlideTarget.CRESP",
          "TargetType",
          "TargetDirection",
          "SlideTarget.ACC",
          "SlideTarget.RT",
          "SlideTarget.RESP",
          "SlideTarget.CRESP")
    } else if (task == "Antisaccade") {
      data_cols = 
        c("Running",
          "Arrow",
          "CR",
          "Eprime.LevelName",
          "Sample",
          "Target1.ACC",
          "Target1.RT",
          "Target1.RESP",
          "Target1.CRESP",
          "TrialList",
          "Target.ACC",
          "Target.RT",
          "Target.RESP",
          "Target.CRESP")
    }  
    
    df_data = df_data[data_cols]
    
    
    if (task == "n-back" || task == "Antisaccade") {
      #---add Block col
      df_data = add_column(df_data, "Block" = as.integer(rownames(df_data)),.before = "Running")
    }
    
    #convert strings to int
    #df_data = readr::type_convert(df_data)
    #str(data)
    
    
    #---create outfile
    subcsv = data.frame(header, df_data) #outputs warning message - OK
    colnames(subcsv)[names(subcsv) == "Subject"] = "Subject_PRECLEAN"
    subcsv = add_column(subcsv, SessionDate_PRECLEAN = subcsv$SessionDate, .after = "SessionDate")
    subcsv = add_column(subcsv, Filename = efile, .before = "SessionDate")
    
    #PC = "PRECLEAN" 
    subPC = as.character(subcsv$Subject %>% head(1))
    
    #---rearrange date to fit MOAS
    subcsv$SessionDate = datecheck

    
    #====================== check 2: MOAS ====================== 
    msg = paste("## Check #2: checking if subID (", subPC, 
              ") matches with Project_Wave_ID/CrossProject_ID and Test_Date in MOAS")
    cat("\n", msg)
    cat(msg, file = sessionlog, append = T, sep = "\n")
    cat(msg, file = lifecyclelog, append = T, sep = "\n")
    
    #---Xref ID and date with MOAS
    if (as.integer(subPC) < 1000000) {
      
      
      #---deal with leading zeros
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
        
        subPC_altered = T
      }
          
      inMOAS = MOAS %>% filter(Project_Wave_ID == subPC & Test_Date == datecheck)  
      
    } else {
      
      inMOAS = MOAS %>% filter(CrossProject_ID == as.numeric(subPC) & Test_Date == datecheck)  
    }
    
    
    if (nrow(inMOAS) == 1) {
      
      #====================== X check success ====================== 
      XSub = inMOAS$CrossProject_ID
      
      msg = paste0("TRUE MATCH : ", XSub)
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")


      #---etxt out
      destfile = paste(outpath_etxt, efile, sep="/")
      
      if (!file.exists(destfile)) {
        
        msg = paste("moving", efile, "to", outpath_etxt)
        cat("\n", msg)
        cat(msg, file = sessionlog, append = T, sep = "\n")
        cat(msg, file = lifecyclelog, append = T, sep = "\n")            
        
        file.rename(paste(path_etxt, efile, sep="/"), destfile) 
      
      } else {
        
        msg = paste("ERROR:", "cannot move", efile, "to", destfile, 
                    "\nFile exists. Check this. Quitting")
        cat("\n", msg)
        cat(msg, file = sessionlog, append = T, sep = "\n")
        cat(msg, file = lifecyclelog, append = T, sep = "\n")
        stop("Quitting")
      }
        # 
      
      #---subcsv out
      subcsv = add_column(subcsv, Subject = subcsv$Subject_PRECLEAN, .before = "Subject_PRECLEAN")
      subcsv = add_column(subcsv, CrossProject_ID = XSub, .before = "Experiment")
      outfile = paste0(XSub, "_", gsub("-","",datecheck), "_", gsub(":","",timecheck), "_", task, ".csv")
      destfile = paste(outpath, outfile, sep="/")
      
      
      if (!file.exists(destfile)) {
        msg = paste("writing data to", destfile)
        cat("\n", msg)
        cat(msg, file = sessionlog, append = T, sep = "\n")
        cat(msg, file = lifecyclelog, append = T, sep = "\n")            
        
        write.csv(subcsv, destfile, row.names = F)
        
      } else {
        
        msg = paste(destfile,"exists: Check this. Quitting")
        cat("\n", msg)
        cat(msg, file = sessionlog, append = T, sep = "\n")
        cat(msg, file = lifecyclelog, append = T, sep = "\n")
        stop("Quitting")
      }
      
    
      if (edat_same == T) {
        
        if (nrow(edatnum) == 1) {
          
          msg = paste("moving", edatstr, "to", outpath_edat)
          cat("\n", msg)
          cat(msg, file = sessionlog, append = T, sep = "\n")
          cat(msg, file = lifecyclelog, append = T, sep = "\n")            
          
          file.rename(paste(path_edat, edatstr, sep="/"), paste(outpath_edat, edatstr ,sep="/")) 
          
          msg = paste(SEARCHKEY, outfile, edatstr, sep=" ; ")
          cat(msg, file = sessionlog, append = T, sep = "\n")
          cat(msg, file = lifecyclelog, append = T, sep = "\n")
          cat(msg, file = donelog, append = T, sep = "\n")
          
        } else if (nrow(edatnum) > 1) { 
          
          msg = paste("\nWARNING: multiple files found (", nrow(edatnum), ") containing name", strcut_edat, 
                    "*. Will not move to", outpath_edat)
          cat("\n", msg)
          cat(msg, file = sessionlog, append = T, sep = "\n")
          cat(msg, file = lifecyclelog, append = T, sep = "\n")            
        }
        
      } else {
        msg = paste(SEARCHKEY, outfile, sep=" ; ")
        cat("\n", msg)
        cat(msg, file = sessionlog, append = T, sep = "\n")
        cat(msg, file = lifecyclelog, append = T, sep = "\n")
        cat(msg, file = donelog, append = T, sep = "\n")
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
      
    } else if (nrow(inMOAS) != 1) {
      
    
      #====================== X check fail ====================== 
      #cat("NOMATCH")
      msg = "NOMATCH: subID and Test Date do not correspond with MOAS"
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")            
      
      msg = "Checking MOAS for possible subID's based on Test_Date"
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")            
      
      
      #---Get ID and date alternatives from MOAS
      if (as.integer(subPC) < 1000000) {
        
        checkMOAS = MOAS %>% filter(Project_Wave_ID == subPC)  
        
      } else {
        
        checkMOAS = MOAS %>% filter(CrossProject_ID == as.numeric(subPC))  
      }
      
    
      msg = paste("MOAS Test_Date may be wrong: Checking for ID's tested on", datecheck)
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")            
      
      
      checkMOASdate = MOAS %>% filter(Test_Date == datecheck)
      
      
      #---create errordir to check alternatives
      tmpstr = strsplit(efile, "-")
      if (task == "n-back") {
        tmpstr = tmpstr[[1]][4]
        strcut = substr(tmpstr, 1, 1)
      } else {
        strcut = tmpstr[[1]][3]
        strcut = substr(strcut, 1, 1)
      }
    
      errordir = paste0(path_error,"/err_",subPC,"-",strcut)
      
      if(!dir.exists(errordir)) {
        dir.create(errordir)
      } 
      
      #cat("writing error files")
      msg = paste("writing Date and subID alternatives to", errordir)
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      
      msg = paste("moving", efile, "to", errordir)
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")            
      
      file.rename(paste(path_etxt, efile, sep="/"), paste(errordir, efile, sep ="/"))
      
      
      if(edat_same == T) {
        
        msg = paste("moving", edatstr, "to", errordir)
        cat("\n", msg)
        cat(msg, file = sessionlog, append = T, sep = "\n")
        cat(msg, file = lifecyclelog, append = T, sep = "\n")            
        
        file.rename(paste(path_edat, edatstr, sep="/"), paste(errordir, edatstr, sep ="/"))
      }
      
      
      #---log error
      errorinfo = paste(SEARCHKEY, basename(errordir), sep = " ; " )
      cat(errorinfo, file = paste(errordir, "INFO-efile.csv", sep = "/"), append = T, sep = "\n")
      
      #---copy session log
      file.copy(lifecyclelog, paste(errordir, "session.log", sep="/"))
      
      #---write alternatives
      write.csv(checkMOASdate, paste(errordir, "DATE-check.csv", sep = "/"))
      write.csv(checkMOAS, paste(errordir, "ID-check.csv", sep = "/"))
      
      #---write PRECLEAN data
      datafile = paste0(subPC, "_PRECLEAN-data.csv")
      write.csv(subcsv, paste(errordir, datafile, sep = "/"), row.names = F)
      
      #---update logs
      msg = paste("updating ERROR-check.log:", basename(errordir))
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      cat(errorinfo, file = errorlog, append = T, sep = "\n")
      
      
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
  }
}
rm(list=ls(pattern="tmp*"))  

toc()
cat("\n\nFINISHED\nsee", sessionlog, "for full output\n\n\n(ignore all 'closing' and 'In data.frame(header, df_data)' warning messages)\n")








#-------------get googlecalendar data-------------
### IDEALLY MATCHES SHOULD BE MADE WITH GOOGLE CALENDAR TIMES AS WELL
### CURRENTLY ONLY READING 250 events - awaiting resolution ###
#https://github.com/jdeboer/gcalendar/issues/4

#devtools::install_github("jdeboer/gcalendar")

# creds <- GoogleApiCreds(
#   userName = "james.roe17@gmail.com", 
#   appCreds = "/Users/jamesroe/Dropbox/Programming/API/client_id.json" # Location of the JSON file containing your
#   # Google APIs project OAuth client ID and
#   # secret. 
# )
# 
# cal_list <- gCalendarLists$new(creds = creds)
# calendars <- cal_list$summary[c("id", "summary", "description")]
# print(calendars)
# 
# testing_calendar <- gCalendar$new(
#   creds = creds,
#   id = "80opa8h5fncd86moj3d4ohpke4@group.calendar.google.com"
# )
# 
# testing_events <- testing_calendar$events
# 
# testing <- testing_events$summary[c("summary", "start", "htmlLink")]
# print(testing)
# 
# date="2013-08-12"
# 
# calmatch = testing[grep(date, testing$start), ]
# out = calmatch[,2:3]
###