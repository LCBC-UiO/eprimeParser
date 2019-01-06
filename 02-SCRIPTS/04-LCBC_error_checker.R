#================================================#
# Project: LCBC error checker
# Purpose: run interactive program to check and correct processing errors outputted from running LCBC_eprime_parser.R
# if correct Sub is identified by user will generate .csv per Sub
# 
# Date: 15.10.18
# Author: James Michael Roe
#================================================#

rm(list=ls())

#====================== inputs to change
task = "n-back"
base = "/Volumes/ENCRYPTED"
path = file.path(base,task)
#path = "H:/n-back"
setwd(path)
#====================== inputs to change

#---load packages
list.of.packages = c("tidyverse", "rprime","devtools","lubridate")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
#google calendar
#devtools::install_github("jdeboer/gcalendar")
library("tidyverse", quietly = T)
library("rprime", quietly = T)
library("lubridate", quietly = T)
#library(gcalendar, quietly = T)
#---


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



#======= Set STREAM =======
cat("\nThis script will run a program to check/correct exisiting processing errors in *task*/ERROR-check\n")

cat("\nNB! checking requires a manual check of the TIMES and ID's in Google Calendar on the DATES the program requests\n") 

cat("\nReady to perform checks in INTERACTIVE mode?")

if (interactive()) {
  whichmethod = readline(prompt = cat("\n\nEnter 1 for YES\nEnter 2 for NO\nEnter 'i' for more info\nWaiting for input : \n"))
} else {
  cat("\n\nEnter 1 for YES\nEnter 2 for NO\nEnter 'i' for more info\nWaiting for input : \n")
  whichmethod = readLines("stdin", n=1)
}

if (whichmethod == 1) {
  cat("\n...make sure you are signed in to google...\n\n")
  cat("\n...enter 'q' to quit anytime...\n\n")
} else if (whichmethod == 2) {
  stop("Quitting\n\n")
} else if ( (whichmethod == "i") || (whichmethod == "I") ) {

  cat("\nINTERACTIVE: Program will cycle through ERRORCHECK/ (dirs),
      open google calendar to allow manual check of TIMES,
      and wait for user input to confirm TRUE SUBJECT (Xproj_ID)\n")
  
  # cat("\nNON-INTERACTIVE: Program assumes manual checks of google calendar TIMES have already been done,
  #     and TRUE SUBJECT (Xproj_ID) exists in 'SUBJECT.csv' in each sub directory.")


  if (interactive()) {
    continue = readline(prompt = cat("\n\ncontinue? (y/n)\n"))
  } else {
    cat("\n\ncontinue? (y/n)\n")
    continue = readLines("stdin", n=1)
  }

  if ( (continue != "y") & (continue != "Y") ) {
    stop("Quitting\n\n")
  }

} else {
  cat("\ndetected input not valid. ")
  stop("Quitting\n\n")
}

#pick up where left off
if (interactive()) {
  startpoint = readline(prompt = cat("\n\nEnter number of dirs to skip? (0 to start sequentially from 1): \n"))
} else {
  cat("\n\nEnter number of dirs to skip? (0 to start sequentially from 1): \n")
  startpoint = readLines("stdin", n=1)
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



#======= STREAM: ERRORCHECK =======
if (file.exists(errorlog)) {
  errorlines = readLines(errorlog)
} else {
  cat(paste("ERROR: ", errorlog, "does not exist. Check this. Quitting"))
}

checkdirs = list.files(path_error)
#checkdirs = rev(checkdirs) #reverse inputs
#checkdirs = sample(checkdirs) #randomise inputs
nfixes = 0
end = length(checkdirs)


for (i in 1:end) {
  
  if (as.numeric(startpoint) > 1) {
    c = i+(as.numeric(startpoint)-1)
    checkdir = checkdirs[c]
  } else {
    checkdir = checkdirs[i]  
  }
  
  efile = list.files(paste(path_error, checkdir, sep="/"), pattern = "\\.txt$") #will fail if more than one found
  
  if (as.numeric(startpoint) > 1) {
    msg_start = paste0("\n\n^^^^^^^^^^^^^^ Checking processing error ", c,"/",end," : ",efile,"...")
  } else {
    msg_start = paste0("\n\n^^^^^^^^^^^^^^ Checking processing error ", i,"/",end," : ",efile,"...")
  }
  cat(msg_start)
  cat(msg_start, file = sessionlog, append = T, sep = "\n")
  
  
  #======= Xcheck etxt info =======
  path_error_full = paste(path_error, checkdir, sep="/")
  
  if (!length(paste(path_error_full, efile, sep="/"))) {
    msg = paste("ERROR:", efile, "does not exist in", path_error_full, "\nre-PROCESS. Quitting")
    cat("\n", msg)
    cat(msg, file = sessionlog, append = T, sep = "\n")
    stop("Quitting")
    
  } else {
    
    fileread = paste(path_error_full, "INFO-efile.csv", sep="/")
    
    if (file.exists(fileread)) {
      
      info = readLines(fileread)
      subcheck = strsplit(info," ; ")[[1]][1]
      datecheck = strsplit(info," ; ")[[1]][2]
      timecheck = strsplit(info," ; ")[[1]][3]
    
    } else {
      
      fileread = paste(path_error_full, efile, sep="/")
      filestamp = readfile_fcn(fileread)
      subcheck = filestamp[[1]]
      datecheck = filestamp[[2]]
      timecheck = filestamp[[3]]
    }
  }
  
  
  
  #rm special characters for grep
  allowed = c("_","-"," ",".")
  efile_str = gsub(paste0(".*?($|'|", paste(paste0("\\", allowed), collapse = "|"), "|[^[:punct:]]).*?"), "\\1", efile)
  efile_str = gsub(" ","_", efile_str)
  
  
  #---lifecycle logs
  lifecyclelog = paste(path_lifecyclelogs, paste0(substr(efile_str, 1, nchar(efile) -4), "_", gsub("-","",datecheck), "_", 
                                                  gsub(":","",timecheck), ".log"), sep = "/")
  
  if (!file.exists(lifecyclelog)) {
    msg = paste("ERROR: ", lifecyclelog, "does not exist. Check this. Quitting")
    cat("\n", msg)
    cat(msg, file = sessionlog, append = T, sep = "\n")
    stop("Quitting")
  }
  
  
  #---read options
  cat("\n\nfile =", efile)
  cat("\npotential matches based on date:\n\n")
  checkMOASdate = MOAS %>% filter(Test_Date == as.Date(datecheck))
  
  print(checkMOASdate)
  cat("\n\nfile was created at ***", timecheck, "*** on", datecheck,"... ")

  IDcheck = read.csv(paste(path_error_full,"ID-check.csv", sep ="/"))
  cat("\nsubjects with this ID tested on:\n\n")
  print(IDcheck)
  
  #---open cal
  googleURL = paste("https://calendar.google.com/calendar/r/day",
                    year(datecheck),
                    month(datecheck),
                    day(datecheck), 
                    sep="/")
  
    
  if (interactive()) {
    tmpanswer = readline(prompt = cat("\n\nopen google calendar? (y/n) "))
  } else {
    cat("\n\nopen google calendar? (y/n) ")
    tmpanswer = readLines("stdin", n=1)
  }
    
  if ( (tmpanswer == "y") || (tmpanswer == "Y") ) {
    browseURL(googleURL, browser = getOption("browser"))
  } else if ( (tmpanswer == "q") || (tmpanswer == "Q") ) {
    stop("Quitting\n")
  }
    
  #---enter ID
  if (interactive()) {
    identified = readline(prompt = cat("can you confirm correct Xproj_ID (y/n) "))
  } else {
    cat("can you confirm correct Xproj_ID (y/n) ")
    identified = readLines("stdin", n=1)
  }
    
    
  if ( (identified == "y") || (identified == "Y") ) {
    
    if (interactive()) {
      XSub1 = readline(prompt = cat("Enter correct Xproj_ID: "))
    } else {
      cat("Enter correct Xproj_ID: ")
      XSub1 = readLines("stdin", n=1)
    }
    
    if ( (XSub1 == "q") || (XSub1 == "Q") ) {
      stop("Quitting\n")
    }
    
    if (interactive()) {
      XSub2 = readline(prompt = cat("Confirm Xproj_ID: "))
    } else {
      cat("Confirm Xproj_ID: ")
      XSub2 = readLines("stdin", n=1)
    }
    
    if ( (XSub2 == "q") || (XSub2 == "Q") ) {
      stop("Quitting\n")
    }
    
    if (XSub1 == XSub2) {
      XSub = XSub1
    } else {
      cat("\nWARNING: inputted ID's do not match. Take better care in future")
      stop("Quitting\n")
    }
    
    options = checkMOASdate$CrossProject_ID
    expectedinput = grep(XSub1, options)
    checkMOAS = MOAS %>% filter(CrossProject_ID == XSub)
    waveoptions = checkMOAS$Project_Wave_ID
    
    if (nrow(checkMOAS) == 0) {
      cat("ERROR:", XSub, "not found in MOAS")
      stop("Quitting\n")
    }
    
    #---if unexpected Xsub input
    if (!length(expectedinput)) {
      cat(XSub, "is not logged as being tested on that day. This means the Test_Date is likely wrong in MOAS...\n")
      
      if (interactive()) {
        continue = readline(prompt = cat(paste0("continue with ",XSub,"? (y/n)")))
      } else {
        cat(paste0("continue with ",XSub,"? (y/n)"))
        continue = readLines("stdin", n=1)
      }
      
      if ( (continue == "y") || (continue == "Y") ) {
        
        # cat("searching MOAS for", XSub,"\n")
        # print(checkMOAS)
        # 
        
        if (interactive()) {
          tmpanswer = readline(prompt = cat("confirm that", XSub, "was actually tested on", datecheck, "(y/n) "))
        } else {
          cat("confirm that", XSub, "was actually tested on", datecheck, "(y/n) ")
          tmpanswer = readLines("stdin", n=1)
        }
        
        if ( (tmpanswer == "y") || (tmpanswer == "Y") ) {
          
          datefix = 1
          
          if (interactive()) {
            tmpwaveID = readline(prompt = cat("valid options are:", unique(waveoptions), "\nconfirm Project_Wave_ID: "))
          } else {
            cat("valid options are:", unique(waveoptions), "\nconfirm Project_Wave_ID: ")
            tmpwaveID = readLines("stdin", n=1)
          }
          
          waveID = tmpwaveID
          expectedinput = grep(waveID, waveoptions)
          
          if (!length(expectedinput)) {
            cat("ERROR:", waveID, "not a valid option. Skipping to next...\n\n")
            fix = integer(0)
          } else {
            fix = 1
            datefix = 1
          }
          
        } else if ( (tmpanswer == "q") || (tmpanswer == "Q") ) {
          stop("Quitting\n")
          
        } else {
          cat("not confirmed. Skipping to next...")
          fix = integer(0)
        }
        
        
      } else if ( (tmpanswer == "q") || (tmpanswer == "Q") ) {
        stop("Quitting\n")
        
      } else {
        cat("\nSkipping to next...\n\n")
        fix = integer(0)
      }
      
      
    } else {
      fix = 1
      datefix = integer(0)
    } 
    
  } else if ( (identified == "n") || (identified == "N") ) {
    cat("\nSkipping to next...\n\n")
    fix = integer(0)
    
  } else if ( (identified == "q") || (identified == "Q") ) {
    stop("Quitting\n")
    
  } else {
    cat("\nDetected input not valid. Skipping to next...\n\n")
    fix = integer(0)
  }
  
  
  
  #======= Fix Subject =======
  if (length(fix)) {
    
    cat(msg_start, file = lifecyclelog, append = T, sep = "\n")
    
    #---check dir contents before removing
    if (!file.exists(paste(path_error_full, efile, sep="/"))) {
      msg = paste("ERROR:", efile, "does not exist in", path_error_full, "\nre-PROCESS. Quitting")
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      stop("Quitting")
      
      removedir = integer(0)
    } else {
      removedir = 1
    }
    
    datafile = list.files(paste(path_error_full, sep="/"), pattern = "*PRECLEAN-data\\.csv$")
    
    if (!length(datafile)) {
      msg = paste("ERROR: **PRECLEAN-data.csv does not exist in", path_error_full, "\nre-PROCESS. Quitting")
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      stop("Quitting")
      
      removedir = integer(0)
    } else {
      removedir = 1
    }
    
    
    #---check outputted efile not already in system
    outfile = paste0(XSub, "_", gsub("-","",datecheck), "_", gsub(":","",timecheck), "_", task, ".csv")
    destfile = paste(outpath, outfile, sep="/")
    
    if (file.exists(destfile)) {
      msg = paste(destfile,"exists. Check this. Quitting")
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      stop("Quitting")
      
      removedir = integer(0)
    } else {
      removedir = 1
    }
    
    edatstr = list.files(path_error_full, pattern = "\\.edat*")
    
    if (length(edatstr)) {
      
      edatdestfile = paste(outpath_edat, edatstr, sep="/")
      
      if (file.exists(edatdestfile)) {
        msg = paste(edatdestfile,"exists. Check this. Quitting")
        cat("\n", msg)
        cat(msg, file = sessionlog, append = T, sep = "\n")
        cat(msg, file = lifecyclelog, append = T, sep = "\n")
        stop("Quitting")
      }
      
      edatfound = 1
    } else {
      edatfound = integer(0)
    }
    
    
    
    if (length(removedir)) {
      
      msg = paste("** MATCH found **\nFIXING Subject ID:", XSub )
      cat("\n", msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      
      nfixes = nfixes + 1
      
      subcsv = read.csv(paste(path_error_full, datafile, sep = "/"), 
                        header=T, 
                        sep=",", 
                        na.strings="#N/A", 
                        stringsAsFactors = F)
      
      
      #---subcsv out
      if (!length(datefix)) {
      
        tmp = MOAS %>% filter(CrossProject_ID == XSub & Test_Date == datecheck)
        waveID = as.character(tmp$Project_Wave_ID %>% head(1))
        
      }
      
      subcsv = add_column(subcsv, Subject = waveID, .before = "Subject_PRECLEAN")
      subcsv = add_column(subcsv, CrossProject_ID = XSub, .before = "Experiment")
    
      
      msg = paste("writing corrected data to", destfile)
      cat("\n",msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      
      write.csv(subcsv, file = destfile, row.names = F)
      
      
      #---etxt out
      msg = paste("moving", efile, "to", outpath_etxt)
      cat("\n",msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      
      file.rename(paste(path_error_full, efile, sep="/"), paste(outpath_etxt, efile, sep="/"))
      
      
      #---edat out  
      if (length(edatfound)) {
        msg = paste("moving", edatstr, "to", outpath_edat)
        cat("\n",msg)
        cat(msg, file = sessionlog, append = T, sep = "\n")
        cat(msg, file = lifecyclelog, append = T, sep = "\n")
        
        file.rename(paste(path_error_full, edatstr, sep="/"), paste(outpath_edat, edatstr, sep="/"))
      }
      
      #---update logs
      msg = "updating logs..."
      cat("\n",msg)
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      
      SEARCHKEY = paste(efile_str, datecheck, timecheck, sep = " ; ")
      if (length(edatfound)) {
        msg = paste(SEARCHKEY, outfile, edatstr, sep=" ; ")  
      } else {
        msg = paste(SEARCHKEY, outfile, sep=" ; ")  
      }
      
      cat(msg, file = donelog, append = T, sep = "\n")
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      
      whichline = paste(SEARCHKEY, checkdir, sep = " ; " )
      whichline = grep(whichline, errorlines)
      writeLines(errorlines[-whichline], errorlog)
      
      
      # msg = "updating ALL_FILES.log"
      # cat("\n", msg)
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
      
      
      
      #---remove matchdir
      msg = paste("***** removing directory", path_error_full, "***** FIX COMPLETE")
      cat("\n",msg,"\n")
      cat(msg, file = sessionlog, append = T, sep = "\n")
      cat(msg, file = lifecyclelog, append = T, sep = "\n")
      unlink(path_error_full, recursive = T)
      
    }
  }
  
  # if (interactive()) {
  #   tmpanswer = readline(prompt = cat("continue to next? (y/n) "))
  # } else {
  #   cat("continue to next? (y/n) ")
  #   tmpanswer = readLines("stdin", n=1)
  # }
  
  # if ( (tmpanswer == "n") & (tmpanswer == "N") ) {
  #   cat("\nSubject ID's fixed =", nfixes, "\nStopping.\n")
  #   stop("Quitting")
  # }
}
