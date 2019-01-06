# LCBC Eprime Parser

James M Roe

This Hard Disk should be exclusively used for retrieving data from the computerised Eprime tasks we run here at LCBC.

To automate this process, the directory requires a set structure

The only files and folders here should be:


## --- 01-COPYDIR 
			-- TestRoom1
			-- TestRoom2
			-- Laptop

## --- 02-SCRIPTS
			-- 02-find_eprime_files.sh
			-- 03-LCBC_eprime_parser.R
			-- 04-LCBC_error_checker.R

## --- MOAS.Rdata 
(replace with newest MOAS, will be overwritten by MOAS_safe.Rdata)

## --- README
(Pre-existing)
## --- $task
## --- $task...
(Created by script)



# Steps:

**1) Copy a directory into 01-COPYDIR/$location-copied-from**

- It does not matter how high up the directory is in the tree, so long as it contains a subdirectory at some point containing .edat and .txt files created by Eprime


**2) open 02-find_eprime_files.sh**

- change $task as necessary
- change copydir to correspond with *location-copied-from* (NB! upper directory in tree should be *location* or informative in some way)
- type *" 02-find_eprime_files.sh "* into the terminal on a unix-based system


**3) open 03-LCBC_eprime_parser.R**

- change path/task as needed
- "source" script within Rstudio to run OR type *" Rscript 03-LCBC_eprime_parser.R "* in terminal

(*Errors are produced if saved ID does not match with MOAS ID on Test_Date.
Thus, for a file to "process without error": ID and test date corresponds with MOAS ID and Test_Date*)

**4) open 04-LCBC_error_checker.R**
- change path/task as needed
- "source" script within Rstudio to run OR type *" 04-LCBC_error_checker.R "* in terminal (though Rstudio is recommended here). This will run an interactive program that is dependent on user input to correct the processing errors. 




## -------------- DIRECTORY STRUCTURE PER TASK --------------

**--> 1-ID_dataframes**
data .csv file per subID saved here (see COMPLETE.log)

**--> 2-COMPLETE_etxt**
etxt files that process without error moved here

**--> 3-COMPLETE_edat**
edat files that match filename with processed etxt file moved here

**--> 4-ERROR_check**
etxt files and linked edats that do not process successfully moved to new dir here (see ERROR-check.log)

**--> 5-COPY_etxt**
etxt files to process

**--> 6-COPY_edat**
edat files yet to be linked

**--> 7-FATAL**
etxt files/edats with irrecoverable data loss (see FATAL.log)

**--> 8-LOGS**
log per etxt


**ALL_FILES / COMPLETE / ERROR-check / FATAL / SESSION.log
--------------------------------------------------------------(full script output)**



Questions to James