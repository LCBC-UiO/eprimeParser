
date_change <- function(x){
  as.Date(x, format="%m-%d-%Y")
}

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

leading_zero <- function(x, width){
  ex <- paste0("%0", width, "d")
  sprintf(ex, as.numeric(x))
  
}

fix_ids <- function(subject, orig_path = NULL){
  
  if(nchar(as.character(subject)) != 7){
    
    if(is.null(orig_path)){
      stop("ID is not CrossProject_ID, supply full path of original file to guess it.")
    }
    
    idx <- sapply(projects$name, function(x) grepl(x, orig_path, ignore.case = TRUE))
    orig_proj <- projects[idx,]
    
    as.numeric(subject) + orig_proj$Project_Number*100000
  }else{
    as.numeric(subject)
  }
}



# File changing ----
copy_raw <- function(ff_df, in_dir, paths, logs, quietly = FALSE){
  
  for(i in 1:nrow(ff_df)){
    log_file <- gsub("FILE", ff_df$files_date_time[i], logs$file )
    
    # Copy raw etxt
    in_file <- paste0(in_dir, ff_df$files_orig_path[i])
    out_file <- paste0(paths$raw_etxt,"/", ff_df$files_date_time[i], ".txt")
    
    copy_file(in_file, out_file, paths, logs, log_file, quietly)
    
    # Copy raw edat
    in_file <- list.files(path = gsub(paste0(ff_df$files_orig[i], "|\\.txt"), "", in_file),
                          pattern = paste0(ff_df$files_orig[i], ".edat"), full.names = T)
    
    edat_ok <- check_edat(in_file, ff_df$files_orig[i], ff_df$files_date_time[i], 
                          logs, quietly)
    
    if(edat_ok){
      ext <- getExtension(in_file)
      
      out_file <- paste0(paths$raw_edat,"/", ff_df$files_date_time[i], ".", ext)
      
      copy_file(in_file, out_file, paths, logs, log_file, quietly)
    }
  }
}

copy_file <- function(in_file, out_file, paths, logs, log_file, quietly = FALSE){
  
  #parts of the code may ask to move a file, though its the same.
  if(in_file == out_file) stop_quietly()
  
  if(file.exists(out_file)){
    new_out <- paste0(paths$error, "/", basename(out_file))
    log_it(paste0("File ", out_file, " already exists. Copying ", 
                  in_file, " to errors-folder: ", new_out),
           logs = c(logs$error, logs$session, log_file),
           type = "warning", quietly = quietly
    )
    
    tt <- file.copy(in_file, new_out, overwrite = TRUE) 
    
  }else{
    log_it(paste0("Copying ", in_file, " to ", out_file),
           logs = c(logs$session, log_file), quietly = quietly
    )
    tt <- file.copy(in_file, out_file)
  }
  
  if(tt == FALSE & !file.exists(out_file)){
    msg <- paste0("File copy failed for ", in_file, " to ", out_file, ". Unknown reason.")
    log_it(msg, type="error",
           logs = c(logs$error, logs$session, log_file),
           quietly = quietly)
  }
}

move_file <- function(in_file, out_file, paths, logs, log_file, quietly = FALSE){
  
  #parts of the code may ask to move a file, though its the same.
  if(in_file == out_file) stop_quietly()
  
  if(file.exists(out_file)){
    new_out <- paste0(paths$error, "/", basename(out_file))
    log_it(paste0("File ", out_file, " already exists. Moving ", 
                  in_file, " to errors-folder: ", new_out),
           logs = c(logs$error, logs$session, log_file),
           type = "warning", quietly = quietly
    )
    
    tt <- file.rename(in_file, new_out) 
    
  }else{
    log_it(paste0("Moving ", in_file, " to ", out_file),
           logs = c(logs$session, log_file), quietly = quietly
    )
    tt <- file.rename(in_file, out_file)
  }
  
  if(tt == FALSE & !file.exists(out_file)){
    msg <- paste0("File move failed for ", in_file, " to ", out_file, ". Unknown reason.")
    log_it(msg, type="error", quietly = quietly,
           logs = c(logs$error, logs$session, log_file))
  }
}

update_filenames <- function(old_name, new_name, paths, logs, quietly = FALSE){
  
  # in case full paths are given, find the bare name of files
  old_name <- barename(old_name)
  new_name <- barename(new_name)
  
  # find all files with the old name in them
  fs <- list.files(paths$main, pattern = old_name, full.names = TRUE, recursive = TRUE)
  
  # remove the raw files, we let them stay the same
  fs <- fs[!grepl("raw", fs)]
  
  msg <- "Altering file names to reflect correct session."
  log_it(msg, type = "message", logs, quietly = quietly)
  
  t <- sapply(fs, function(x) file.rename(x, gsub(old_name, new_name, x)) )
  
}

# File extention handling
getExtension <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  ex[-1]
} 

barename <- function(file_path){
  filename <- basename(file_path)
  gsub(paste0(getExtension(filename),"|[.]"), "", filename)
}


# Menu
choose_option <- function(which = NULL, choices = c("a", "b") , title = "Test title") {
  if (is.null(which)) {
    choice <- utils::menu(
      choices = choices,
      title = title
    )
    if (choice == 0) {
      return(invisible())
    } else {
      which <- choices[choice]
    }
  }
  
  which
}
