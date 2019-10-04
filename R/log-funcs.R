
# Add information to the parser status.log
add_log_info <- function(data, condition, status_string, comment_string){
  condition <- dplyr::enquo(condition)
  dplyr::mutate(data,
                status = ifelse(!!condition, status_string, status),
                comment = ifelse(!!condition, comment_string, comment)
  )
}

write_status <- function(ff_df, logs){
  status <- if(file.exists(logs$status)){
    in_process <- suppressMessages(readr::read_tsv(logs$status) )
    
    suppressMessages(
      dplyr::bind_rows(
        in_process, 
        ff_df %>% readr::type_convert()
      )
    )
    
  }else{
    ff_df
  }
  readr::write_tsv(status, path = logs$status, na="")
}


update_status <- function(status_path, condition, status_string, comment_string){
  condition <- dplyr::enquo(condition)
  
  stat <- read_tsv(status_path, 
                   col_types = cols(
                     id = col_double(),
                     files_orig = col_character(),
                     date = col_character(),
                     time = col_character(),
                     nlines = col_double(),
                     status = col_character(),
                     comment = col_character(),
                     processed_date = col_character(),
                     files_date_time = col_character(),
                     files_orig_path = col_character()
                   )) %>% 
    dplyr::mutate(
      status = ifelse(!!condition, status_string, status),
      comment = ifelse(!!condition, comment_string, comment)
    ) %>% 
    write_tsv(path = status_path, na = "")
}

update_status_filename <- function(status_path, condition, files_date_time_new){
  condition <- dplyr::enquo(condition)
  
  ss <- strsplit(files_date_time_new, "_")[[1]]

  stat <- read_tsv(status_path, 
                   col_types = cols(
                     id = col_double(),
                     files_orig = col_character(),
                     date = col_character(),
                     time = col_character(),
                     nlines = col_double(),
                     status = col_character(),
                     comment = col_character(),
                     processed_date = col_character(),
                     files_date_time = col_character(),
                     files_orig_path = col_character()
                   )) %>% 
    dplyr::mutate(
      id = ifelse(!!condition, gsub("sub-", "", ss[1]), id),
      files_date_time = ifelse(!!condition, files_date_time_new, files_date_time)
    ) %>% 
    write_tsv(path = status_path, na = "", quote_escape = FALSE)
}

# Verbose message functions ----

# Function to create custom log entries that also print out messages to the console.
log_it <- function(string, type = "message", logs = NULL, quietly = FALSE){
  
  if(any(class(string) %in% "data.frame")){
    for(k in logs){
      cat(paste0(colnames(string), collapse='\t'), file = k, append = T, sep = '\n')
      cat(apply(string,1,paste0, collapse='\t'), file = k, append = T, sep = '\n')
    }
  }else if(class(string) == "character"){
    string <- paste0(string, "  ")
    
    if(!is.null(logs)){
      # add to logs
      t <- switch(type, 
                  "message" = string,
                  "warning" = paste0(type, ": ", string),
                  "error" =   paste0(type, ": ", string),
                  "ok" = string
      )
      
      lapply(logs, function(x) 
        cat(t, file = x, append = T, sep = "\n")
      )
    }
    
    if(quietly == FALSE){
      # Print it to console
      msg <- switch(type,
                    "message" = string,
                    "warning" = warn(string),
                    "error" =   err(string),
                    "ok" = ok(string)
      )
      cat(paste0(msg, "\n\n"))
    }
  }  
}  

warn <- function(string){
  cat(crayon::yellow(string))
}

err <- function(string){
  cat(crayon::red(string))
}

note <- function(string){
  cat(string)
}

ok <- function(string){
  cat(crayon::green(string))
}



if(getRversion() >= "2.15.1")  utils::globalVariables(c("status"))
