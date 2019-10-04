
etxt_readLines <- function(etxt){
  # Find encoding type, this differs between tasks
  encoding <- readr::guess_encoding(etxt, n_max = 1000)[1,1]
  
  if (encoding == "ASCII") {
    readLines(etxt)
    
  } else if (encoding == "UTF-16LE") {
    con <- file(etxt, encoding = "UTF-16LE")
    r <- scan(con,
              what = character(),
              quiet = T)
    t <- close.connection(con)
    r
  }
}

etxt_clean <- function(efile_path, task){
  
  efile <- basename(efile_path)
  efile <- gsub(paste0(getExtension(efile),"|[.]"), "", efile)
  
  # load file & parse contents
  exp_lines = rprime::read_eprime(efile_path)
  experiment_data = rprime::FrameList(exp_lines)
  
  # extract header
  header = rprime::keep_levels(experiment_data, 1)
  header = utils::tail(rprime::to_data_frame(header), 1)
  header_cols =  c("Experiment","Subject","SessionDate","SessionTime")
  header = header[header_cols] 
  
  # remove header
  data = rprime::drop_levels(experiment_data, 1)
  
  # full data extracted
  df_data = rprime::to_data_frame(data)
  
  # subset df
  df_data = df_data[eprime_tasks[[task]]$cols]
  
  # add Block col
  if (task == "n-back" || task == "Antisaccade") {
    df_data = dplyr::mutate(df_data, "Block" = as.integer(rownames(df_data)))
  }
  
  # create outfile
  subcsv = suppressWarnings(data.frame(header, df_data))
  names(subcsv) = gsub("\\.", "_", names(subcsv))
  
  subcsv %>%
    dplyr::rename(Project_Wave_ID = Subject) %>% 
    dplyr::mutate(Filename = efile) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("Date")), date_change )
}

etxt_initiate <- function(ff, task, in_dir){
  
  # cleanup file names, make output path
  ff_names <- gsub("\\.txt", "", basename(ff))
  ff_new <- ff_names %>% 
    gsub(paste0(task, "-"), "", .) %>% 
    gsub("-", "_", .) %>% 
    paste(., task,sep="_")
  
  # Add BIDS type naming, for compatibility with other data
  ff_new <- strsplit(ff_new, "_")
  ff_new <- suppressWarnings(
    lapply(ff_new,  function(x){
      x[1] <- paste0("sub-", x[1]) 
      x[2] <- paste0("ses-", leading_zero(x[2], 2)) 
      paste0(x, collapse = "_")
    }) %>% 
      unlist() %>% 
      unname()
  )
  
  # Find IDs ----
  ff_nms <- gsub("Copy|[[:space:]]", "", ff_names) 
  ff_nms <- gsub("--", "-", ff_nms) 
  ids <- sapply(gsub(task, "", ff_nms), function(x) strsplit(x, "-")[[1]][2])
  ids <- suppressWarnings(as.numeric(ids))
  
  files_orig_path = gsub(in_dir, "", ff)
  
  # Fix them
  for(i in 1:length(ids)){
    #print(ids[i])
    tmp <- paste0("sub-", ids[i]) 
    ids[i] <- fix_ids(ids[i], files_orig_path[i]) 
    ff_new[i] <- gsub(tmp, paste0("sub-", ids[i]), ff_new[i])
  }
  
  # Read in the files
  dat <- suppressWarnings(
    sapply(ff, etxt_readLines)
  )
  
  # Count number of lines
  nlines <- unlist(unname(lapply(dat, length)))
  
  # Find date and time of test
  datetime <- lapply(dat, function(x){
    z <- x[1:35]
    z <- z[grep("SessionDate|SessionTime", z)+1]
    data.frame(Date = z[1],
               Time = z[2],
               stringsAsFactors = F)
  })
  datetime <- dplyr::bind_rows(datetime)
  
  # Make data.frame to work with
  ff_df <- dplyr::tibble(
    id = ids,
    date = as.character(as.Date(datetime$Date, format = "%m-%d-%Y")),
    time = datetime$Time,
    nlines = nlines,
    status = "",
    comment = "",
    processed_date = Sys.Date(),
    files_date_time = ff_new,
    files_orig = gsub("\\.txt", "", ff_names),
  ) 
  
  dplyr::mutate(ff_df, 
                files_date_time = paste(files_date_time, date, gsub(":","-",time), sep="_"),
                files_orig_path = files_orig_path
  )
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("subcheck", "datecheck",
                                                        "timecheck", "Subject", "time"))
