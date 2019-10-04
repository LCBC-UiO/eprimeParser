
# 
# edat_copy <- function(ff_df, in_dir, out_dir, logs){
#   
#   for(i in 1:nrow(ff_df)){
#     out_file <- ff_df$files_date_time[i]
#     file_log <- gsub("FILE", ff_df$files_date_time[i], logs$file)
#     
#     # check 1: edat ----
#     msg = paste("\n## Check 1: edat\nChecking if edat companion exists for", out_file)
#     log_it(msg, type = "message", logs = list(logs$session, file_log))
#     
#     edat_file = list.files(in_dir, 
#                            pattern=paste0(ff_df$files_orig[i], "\\.edat"), 
#                            recursive = T,
#                            full.names = T)
#     
#     edat_ok <- edat_check(edat_file, ff_df$files_orig[i], ff_df$files_date_time[i], logs)
#     
#     # edat out 
#     if (edat_ok) {
#       ext <- getExtension(edat_file)
#       new <- paste0(out_dir, "/", out_file, ".", ext)
#       log_it(paste0("Copying edat file to ", new), 
#              logs = list(logs$session, file_log))
#       
#       t <- file.copy(edat_file, new )
#     } 
#   }
# }
# 
# edat_move <- function(edat_file, out_file, out_dir, efile, logs, log_file){
#   if (length(edat_file) > 1) { 
#     
#     log_it(paste0("\nMultiple edat-files found (", 
#                   length(edat_file), ") containing name ", 
#                   gsub("txt", "edat", efile), 
#                   ". Will not move to ", out_dir), 
#            type = "warning", 
#            logs = c(logs$error, logs$session, log_file)
#            )
#     
#   } else if (!is_empty(edat_file)) {
#     
#     copy_files(in_file = edat_file, 
#                out_file = paste(out_dir, out_file, sep="/"), 
#                logs = logs,
#                log_file = log_file
#                )
#   }
# }
# 



if(getRversion() >= "2.15.1")  utils::globalVariables(c("SessionDate"))
