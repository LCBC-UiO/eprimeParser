
task <- "Antisaccade"
out_dir <- "test_folder"

in_dir <- "test_files/"
# in_dir <- "tests/testthat/test_files/"

# Need to setup some stuff, for testing to work
# Convenience function to do so
setup_tests <- function(task, in_dir, out_dir){
  
  unlink(out_dir, recursive = TRUE)
  MOAS <- readr::read_tsv(paste0(in_dir, "moas.tsv"))
  logs <- eprime_setup_logs(out_dir, task, quietly = TRUE)
  paths <- eprime_setup_paths(out_dir, task, logs, quietly = TRUE)
  
  ff_df <- eprime_find(in_dir = in_dir, 
                       task = task,
                       paths = paths,
                       logs = logs, 
                       quietly = TRUE)
  
  eprime_setup_files(ff_df = ff_df,
                     in_dir = in_dir, 
                     task = task,
                     paths = paths,
                     logs = logs,
                     quietly = TRUE)
  
  ff_df <- check_nlines(ff_df, task, paths = paths,
                        logs = logs[c("session", "file", "fatal", "status")],
                        quietly = TRUE)
  
  ff_df <- check_duplicates(ff_df, task, paths = paths,
                            logs = logs[c("session", "file", "error", "status")],
                            quietly = TRUE)
  
  ff_df <- ff_df %>% 
    arrange(date)
  
  list(MOAS = MOAS, paths = paths, logs = logs, ff_df = ff_df)
}


test_that("eprime_process_file works", {
  files <- setup_tests(task, in_dir, out_dir)
  efile_path <- paste0(files$paths$raw_etxt,"/", files$ff_df$files_date_time[1], ".txt")
  expect_output(eprime_process_file(efile_path, 
                                    in_dir=in_dir, task=task, MOAS=files$MOAS, 
                                    paths=files$paths, logs=files$logs, 
                                    start_message = "Testing"),
                "File completed processing")
  
  # Check that files have been sucessfully moved
  expect_true(file.exists(paste0(files$paths$etxt, 
                                 "/sub-1100863_ses-01_Antisaccade_2017-07-14_14-46-09.txt")))
  expect_true(file.exists(paste0(files$paths$tsv, 
                                 "/sub-1100863_ses-01_Antisaccade_2017-07-14_14-46-09.tsv")))
  
  # Check that IDs not in the MOAS throw errors
  files <- setup_tests(task, in_dir, out_dir)
  MOAStmp <- files$MOAS %>% mutate(CrossProject_ID = 1100400)
  expect_output(eprime_process_file(efile_path, 
                                    in_dir=in_dir, task=task, MOAS=MOAStmp, 
                                    paths=files$paths, logs=files$logs, 
                                    start_message = "Testing"),
                "No ID 1100863")
  
  # Check that when multiple matches these are added to logs
  files <- setup_tests(task, in_dir, out_dir)
  MOAStmp <- files$MOAS %>% mutate(Test_Date = "15-07-2017")
  tmp <- expect_output(eprime_process_file(efile_path, 
                                    in_dir=in_dir, task=task, MOAS=MOAStmp, 
                                    paths=files$paths, logs=files$logs,
                                    start_message = "Testing", 
                                    return_options = TRUE),
                "ID matches are being added to the logs")
  expect_equal(class(tmp), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(tmp), 1)
  expect_equal(tmp$CrossProject_ID, 1100863)
  # Check that file then is moved to error folder
  expect_true(file.exists(paste0(files$paths$error, 
                                 "/sub-1100979_ses-01_Antisaccade_2017-09-08_17-56-56.txt")))
  
  # Check that files newer than newest MOAS throws error
  files <- setup_tests(task, in_dir, out_dir)
  MOAStmp <- files$MOAS %>% mutate(Test_Date = "12-07-2017")
  expect_output(eprime_process_file(efile_path, 
                                    in_dir=in_dir, task=task, MOAS=MOAStmp, 
                                    paths=files$paths, logs=files$logs, 
                                    start_message = "Testing"),
                "This data is ahead of the MOAS in time")
})
