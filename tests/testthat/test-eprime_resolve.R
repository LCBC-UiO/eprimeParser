
# Do some setup to prep for testing
task <- "Antisaccade"
out_dir <- "test_folder"

in_dir <- "test_files/"
# in_dir <- "tests/testthat/test_files/"
moas_path = paste0(in_dir, "moas.tsv")

test_that("eprime_resolve_file works", {

  # Get necessary information for the resolve functions
  logs <- eprime_setup_logs(out_dir, task, quietly = TRUE)
  paths <- eprime_setup_paths(out_dir, task, logs, quietly = TRUE)
  
  # Remove the out_dir
  unlink(out_dir, recursive = TRUE)
  
  # Should fail ir the dir does not exist
  expect_error(
    eprime_resolve_file(files_date_time = "sub-1100979_ses-01_Antisaccade_2017-09-08_17-56-56", 
                        file_date = "2017-09-08", in_dir, task, paths, logs, moas_path),
    "There are no files in")
  
  # Initiate the folder with files, to attempt solve
  eprime_parse(in_dir, out_dir, task, moas_path, TRUE)
  
  expect_output(eprime_resolve_file(files_date_time = "sub-1100979_ses-01_Antisaccade_2017-09-08_17-46-56",
                                    file_date = "2017-09-08", 
                                    in_dir, task, paths, logs, moas_path,
                                    .choice = 1),
                "Altering files to correspond to manually inputted information")
  expect_true(file.exists(paste0(paths$etxt, 
                                 "/sub-1100913_ses-01_Antisaccade_2017-09-08_17-46-56.txt")))
  expect_true(file.exists(paste0(paths$tsv, 
                                 "/sub-1100913_ses-01_Antisaccade_2017-09-08_17-46-56.tsv")))
  
  unlink(out_dir, recursive = TRUE)
})


test_that("eprime_resolve_all works", {
  unlink(out_dir, recursive = TRUE)
  
  expect_error(eprime_resolve_all(in_dir, out_dir, task, 
                                  MOAS = moas_path,
                                  gmail_user = "athanasiamo@gmail.com"),
               "does not exists. No data to resolve.")
  
  expect_error(eprime_resolve_all(in_dir, "~/test_dir/", task, 
                                  MOAS = moas_path,
                                  gmail_user = "athanasiamo@gmail.com"),
               "tilde")
  
  eprime_parse(in_dir, out_dir, task, moas_path, TRUE)
  expect_output(eprime_resolve_all(in_dir, out_dir, task, 
                                   MOAS = moas_path,
                                   gmail_user = "athanasiamo@gmail.com"),
                "There are no files to resolve")
  
  # Create a faux file to resolve
  update_status(paste0(out_dir,"/",task, "/logs/status.log"),
                files_date_time == "sub-1100979_ses-01_Antisaccade_2017-09-08_17-56-56",
                status_string = "needs resolution", comment_string = NA)
  
  # expect_output(eprime_resolve_all(in_dir, out_dir, task,
  #                                  MOAS = moas_path,
  #                                  gmail_user = "athanasiamo@gmail.com",
  #                                  .choice = 1),
  #               "Entering manual processing")
  
})



