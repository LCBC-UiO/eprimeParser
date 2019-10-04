
test_that("date change works", {
  expect_equal(date_change("03-12-2019"),
               structure(17967, class = "Date"))
  
  expect_equal(date_change("12-24-2019"),
               structure(18254, class = "Date"))
})

test_that("stopping without message works", {
  expect_error(stop_quietly())
})


test_that("adding leading zeroes works", {
  expect_equal(leading_zero(3, 4),
               "0003")
  
  expect_equal(leading_zero("01", 2),
               "01")
  
  expect_equal(leading_zero("000009", 3),
               "009")
})


test_that("adding leading zeroes works", {
  expect_equal(fix_ids(1100333, orig_path = NULL),
               1100333)
  
  expect_error(fix_ids(333, orig_path = NULL),
               "CrossProject_ID")
  
  expect_equal(fix_ids(333, orig_path = "some/path/HUK/data"),
               1100333)
  
  expect_equal(fix_ids(333, orig_path = "some/path/nevrodev/data"),
               1000333)
  
  expect_equal(fix_ids(333, orig_path = "some/path/NCP/data"),
               1200333)
  
})




# File changing ----

task <- "Antisaccade"
out_dir <- "test_folder"

in_dir <- "test_files/"
# in_dir <- "tests/testthat/test_files/"

setup_tests <- function(task, in_dir, out_dir){
  unlink(out_dir, recursive = TRUE)
  MOAS <- readr::read_tsv(paste0(in_dir, "moas.tsv"))
  logs <- eprime_setup_logs(out_dir, task, quietly = TRUE)
  paths <- eprime_setup_paths(out_dir, task, logs, quietly = TRUE)
  
  list(MOAS = MOAS, paths = paths, logs = logs)
}

unlink(out_dir, recursive = TRUE)
files <- setup_tests(task, in_dir, out_dir)
ff_df <- eprime_find(in_dir, task, files$paths, files$logs, quietly = TRUE)

test_that("copy_raw works", {
  expect_output(copy_raw(ff_df[1,], in_dir, files$paths, files$logs, quietly = FALSE),
                "Copying")
  expect_true(file.exists(paste0(files$paths$raw_etxt,"/",  ff_df$files_date_time[1], ".txt")))
  expect_true(file.exists(paste0(files$paths$raw_edat,"/",  ff_df$files_date_time[1], ".edat2")))
  
  expect_output(copy_raw(ff_df[2,], in_dir, files$paths, files$logs, quietly = FALSE),
                "found multiple")
  expect_true(file.exists(paste0(files$paths$raw_etxt,"/",  ff_df$files_date_time[2], ".txt")))
  
  expect_output(copy_raw(ff_df[3,], in_dir, files$paths, files$logs, quietly = FALSE),
                "cannot find")
  expect_true(file.exists(paste0(files$paths$raw_etxt,"/",  ff_df$files_date_time[3], ".txt")))
})

test_that("copy_file works", {
  expect_output(copy_file(paste0(files$paths$raw_etxt,"/",  ff_df$files_date_time[3], ".txt"),
                          paste0(files$paths$etxt,"/",  ff_df$files_date_time[3], ".txt"),
                          files$paths, files$logs, gsub("FILE",ff_df$files_date_time[3], files$logs$file)),
                "Copying")
  expect_true(file.exists(paste0(files$paths$etxt,"/",  ff_df$files_date_time[3], ".txt")))
  
  expect_output(copy_file(paste0(files$paths$raw_etxt,"/",  ff_df$files_date_time[3], ".txt"),
                          paste0(files$paths$etxt,"/",  ff_df$files_date_time[3], ".txt"),
                          files$paths, files$logs, gsub("FILE",ff_df$files_date_time[3], files$logs$file)),
                "already exists")
  
  expect_error(copy_file(paste0(files$paths$etxt,"/",  ff_df$files_date_time[3], ".txt"),
                         paste0(files$paths$etxt,"/",  ff_df$files_date_time[3], ".txt"),
                         files$paths, files$logs, gsub("FILE",ff_df$files_date_time[3], files$logs$file)),
               "")
  
})

test_that("move_file works", {
  
  expect_error(move_file("sub-1100920_ses-01_Antisaccade_2017-08-29_11-44-36.txt", 
                         "sub-1100920_ses-01_Antisaccade_2017-08-29_11-44-36.txt", 
                         files$paths, files$logs, gsub("FILE",ff_df$files_date_time[3], files$logs$file), quietly = FALSE),
               "")
  
  expect_output(move_file(paste0(files$paths$error, "/sub-1100920_ses-01_Antisaccade_2017-08-29_11-44-36.txt"), 
                          paste0(files$paths$error, "/sub-1100920_ses-01_Antisaccade_2017-08-29_11-44-36_est.txt"), 
                          files$paths, files$logs, gsub("FILE",ff_df$files_date_time[3], files$logs$file), quietly = FALSE),
                "Moving")
  expect_true(file.exists(paste0(files$paths$error, "/sub-1100920_ses-01_Antisaccade_2017-08-29_11-44-36_est.txt")))
  
  copy_file(paste0(files$paths$raw_etxt, "/sub-1100863_ses-03_Antisaccade_2017-07-14_14-46-09.txt"),
            paste0(files$paths$etxt, "/sub-1100863_ses-03_Antisaccade_2017-07-14_14-46-09.txt"),
            files$paths, files$logs, gsub("FILE",ff_df$files_date_time[3], files$logs$file), quietly = TRUE)
  
  expect_output(move_file(paste0(files$paths$raw_etxt, "/sub-1100863_ses-03_Antisaccade_2017-07-14_14-46-09.txt"), 
                          paste0(files$paths$etxt, "/sub-1100863_ses-03_Antisaccade_2017-07-14_14-46-09.txt"), 
                          files$paths, files$logs, gsub("FILE",ff_df$files_date_time[2], files$logs$file), quietly = FALSE),
                "already exists")
  expect_true(file.exists(paste0(files$paths$error, "/sub-1100863_ses-03_Antisaccade_2017-07-14_14-46-09.txt")))
  
})


test_that("update_filenames works", {
  ff <- list.files(files$paths$raw_etx)
  expect_output(update_filenames(ff[1], gsub("\\.txt", "testing.txt", ff[1]), 
                                 files$paths, files$logs, quietly = FALSE),
                "Altering file names to reflect correct session")
  
  ff <- list.files(files$paths$main, pattern="testing", 
                   recursive = TRUE, full.names = TRUE)
  expect_true(file.exists(ff[1]))
})


# File extention handling
test_that("getExtension works", {
  expect_equal(getExtension("test/path/to/somethere/file_with_123_in_it.pdf"),
               "pdf")
  
  expect_equal(getExtension("test/path/file_with_123_in_it.md"),
               "md")
  
})

test_that("barename works", {
  expect_equal(barename("test/path/to/somethere/file.md"),
               "file")
  
  expect_equal(barename("test/path/to/somethere/file_with_123_in_it.pdf"),
               "file_with_123_in_it")
})


test_that("choose_option works", {
  expect_equal(choose_option(1), 1)
  expect_equal(choose_option(2), 2)
  expect_equal(choose_option(0), 0)
  
  expect_equal(choose_option(3, choices = c("these", "are", "choices"),
                title = "this is title"), 3)
})

unlink(out_dir, recursive = TRUE)
