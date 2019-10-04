
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


files <- setup_tests(task, in_dir, out_dir)
ff_df <- eprime_find(in_dir, task, files$paths, files$logs, quietly = TRUE)

eprime_setup_files(ff_df, in_dir, task, files$logs, files$paths, quietly = TRUE)


test_that("test check_session", {

  expect_null(check_session("sub-1100883_ses-03_2019-01-01_12-00-00", 3))
  expect_equal(check_session("sub-1100883_ses-03_2019-01-01_12-00-00", 4, logs = files$logs),
               "sub-1100883_ses-04_2019-01-01_12-00-00")

})


test_that("test check_tilde_paths", {
  
  expect_error(check_tilde_paths("~/User/athanasm"),
               "tilde")
  
  expect_null(check_tilde_paths("/User/athanasm"))
  
})


test_that("test check_nlines", {

  expect_output(check_nlines(ff_df, task, files$logs, files$paths, quietly = FALSE),
                "too few lines")
  expect_silent(check_nlines(ff_df, task, files$logs, files$paths, quietly = TRUE))
  
  tmp <- check_nlines(ff_df, task, files$logs, files$paths, quietly = TRUE)
  expect_equal(nrow(tmp), 3)

})


test_that("test check_duplicates", {

  expect_output(check_duplicates(ff_df, task, files$logs, 
                                 files$paths, quietly = FALSE),
                "Found duplicates")
  expect_silent(check_duplicates(ff_df, task, files$logs, 
                                 files$paths, quietly = TRUE))
  
  tmp <- check_duplicates(ff_df, task, files$logs, 
                          files$paths, quietly = TRUE)
  expect_equal(nrow(tmp), 2)
  expect_equal(tmp$id, c(1100863, 1100920))

  tmp <- filter(ff_df, !files_orig %in% tmp$files_orig)
  
  err_files <- list.files(files$paths$error, pattern = as.character(tmp$id[1]))  
  expect_equal(length(err_files), 2)
})


test_that("test check_existing", {

  expect_error(check_existing(ff_df, files$logs, quietly = TRUE),
                "")

  stat <- readr::read_tsv(files$logs$status, col_types = cols()) 
  stat <- stat[-1, ]
  readr::write_tsv(stat, files$logs$status, na="")
  
  tmp <- expect_output(check_existing(ff_df, files$logs, quietly = FALSE),
                       "new")
  
  
  unlink(out_dir, recursive = TRUE)
  files <- setup_tests(task, in_dir, out_dir)
  
  ff_df <- list.files(in_dir, 
                      pattern=paste0(task,".*txt"), 
                      recursive = T, 
                      full.names = T) 
  ff_df <- etxt_initiate(ff_df, task = task, in_dir = in_dir)
  
  tmp <- expect_output(check_existing(ff_df, files$logs, quietly = FALSE),
                       "Found \\*\\*4")
  expect_equal(class(tmp), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(tmp), 4)
  
})

# edat checks ----
test_that("test check_edat", {

  in_file <- paste0(in_dir, ff_df$files_orig_path[1])
  in_file <- list.files(path = gsub(paste0(ff_df$files_orig[1], "|\\.txt"), "", in_file),
                        pattern = paste0(ff_df$files_orig[1], ".edat"), full.names = T)
  
  expect_output(check_edat(in_file, ff_df$files_orig[1], ff_df$files_date_time[1], 
             files$logs, quietly = FALSE),
             "edat companion file found")
  
  in_file <- paste0(in_dir, ff_df$files_orig_path[2])
  in_file <- list.files(path = gsub(paste0(ff_df$files_orig[2], "|\\.txt"), "", in_file),
                        pattern = paste0(ff_df$files_orig[2], ".edat"), full.names = T)
  
  expect_output(check_edat(in_file, ff_df$files_orig[2], ff_df$files_date_time[2], 
                           files$logs, quietly = FALSE),
                "found multiple")
  
  in_file <- paste0(in_dir, ff_df$files_orig_path[3])
  in_file <- list.files(path = gsub(paste0(ff_df$files_orig[3], "|\\.txt"), "", in_file),
                        pattern = paste0(ff_df$files_orig[3], ".edat"), full.names = T)
  
  expect_output(check_edat(in_file, ff_df$files_orig[3], ff_df$files_date_time[3], 
                           files$logs, quietly = FALSE),
                "cannot find")

})

# MOAS checks ----

test_that("test check_MOAS_subj", {
  
  tmp <- check_MOAS_subj(MOAS = files$MOAS, subject = 1100913)
  expect_equal(tmp$CrossProject_ID, 1100913)
  expect_equal(tmp$Subject_Timepoint, 1)
  
  expect_equal(nrow(tmp), 1)
  expect_equal(ncol(tmp), 9)
  
  tmp <- check_MOAS_subj(MOAS = files$MOAS, subject = 910)
  expect_equal(nrow(tmp), 0)
  expect_equal(ncol(tmp), 9)

})


test_that("test check_MOAS_date", {
  
  tmp <- check_MOAS_date(MOAS = files$MOAS, date = "2017-07-14")
  expect_equal(tmp$CrossProject_ID, 1100863)
  expect_equal(tmp$Subject_Timepoint, 1)
  
  expect_equal(nrow(tmp), 1)
  expect_equal(ncol(tmp), 9)
  
  tmp <- check_MOAS_date(MOAS = files$MOAS, date = "2018-07-14")
  expect_equal(nrow(tmp), 0)
  expect_equal(ncol(tmp), 9)
})

# Manual checks ----


test_that("test check_manual_moas works", {
  
  tmp_moas <- files$MOAS %>% 
    dplyr::mutate(selection = paste(CrossProject_ID, Test_Date, sep=": "))
  
  new_info <- check_manual_moas(1, opts = tmp_moas, 
                                efile_orig = "Antisaccade-1100863-3", quietly = TRUE)
  expect_true(is.list(new_info))
  expect_equal(names(new_info), c("new_id", "new_ses"))  
  expect_equal(names(new_info), c("new_id", "new_ses"))  
  expect_equal(length(new_info$new_id), 1)
  expect_equal(length(new_info$new_ses), 1)

  new_info <- check_manual_moas(3, opts = tmp_moas, 
                                efile_orig = "Antisaccade-1100863-3", quietly = TRUE)
  expect_true(is.null(new_info))
  
  new_info <- check_manual_moas(0, opts = tmp_moas, 
                                efile_orig = "Antisaccade-1100863-3", quietly = TRUE)
  expect_true(is.null(new_info))

})


# gcalendr checks ----
# test_that("test check_calendar", {
#   
#   expect_equal(check_calendar(date = "08-08-2019"),
#                structure(list(event = character(0), 
#                               start_date = structure(numeric(0), class = "Date"), 
#                               html_link = character(0), 
#                               ical_uid = character(0)), 
#                          row.names = integer(0), 
#                          class = c("tbl_df","tbl", "data.frame"))
#   )
#   
# })


# Remove test dir
unlink(out_dir, recursive = TRUE)

