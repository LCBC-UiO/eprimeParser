out_dir <- "test_folder"
unlink(out_dir, recursive = TRUE)

task <- "Antisaccade"
in_dir <- "test_files/"
# in_dir <- "tests/testthat/test_files"

eprime_parse(in_dir = in_dir,
             out_dir = out_dir,
             task = task,
             MOAS = paste(in_dir, "moas.tsv", sep="/"), 
             quietly = TRUE)


logs <- eprime_setup_logs(out_dir, task, TRUE)

# Single logs entries ----

test_that("log_it", {
  
  # check if it prints to console or not
  expect_silent(log_it("hello", quietly = TRUE))
  expect_output(log_it("hello"))
  
  # check if it prints to file
  log_it("hello", logs = logs$session, quietly = T)
  tmp <- readLines(logs$session)
  expect_equal(tmp[length(tmp)], "hello  ")
  
})


# Status logs updates ----


test_that("add_log_info", {
  
  status <- readr::read_tsv(paste(out_dir, task, "logs/status.log", sep="/"),
                            col_types = cols())              
  status <- add_log_info(status, nlines == 13893, "altered", "changed")
  
  expect_equal(sum(grepl("altered", status$status)), 2)
  expect_equal(sum(grepl("changed", status$comment)), 2)
})

test_that("write_status", {
  status <- readr::read_tsv(paste(out_dir, task, "logs/status.log", sep="/"),
                            col_types = cols())   
  
  write_status (status, logs)
  
  status <- readr::read_tsv(paste(out_dir, task, "logs/status.log", sep="/"),
                            col_types = cols())   
  expect_equal(nrow(status), 8)
  
  write_status (status, logs)
  status <- readr::read_tsv(paste(out_dir, task, "logs/status.log", sep="/"),
                            col_types = cols())   
  expect_equal(nrow(status), 16)
})


test_that("update_status", {
  update_status(logs$status, nlines == 1152, "testing", "altered test")
  
  status <- readr::read_tsv(paste(out_dir, task, "logs/status.log", sep="/"),
                            col_types = cols())   
  status <- filter(status, status == "testing")
  
  expect_equal(nrow(status), 4)
  expect_equal(unique(status$comment), "altered test")
  
})

test_that("update_status_filename", {
  
  update_status_filename(logs$status, grepl("sub-1100920_ses-01", files_date_time), 
                         "sub-1100920_ses-01_sucker")
  
  status <- readr::read_tsv(paste(out_dir, task, "logs/status.log", sep="/"),
                            col_types = cols())   
  status <- filter(status, files_date_time == "sub-1100920_ses-01_sucker")
  
  expect_equal(nrow(status), 4)
  expect_equal(unique(status$files_date_time), "sub-1100920_ses-01_sucker")
})


test_that("Crayons are working", {
  op <- options()
  on.exit(options(op))
  options(crayon.enabled = TRUE)
  
  expect_output(warn("hello"), "\\\033\\[33mhello\\\033\\[39m")
  expect_output(err("hello"), "\\\033\\[31mhello\\\033\\[39m")
  expect_output(note("hello"), "hello")
  expect_output(ok("hello"), "\\\033\\[32mhello\\\033\\[39m")
})


# Remove test dir
unlink("logs-test", recursive = TRUE)
unlink(out_dir, recursive = TRUE)
