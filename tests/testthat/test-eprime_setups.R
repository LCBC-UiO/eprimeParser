
task <- "Antisaccade"
out_dir <- "test_folder"

in_dir <- "test_files/"
# in_dir <- "tests/testthat/test_files/"

test_that("eprime_setup_logs works", {
  unlink(out_dir, recursive = TRUE)
  
  expect_output(logs <- eprime_setup_logs(out_dir, task, quietly = FALSE),
                "first time")
  
  expect_equal(names(logs), 
               c("fatal", "error", "status", "file", "session"))
  
  expect_equal(list.dirs(out_dir), 
               c("test_folder", 
                 "test_folder/Antisaccade", 
                 "test_folder/Antisaccade/logs", 
                 "test_folder/Antisaccade/logs/files", 
                 "test_folder/Antisaccade/logs/sessions")
               )
  
  expect_equal(list.files(out_dir, recursive = TRUE), 
               gsub("^/", "", gsub(out_dir, "", logs$session))
  )
  unlink(out_dir, recursive = TRUE)
  
})

test_that("eprime_setup_paths works", {
  logs <- eprime_setup_logs(out_dir, task, quietly = TRUE)
  
  paths <- expect_output(eprime_setup_paths(out_dir, task, logs, quietly = FALSE),
                         "Setting up")
  
  paths <- expect_silent(eprime_setup_paths(out_dir, task, logs, quietly = FALSE))
  
  unlink(out_dir, recursive = TRUE)
  logs <- eprime_setup_logs(out_dir, task, quietly = TRUE)
  paths <- expect_silent(eprime_setup_paths(out_dir, task, logs, quietly = TRUE))
  
  dd <- list.dirs(out_dir)
  dd <- dd[grep("data", dd)]
  dd <- c(dd, paste0(out_dir, "/", task, "/"))

  expect_equal(sum(dd %in% unname(unlist(paths))), 8)
  
  tt <- list();for(k in paths) { tt[k] <- dir.exists(k) }
  expect_true(all(unlist(tt)))
})



test_that("eprime_setup_files works", {
  unlink(out_dir, recursive = TRUE)
  
  logs <- eprime_setup_logs(out_dir, task, quietly = TRUE)
  paths <- expect_silent(eprime_setup_paths(out_dir, task, logs, quietly = TRUE))
  ff_df <- eprime_find(in_dir, task, paths, logs, quietly = TRUE)
  
  expect_output(eprime_setup_files(ff_df, in_dir, task, logs, paths, quietly = FALSE),
                "lifetime")
  
  unlink(out_dir, recursive = TRUE)
  
  logs <- eprime_setup_logs(out_dir, task, quietly = TRUE)
  paths <- expect_silent(eprime_setup_paths(out_dir, task, logs, quietly = TRUE))
  ff_df <- eprime_find(in_dir, task, paths, logs, quietly = TRUE)
  
  expect_silent(eprime_setup_files(ff_df, in_dir, task, logs, paths, quietly = TRUE))
})


unlink(out_dir, recursive = TRUE)

