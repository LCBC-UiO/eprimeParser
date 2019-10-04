
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

test_that("eprime find works", {

  tmp <- expect_output(eprime_find(in_dir, task, files$paths, files$logs, quietly = FALSE))
  expect_equal(nrow(tmp), 4)
  
  expect_error(eprime_find(in_dir, task, files$paths, files$logs, quietly = TRUE))
  
  files <- setup_tests("n-back", in_dir, out_dir)
  expect_error(eprime_find(in_dir, "n-back", files$paths, files$logs, quietly = TRUE))
  
})



