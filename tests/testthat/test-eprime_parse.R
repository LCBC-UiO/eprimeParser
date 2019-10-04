
out_dir <- "test_folder"
unlink(out_dir, recursive = TRUE)

in_dir <- "test_files/"
# in_dir <- "tests/testthat/test_files"


test_that("find parser works", {
  
  t <- capture_output(eprime_parse(in_dir = in_dir,
                                    out_dir = out_dir,
                                    task = "Antisaccade",
                                    MOAS = paste(in_dir, "moas.tsv", sep="/")))
  expect_true(grepl("There are files with issues", t))
  
  
  expect_error(eprime_parse(in_dir = in_dir,
                             out_dir = out_dir,
                             task = "Antisaccade",
                             MOAS = paste(in_dir, "moas.tsv", sep="/"), quietly = TRUE))
  
  unlink(out_dir, recursive = TRUE)
  expect_silent(eprime_parse(in_dir = in_dir,
                              out_dir = out_dir,
                              task = "Antisaccade",
                              MOAS = paste(in_dir, "moas.tsv", sep="/"), 
                              quietly = TRUE))
})

unlink(out_dir, recursive = TRUE)
