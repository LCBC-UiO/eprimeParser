task <- "Antisaccade"
out_dir <- "test_folder"

in_dir <- "test_files/"
# in_dir <- "tests/testthat/test_files/"

unlink(out_dir, recursive = TRUE)
MOAS <- readr::read_tsv(paste0(in_dir, "moas.tsv"))
logs <- eprime_setup_logs(out_dir, task, quietly = TRUE)
paths <- eprime_setup_paths(out_dir, task, logs, quietly = TRUE)
ff_df <- eprime_find(in_dir, task, paths, logs, quietly = TRUE)
eprime_setup_files(ff_df, in_dir, task, logs, paths, quietly = TRUE)

test_that("edat_find works", {


  expect_output(edat_find(paths$raw_edat, ff_df$files_date_time[1], 
            paths, logs, log_file = gsub("FILE", ff_df$files_date_time[1],logs$file), 
            quietly = FALSE),
            "Moving companion")
})

unlink(out_dir, recursive = TRUE)
