context("moas-functions")

moas_path = "test_files/moas.tsv"
# moas_path = "tests/testthat/test_files/moas.tsv"


test_that("moas loading works", {
  MOAS <- get_moas(moas_path)
  
  expect_equal(class(MOAS), "data.frame")
  expect_equal(names(MOAS), c("CrossProject_ID", "Subject_Timepoint", "Project_Wave", "Project_Name", 
                              "Project_Wave_ID", "Test_Date", "Age", "Sex", "Birth_Date"))
  
  # Check if providing function the df, df is returned
  MOAS <- get_moas(MOAS)
  expect_equal(class(MOAS), "data.frame")
  expect_equal(names(MOAS), c("CrossProject_ID", "Subject_Timepoint", "Project_Wave", "Project_Name", 
                              "Project_Wave_ID", "Test_Date", "Age", "Sex", "Birth_Date"))
  
  expect_error(get_moas("test"), "MOAS not supplied")
})

test_that("find entry in MOAS works", {
  MOAS <- get_moas(moas_path)
  
  inMOAS <- find_in_moas(subject = 1100863, "2017-07-14", MOAS)
  expect_equal(nrow(inMOAS), 1)
  expect_equal(inMOAS$CrossProject_ID, 1100863)
  expect_equal(inMOAS$Subject_Timepoint, 1)
  
  inMOAS <- find_in_moas(subject = 1100866, "2017-07-14", MOAS)
  expect_equal(nrow(inMOAS), 0)
  
  inMOAS <- find_in_moas(subject = 979, "2017-09-08", MOAS)
  expect_equal(nrow(inMOAS), 1)
  
})


