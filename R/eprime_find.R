#' Find eprime files
#'
#' @param in_dir Directory with E-prime files
#' @param data_dir Sub-dir of base that has the data
#' @param out_dir directory to output the data to
#' @param task Which task, one of ["Antisaccade", "n-back", "Attention"]
#' @param copy_etxts binary logical, 1 to copy the txt's
#' @param copy_edats binary logical, 1 to copy the edat's.
#'
#' @export
#'
eprime_find <- function(in_dir = "~/LCBC/Projects/Cross_projects/computer_tasks",
                        data_dir = "C_Eprime_Testrom1", #must have /LOCATION (i.e.TestRoom1/TestRoom2)
                        out_dir = "~/LCBC/Users/athanasm_mo/",
                        task = "Antisaccade",
                        copy_etxts=1,
                        copy_edats=1){

  # script <- system.file("find_eprime_files.sh", package = "eprimeParser")
  script <- "R/find_eprime_files.sh"
  
  system(
    paste("sh", script, in_dir, data_dir, out_dir, task, copy_etxts, copy_edats)
    )
  
}
