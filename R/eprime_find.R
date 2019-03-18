#' Find eprime files
#'
#' @param basedir Directory of files
#' @param copydir Sub-dir of basefr that have the data
#' @param task Which task, one of ["Antisaccade", "n-back", "Attention"]
#' @param copy_etxts binary logical, 1 to copy the txt's
#' @param copy_edats binary logical, 1 to copy the edat's.
#'
#' @export
#'
#' @examples
eprime_find <- function(basedir="~/LCBC/Projects/Cross_projects/computer_tasks",
                        copydir="C_Eprime_Testrom1", #must have /LOCATION (i.e.TestRoom1/TestRoom2)
                        task="Antisaccade",
                        #tasks="n-back \
                        #Attention \
                        #Antisaccade"
                        copy_etxts=1,
                        copy_edats=1){
  
  script <- system.file("find_eprime_files.sh", package = "eprimeParser")
  
  system(
    paste("sh", script, basedir, copydir, task, copy_etxts, copy_edats)
    )
  
}
