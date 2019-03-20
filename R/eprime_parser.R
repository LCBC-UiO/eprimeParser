#' Parse LCBC E-prime files
#'
#' @inheritParams  eprime_find 
#' @param MOAS Either string with path to, or as data.frame, the MOAS dataframe
#'
#' @export
#' @importFrom dplyr mutate select distinct
eprime_parser <- function(in_dir = "~/LCBC/Projects/Cross_projects/computer_tasks",
                          out_dir = "~/LCBC/Users/athanasm_mo",
                          data_dir = "C_Eprime_Testrom1",
                          task = "Antisaccade",
                          MOAS = "~/LCBC/Projects/Cross_projects/MOAS/Data/MOAS.RData"
){
  
  if(is.data.frame(MOAS)){
    MOAS = MOAS 
  }else if(is.character(MOAS)){
    nn = load(MOAS)
    MOAS = get(nn)
  }else{
    stop("MOAS not supplied. Either provide path to MOAS.RData, or a pre-loaded MOAS.")
  }
  
  # Make sure ID is integer, easier handling
  MOAS <- MOAS %>% 
    dplyr::mutate(CrossProject_ID = as.integer(as.character(CrossProject_ID))) %>% 
    dplyr::select(CrossProject_ID,
                  Subject_Timepoint,
                  Project_Wave,
                  Project_Name,
                  Project_Wave_ID,
                  Test_Date,
                  Age,
                  Sex,
                  Birth_Date) %>% 
    dplyr::distinct()
  
  path = paste(out_dir, task, sep="/")
  
  # Set up directories, and get their paths
  paths <- eprime_setup_dirs(path)
  
  # Set up logs, and get their paths
  logs <- eprime_setup_logs(path)
  
  errorlines <- logs$errorlines
  loglines <- logs$loglines
  logs <- logs$logs
  
  eprime_find(in_dir = in_dir, data_dir = data_dir,
              out_dir = out_dir, task = task)
  
  #======= PROCESS =======
  #---read eprime / etxt files in copy dir
  etxts = list.files(paths$path_etxt, pattern = "\\.txt$")
  edats = list.files(paths$path_edat)
  #etxts = rev(etxts) #reverse inputs
  etxts = sample(etxts) #randomise inputs
  
  for (i in 3:length(etxts)) {
    eprime_process_file(efile = etxts[i], MOAS, task, 
                        paths, logs,
                        errorlines, loglines)
  }
  
  cat("\n\nFINISHED\nsee", logs$sessionlog, "for full output\n\n\n(ignore all 'closing' and 'In data.frame(header, df_data)' warning messages)\n")
  

  #-------------get googlecalendar data-------------
  ### IDEALLY MATCHES SHOULD BE MADE WITH GOOGLE CALENDAR TIMES AS WELL
  ### CURRENTLY ONLY READING 250 events - awaiting resolution ###
  #https://github.com/jdeboer/gcalendar/issues/4
  
  #devtools::install_github("jdeboer/gcalendar")
  
  # creds <- GoogleApiCreds(
  #   userName = "james.roe17@gmail.com", 
  #   appCreds = "/Users/jamesroe/Dropbox/Programming/API/client_id.json" # Location of the JSON file containing your
  #   # Google APIs project OAuth client ID and
  #   # secret. 
  # )
  # 
  # cal_list <- gCalendarLists$new(creds = creds)
  # calendars <- cal_list$summary[c("id", "summary", "description")]
  # print(calendars)
  # 
  # testing_calendar <- gCalendar$new(
  #   creds = creds,
  #   id = "80opa8h5fncd86moj3d4ohpke4@group.calendar.google.com"
  # )
  # 
  # testing_events <- testing_calendar$events
  # 
  # testing <- testing_events$summary[c("summary", "start", "htmlLink")]
  # print(testing)
  # 
  # date="2013-08-12"
  # 
  # calmatch = testing[grep(date, testing$start), ]
  # out = calmatch[,2:3]
  ###
  
  
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("etxt", "toc",
                                                        "CrossProject_ID",
                                                        "Subject_Timepoint",
                                                        "Project_Wave",
                                                        "Project_Name",
                                                        "Project_Wave_ID",
                                                        "Test_Date",
                                                        "Age",
                                                        "Sex",
                                                        "Birth_Date",
                                                        "timecheck"))
