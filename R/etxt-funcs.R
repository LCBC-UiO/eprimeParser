#' @importFrom readr guess_encoding
etxt_fcn <- function(efile_path) {
  
  encodetype <- readr::guess_encoding(efile_path, n_max = 1000)[1,1]
  
  if (encodetype == "ASCII") {
    
    tmpread <- readLines(efile_path, n = 30)
    tmp1 <- tmpread[grep('Subject', tmpread)]
    tmp2 <- tmpread[grep('SessionDate', tmpread)]
    tmp3 <- tmpread[grep('SessionTime', tmpread)]
    
    tmpstr <- strsplit(tmp2,": ")[[1]][2]
    tmpstr <- strsplit(tmpstr,"-")
    
    subcheck <- strsplit(tmp1,": ")[[1]][2] #sub
    datecheck <- paste(tmpstr[[1]][3], tmpstr[[1]][1], tmpstr[[1]][2],sep="-") #date
    timecheck <- strsplit(tmp3,": ")[[1]][2]
    
  } else if (encodetype == "UTF-16LE") { 
    
    tmpread <- scan(file(efile_path, encoding = "UTF-16LE"), what = character(), nlines=30) 
    
    tmpstr <- strsplit(tmpread[grep('SessionDate', tmpread) + 1], "-")
    
    subcheck <- tmpread[grep('Subject', tmpread) + 1] #sub
    datecheck <- paste( tmpstr[[1]][3], tmpstr[[1]][1],tmpstr[[1]][2], sep="-") #date 
    timecheck <- tmpread[grep('SessionTime', tmpread) + 1]
  }
  
  list(subcheck = subcheck, #sub
       datecheck = datecheck, #date
       timecheck = timecheck, #time
       encodetype = encodetype)
  
}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("subcheck", "datecheck",
                                                        "timecheck"))
