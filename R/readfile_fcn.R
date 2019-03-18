readfile_fcn = function(fileread) {
  
  encodetype = readr::guess_encoding(fileread, n_max = 1000)[1,1]
  
  if (encodetype == "ASCII") {
    
    tmpread = readLines(fileread, n = 30)
    tmp1 = tmpread[grep('Subject', tmpread)]
    tmp2 = tmpread[grep('SessionDate', tmpread)]
    tmp3 = tmpread[grep('SessionTime', tmpread)]
    
    tmpstr = strsplit(tmp2,": ")[[1]][2]
    tmpstr = strsplit(tmpstr,"-")
    tmpstr1 = tmpstr[[1]][3]
    tmpstr2 = tmpstr[[1]][1]
    tmpstr3 = tmpstr[[1]][2]
    
    subcheck = strsplit(tmp1,": ")[[1]][2] #sub
    datecheck = paste(tmpstr1,tmpstr2,tmpstr3,sep="-") #date
    timecheck = strsplit(tmp3,": ")[[1]][2] #time
    
    filestamp = list(subcheck,datecheck,timecheck,encodetype)

  } else if (encodetype == "UTF-16LE") { 
    
    tmpread = scan(file(fileread, encoding = "UTF-16LE"), what = character(), nlines=30) 
    
    tmpstr = strsplit(tmpread[grep('SessionDate', tmpread) + 1], "-")
    tmpstr1 = tmpstr[[1]][3]
    tmpstr2 = tmpstr[[1]][1]
    tmpstr3 = tmpstr[[1]][2]
    
    subcheck = tmpread[grep('Subject', tmpread) + 1] #sub
    datecheck = paste(tmpstr1,tmpstr2,tmpstr3,sep="-") #date
    timecheck = tmpread[grep('SessionTime', tmpread) + 1] #time
    
    filestamp = list(subcheck,datecheck,timecheck,encodetype)
  }
  return(filestamp)
  
}
