edat_fcn = function(efile) {
  
  edats = list.files(path_edat)
  
  #change extension to .edat and search
  strcut = substr(efile, 1, nchar(efile) -4)
  strcut_edat = paste0(strcut, ".edat")
  regexp = grep(strcut_edat, edats)
  
  #get true edat
  edatstr = edats[regexp]
  edatnum = as.data.frame(regexp)
  
  edat_info = list(strcut,strcut_edat,regexp,edatstr,edatnum)
  return(edat_info)
}
