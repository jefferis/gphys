#' Query the colour label of a file in the MacOS X Finder
#' 
#' Note the colours are mapped according to my 10.6.8 system but see:
#' http://stackoverflow.com/questions/2435580/tagging-files-with-colors-in-os-x-finder-from-shell-scripts
#' @param filename Path to a file 
#' @return Character string (None when no colour is set) or NA on error 
#' @author jefferis
#' @export
finder_colour<-function(filename){
  if(length(filename)>1) return(mapply(finder_colour,filename))
  if(Sys.info()['sysname']!='Darwin') return(NA)
  cmd2=paste('-raw -name kMDItemFSLabel',shQuote(filename))
  ow=options(warn=-1)
  on.exit(options(ow))
  mdls=system2('mdls',cmd2,stdout=TRUE)
  cols=c("None","Gray","Green","Purple","Blue","Yellow","Red","Orange")
  coln=as.integer(mdls)
  if(!is.na(coln) && length(coln)>0 && coln>=0 && coln<=7) cols[coln+1]
  else NA
}

#' Set the colour label of a file in the MacOS X Finder 
#' See http://stackoverflow.com/questions/2435580/tagging-files-with-colors-in-os-x-finder-from-shell-scripts
#' @param filename Character vector of file(s) to add colour labels 
#' @param col Character vector of colour(s)
#' @return Logical indicating success or failure
#' @author jefferis
#' @export
set_finder_colour<-function(filename,col=c("None","Gray","Grey","Green","Purple","Blue","Yellow","Red","Orange")){
  if(length(filename)>1) return(mapply(set_finder_colour,filename,col))
  if(Sys.info()['sysname']!='Darwin') return(FALSE)
  if(any(is.na(col)))
    col='None'
  else {
    col=match.arg(col)
    if(col=='Grey') col="Gray"
  }
  cols=c("None","Orange","Red","Yellow","Blue","Purple","Green","Gray")
  coln=which(cols==col)-1
  cmd=paste('osascript -e \'tell application "Finder" to set label index of alias POSIX file "',filename,'" to ',coln,'\'',sep="")
  system(cmd)==0
}
