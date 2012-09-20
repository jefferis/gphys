#' Query the colour label of a file in the MacOS X Finder
#' 
#' Note the colours are mapped according to my 10.6.8 system but see:
#' http://stackoverflow.com/questions/2435580/tagging-files-with-colors-in-os-x-finder-from-shell-scripts
#' @param filename Path to a file 
#' @return Character string (None when no colour is set) or NA on error 
#' @family finder_colour
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

#' Query the colour label of a file in the MacOS X Finder
#' 
#' Note the colours are mapped according to my 10.6.8 system but see:
#' http://stackoverflow.com/questions/2435580/tagging-files-with-colors-in-os-x-finder-from-shell-scripts
#' Drops the file and gives a message if a file does not exist
#' @param files Path to (a) file(s) 
#' @return Named character vector (None when no colour is set)
#' @family finder_colour
#' @author jefferis
#' @export

finder_colour_fast<-function(files){
	if(Sys.info()['sysname']!='Darwin') return(NA)
	ow=options(warn=-1)
	on.exit(options(ow))
	cmd=paste("mdls -raw -name kMDItemFSLabel", paste(shQuote(files), collapse=" "), " | xargs -0 echo")
	cmd=paste("mdls -raw -name kMDItemFSLabel", paste(shQuote(path.expand(existingFiles)), collapse=" "), " | xargs -0 echo")
	mdls=system(cmd,intern=T)
	colors=scan(tc<-textConnection(mdls))
	on.exit(close(tc))
	assignColor<-function(num){
		cols=c("None","Gray","Green","Purple","Blue","Yellow","Red","Orange")
		color=cols[num+1]
		color
	}
	colors<-assignColor(colors)
	names(colors)=basename(files)
	colors
}

#' Set the colour label of a file in the MacOS X Finder 
#' See http://stackoverflow.com/questions/2435580/tagging-files-with-colors-in-os-x-finder-from-shell-scripts
#' @param filename Character vector of file(s) to add colour labels 
#' @param col Character vector of colour(s)
#' @return Logical indicating success or failure
#' @family finder_colour
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
