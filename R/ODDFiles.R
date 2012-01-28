# Routines to handle parsing of ODD config files
# 
# Author: jefferis
###############################################################################

#' Read in the ODD text file describing odour presentation sequence
#' 
#' @param oddfile Path to ODD file
#' @return dataframe containing a list of odours / presentation times
#' @author jefferis
#' @export
read.odd<-function(oddfile){
  ll=readLines(oddfile,warn=FALSE)
  tablelines=character(0)
  for(l in ll){
    # trim trailing white space
    l=sub('[[:space:]]+$', '', l)
    firstChar=substr(l,1,1)
    # skip comment lines, special instructions and empty lines
    if(firstChar%in%c("#","!") || nchar(l)==0) 
      next
    tablelines=c(tablelines,l)
  }
  tc=textConnection(tablelines,'r')
  on.exit(close(tc))
  res<-try(read.table(tc,col.names=make.unique(c('odour',rep(c('del','dur','chan'),5))),
      stringsAsFactors=FALSE))
  if(inherits(res,'try-error')){
    stop("Malformed ODD file: ",oddfile)
  }
  res
}