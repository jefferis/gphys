# Routines to handle parsing of ODD config files
# 
# Author: jefferis
###############################################################################

#' Read in the ODD text file describing odour presentation sequence
#' 
#' @param oddfile Path to ODD file
#' @param stringsAsFactors Whether to convert odours to strings (default) or factors
#' @param fill Whether to fill in missing delay/duration values with NAs
#' @param Verbose Show number of records read (default TRUE)
#' @return dataframe containing a list of odours / presentation times
#' @author jefferis
#' @export
read.odd<-function(oddfile,fill=TRUE,stringsAsFactors=FALSE,Verbose=FALSE){
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
  res<-try(scan(tc, quiet=!Verbose, fill=TRUE,
          what=list('odour',0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
  
  if(inherits(res,'try-error')){
    stop("Malformed ODD file: ",oddfile)
  }
  df=as.data.frame(res,stringsAsFactors=stringsAsFactors)
  colnames(df)=make.unique(c('odour',rep(c('del','dur','chan'),5)))
  df
}