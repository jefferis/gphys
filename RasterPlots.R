#' Make a raster plot from a set of Nclamp sweeps recording odour responses  
#' @param sweepdir directory containing Nclamp pxp sweep files 
#' @param sweeps Vector of sweeps to include (e.g. 1:7)
#' @param xlim x axis range of plot 
#' @param main main title of plot (see \code{\link{title}}) 
#' @param sub subtitle of plot
#' @param xlab axis label (default Time/ms)
#' @param ylab axis label (default odour)
#' @param dotcolour colour of dots in raster plot (default black)
#' @param dotsize size of dots in raster plot (default 0.5) 
#' @param odourRange time window of odour delivery 
#' @param odourCol colour of odour window (default pale red) 
#' @param relabelfun function to apply to odour labels (default no relabelling)
#' @return 
#' @author jefferis
#' @export
#' @example
#' ## Plot time range 2-4s with odour pulse 2-3s for sweeps 0,1,3
#' PlotRasterFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110811c0",
#'   c(0,1,3),xlim=c(2000,4000),odourRange=c(2000,3000))
PlotRasterFromSweeps<-function(sweepdir,sweeps,xlim=c(0,5000),
  main,sub,xlab='Time/ms', ylab='Odour',
  dotcolour='black',dotsize=0.5,
  odourRange=c(2000,2500),odourCol=rgb(1,0,0,alpha=.3),
  relabelfun=identity,...){
  # Read in all spike times
  ff=dir(sweepdir,'^[0-9]{3}_SP_',full=T)
  rasterd=lapply(ff,read.table,col.names=c("Time","Sweep"),header=T, 
    na.strings='NAN')
  names(rasterd)=substring(basename(ff),1,3)
  
  if(missing(sweeps)){
    sweeps=names(rasterd)
  } else {
    sweeps=sprintf("%03d",sweeps)
    rasterd=rasterd[sweeps]
  }

  # read in ODD protocol
  oddfiles=file.path(sweepdir,paste(basename(sweepdir),sweeps,"odd.txt",sep="_"))
  oddconf=read.table(oddfiles[1],col.names=make.unique(c('odour',rep(c('del','dur','chan'),5))))
  md5s=md5sum(oddfiles)
  if(length(unique(md5s))>1) stop("I dont yet know how to handle different odd config files")

  last_sweep=max(sapply(rasterd,function(x) max(x$Sweep,na.rm=T)))
  plot(NA,xlim=xlim,ylim=c(last_sweep+1,0),ylab=ylab,xlab=xlab,axes=F,...)

  axis(side=2,at=seq(last_sweep+1)-0.5,labels=relabelfun(oddconf$odour),tick=F,las=1)
  axis(1)
  nreps=length(rasterd)
  for(i in seq(rasterd)){
    yoff=i/(nreps+1)
    df=rasterd[[i]]
    points(x=df$Time,y=df$Sweep+yoff,pch=22,col=NA,bg=dotcolour,cex=dotsize)
  }
  rect(odourRange[1],-0.5,odourRange[2],last_sweep+1.5,col=odourCol,border=NA)
  for(i in seq(last_sweep+1)){
    rect(0,i-1,5000,i,col=NA,border='black')
  }
  if(missing(main)) main=""
  if(missing(sub)) sub=paste("Cell:",basename(sweepdir),"sweeps:",paste(sweeps,collapse=","))  
  title(main=main,sub=sub)
}
