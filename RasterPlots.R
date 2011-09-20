#' Make a raster plot from a set of Nclamp sweeps recording odour responses
#'
#' Note that can also give a spiketimes list from CollectSpikesFromSweeps
#' @param sweepdir directory containing Nclamp sweep files (or list of spiketimes)
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
#' @param IncludeChannels include numeric id of odour channel (e.g. for blanks)
#' @return 
#' @author jefferis
#' @seealso CollectSpikesFromSweeps
#' @export
#' @example
#' ## Plot time range 2-4s with odour pulse 2-3s for sweeps 0,1,3
#' PlotRasterFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110811c0",
#'   c(0,1,3),xlim=c(2000,4000),odourRange=c(2000,3000))
#' ## Fix a bad label, first define a function 
#' relabel=function(labels) {labels[labels=="fly"]="empty";labels}
#' ## and then use it
#' PlotRasterFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110811c0",c(0,1,3),relabelfun=relabel)
PlotRasterFromSweeps<-function(sweepdir,sweeps,xlim=c(0,5000),
  main,sub,xlab='Time/ms', ylab='Odour',
  dotcolour='black',dotsize=0.5,
  odourRange=c(2000,2500),odourCol=rgb(1,0,0,alpha=.3),
  relabelfun=identity,IncludeChannels=FALSE,...){
  if(inherits(sweepdir,'spiketimes'))
    rasterd=sweepdir
  else
    rasterd=CollectSpikesFromSweeps(sweepdir,sweeps)
  last_wave=max(sapply(rasterd,function(x) max(x$Wave,na.rm=T)))
  plot(NA,xlim=xlim,ylim=c(last_wave+1,0),ylab=ylab,xlab=xlab,axes=F,...)

  labels=relabelfun(attr(rasterd,'oddconf')$odour)
  if(IncludeChannels) labels=paste(labels,attr(rasterd,'oddconf')$chan)
  axis(side=2,at=seq(last_wave+1)-0.5,labels=labels,tick=F,las=1)
  axis(1)
  nreps=length(rasterd)
  for(i in seq(rasterd)){
    yoff=i/(nreps+1)
    df=rasterd[[i]]
    points(x=df$Time,y=df$Wave+yoff,pch=22,col=NA,bg=dotcolour,cex=dotsize)
  }
  rect(odourRange[1],-0.5,odourRange[2],last_wave+1.5,col=odourCol,border=NA)
  for(i in seq(last_wave+1)){
    rect(0,i-1,5000,i,col=NA,border='black')
  }
  if(missing(main)) main=""
  if(missing(sub)) sub=paste("Cell:",basename(attr(rasterd,'sweepdir')),
    "sweeps:",paste(attr(rasterd,'sweeps'),collapse=","))
  title(main=main,sub=sub)
}

#' Make a raster plot from a set of Nclamp sweeps recording odour responses
#'
#' The list of spiketimes has two columns, Time and Wave, where wave is 
#' the number of the wave within each sweepfile containing the spike.
#' @param sweepdir directory containing Nclamp pxp sweep files
#' @param sweeps Vector of sweeps to include (e.g. 1:7)
#' @return list (with class spiketimes) containing times for each sweep
#' @author jefferis
#' @export
#' @example
#' spikes=CollectSpikesFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110811c0",c(0,1,3))
#' PlotRasterFromSweeps(spikes,xlim=c(2000,4000),odourRange=c(2000,3000))
CollectSpikesFromSweeps<-function(sweepdir,sweeps){
  # Read in all spike times
  ff=dir(sweepdir,'^[0-9]{3}_SP_',full=T)
  rasterd=lapply(ff,read.table,col.names=c("Time","Wave"),header=T, 
    na.strings='NAN')
  names(rasterd)=substring(basename(ff),1,3)
  
  if(missing(sweeps)){
    sweeps=names(rasterd)
  } else {
    sweeps=sprintf("%03d",sweeps)
    if(all(sweeps%in%names(rasterd)))
      rasterd=rasterd[sweeps]
    else
      stop("Missing spike counts for sweeps: ",setdiff(sweeps,names(rasterd)))
  }

  # read in ODD protocol
  oddfiles=file.path(sweepdir,paste(basename(sweepdir),sweeps,"odd.txt",sep="_"))
  oddconf=read.table(oddfiles[1],col.names=make.unique(c('odour',rep(c('del','dur','chan'),5))),
    stringsAsFactors=FALSE)
  md5s=md5sum(oddfiles)
  if(length(unique(md5s))>1) stop("I dont yet know how to handle different odd config files")
  
  attr(rasterd,'oddconf')=oddconf
  attr(rasterd,'sweeps')=sweeps
  attr(rasterd,'sweepdir')=sweepdir
  class(rasterd)=c('spiketimes',class(rasterd))
  rasterd
}

#' Boxplot of spikes within a window (optionally less a baseline)
PlotOdourResponseFromSpikes<-function(spiketimes,responseWindow,baselineWindow,
  PlotFrequency=FALSE,PLOTFUN=stripchart,...){
  nreps=length(spiketimes)
  last_wave=max(sapply(spiketimes,function(x) max(x$Wave,na.rm=T)))
  # plot(NA,xlim=xlim,ylim=c(last_sweep+1,0),ylab=ylab,xlab=xlab,axes=F,...)
  # Want to collect a table which has rows for each odour
  # and re
  spikess=do.call(rbind,spiketimes)
  spikess$Sweep=rep(names(spikes),sapply(spikes,nrow))

  responseTime=diff(responseWindow)
  responsecount=by(spikess$Time,
    list(factor(spikess$Sweep),factor(spikess$Wave)),
    function(t) sum(t>responseWindow[1] &t<responseWindow[2]))
  if(!missing(baselineWindow)){
    baselinecount=by(spikess$Time,
      list(factor(spikess$Sweep),factor(spikess$Wave)),
      function(t) sum(t>baselineWindow[1] &t<baselineWindow[2]))
    baselineTime=diff(baselineWindow)
    responsecount=responsecount-baselinecount*responseTime/baselineTime
  }
  bbdf=as.data.frame(matrix(responsecount,ncol=ncol(responsecount)))
  colnames(bbdf)=make.unique(attr(spiketimes,'oddconf')$odour)
  # stack(bbdf)
  if(PlotFrequency) {
    bbdf=bbdf/(responseTime/1000)
    PLOTFUN(bbdf,xlab='Spike Frequency /Hz',las=2,...)
  } else {
    PLOTFUN(bbdf,xlab='Spike Count',las=2,...)
  }
}
