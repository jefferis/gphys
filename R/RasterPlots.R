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
#' @param ... Additional parameters passed to plot 
#' @author jefferis
#' @seealso CollectSpikesFromSweeps
#' @export
#' @examples
#' \dontrun{ 
#' ## Plot time range 2-4s with odour pulse 2-3s for sweeps 0,1,3
#' PlotRasterFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110811c0",
#'   c(0,1,3),xlim=c(2000,4000),odourRange=c(2000,3000))
#' ## Fix a bad label, first define a function 
#' relabel=function(labels) {labels[labels=="fly"]="empty";labels}
#' ## and then use it
#' PlotRasterFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110811c0",c(0,1,3),relabelfun=relabel)
#' }
PlotRasterFromSweeps<-function(sweepdir,sweeps,xlim=c(0,5000),
  main,sub,xlab='Time/ms', ylab='Odour',
  dotcolour='black',dotsize=0.5,
  odourRange=c(2000,2500),odourCol=rgb(1,0,0,alpha=.3),
  relabelfun=identity,IncludeChannels=FALSE,...){
  if(inherits(sweepdir,'spiketimes'))
    rasterd=sweepdir
  else
    rasterd=CollectSpikesFromSweeps(sweepdir,sweeps)
  last_wave=max(sapply(rasterd,function(x) max(x$Wave,na.rm=TRUE)))
  plot(NA,xlim=xlim,ylim=c(last_wave+1,0),ylab=ylab,xlab=xlab,axes=F,...)

  labels=relabelfun(attr(rasterd,'oddconf')$odour)
  if(IncludeChannels) labels=paste(labels,attr(rasterd,'oddconf')$chan)
	if(length(labels)>(last_wave+1))
	{
		warning("Dropping ", length(labels)-last_wave-1, " labels from odd config")
		labels = labels[seq(last_wave+1)]
	}
  axis(side=2,at=seq(last_wave+1)-0.5,labels=labels,tick=F,las=1)
  axis(1)
  nreps=length(rasterd)
  for(i in seq(rasterd)){
    yoff=i/(nreps+1)
    df=rasterd[[i]]
    points(x=df$Time,y=df$Wave+yoff,pch=22,col=NA,bg=dotcolour,cex=dotsize)
  }
  rect(odourRange[1],-0.5,odourRange[2],last_wave+1.5,col=odourCol,border=NA)
  # plot boxes around each odour set
  for(i in seq(last_wave+1)){
    rect(0,i-1,max(xlim),i,col=NA,border='black')
  }
  if(missing(main)) main=""
  if(missing(sub)) sub=paste("Cell:",basename(attr(rasterd,'sweepdir')),
    "sweeps:",paste(attr(rasterd,'sweeps'),collapse=","))
  title(main=main,sub=sub)
}

#' Read in Igor Pro exported text file of Nclamp spike times
#'
#' The list of spiketimes has two columns, Time and Wave, where wave is 
#' the number of the wave within each sweepfile containing the spike.
#' @param sweepdir directory containing Nclamp pxp sweep files
#' @param sweeps Vector of sweeps to include (e.g. 1:7) or character regex which
#'               sweeps must match.
#' @return list (with class spiketimes) containing times for each sweep
#' @author jefferis
#' @export
#' @examples
#' \dontrun{
#' spikes=CollectSpikesFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110811c0",c(0,1,3))
#' PlotRasterFromSweeps(spikes,xlim=c(2000,4000),odourRange=c(2000,3000))
#' }
CollectSpikesFromSweeps<-function(sweepdir,sweeps,xlim,stimRange){
  require(tools)
  fi=file.info(sweepdir)
  if(is.na(fi$isdir) || !fi$isdir)
    stop("Cannot read directory",sweepdir)
  # Read in all spike times
  ff=dir(sweepdir,'^[0-9]{3}_SP_',full=TRUE)
  rasterd=lapply(ff,read.table,col.names=c("Time","Wave"),header=TRUE, 
    na.strings='NAN')
  names(rasterd)=substring(basename(ff),1,3)
  
  oddfiles=dir(sweepdir,patt='_odd[_.]')
  names(oddfiles)=sub('.*_([0-9]{3})_odd.*','\\1',oddfiles)
  if(missing(sweeps)){
    sweeps=names(rasterd)
  } else {
    if(is.character(sweeps)){
      # assume we have been given a regex pattern to match against the odd 
      # config files saved in the current data directory.
      
      oddfiles=oddfiles[grepl(sweeps,oddfiles)]
      if(length(oddfiles)==0) 
        stop("No ODD config files match regex: ",sweeps)
      # Now extract the numeric ids of the relevant sweeps
      sweeps=names(oddfiles)      
    } else {
      # we've been given a numeric vector 1:2->c("001","002")
      sweeps=sprintf("%03d",sweeps)
      if(!all(sweeps%in%names(oddfiles))) 
        stop("Cannot find ODD config files for some sweeps: ",sweeps)
      oddfiles=oddfiles[sweeps]
    }
    
    if(all(sweeps%in%names(rasterd)))
      rasterd=rasterd[sweeps]
    else
      stop("Missing spike counts for sweeps: ",setdiff(sweeps,names(rasterd)))
  } 

  # read in ODD protocol
  oddfiles=file.path(sweepdir,oddfiles)
  oddconf=read.odd(oddfiles[1])
  md5s=md5sum(oddfiles)
  if(length(unique(md5s))>1) stop("I don't yet know how to handle different odd config files")
  
  attr(rasterd,'oddconf')=oddconf
  attr(rasterd,'sweeps')=sweeps
  attr(rasterd,'sweepdir')=sweepdir
	if(!missing(stimRange))
  	attr(rasterd,'stimRange')=stimRange
	if(!missing(xlim))
  	attr(rasterd,'xlim')=xlim
	
  class(rasterd)=c('spiketimes',class(rasterd))
  rasterd
}

#' Boxplot of spikes within a window (optionally less a baseline)
#' @param spiketimes list of spiketimes collected by CollectSpikesFromSweeps
#' @param responseWindow vector of start and end time of odour response (in ms)
#' @param baselineWindow vector of start and end time of baseline period (in ms)
#' @param PlotFrequency Plot spike rate in Hz rather than counts (default FALSE)
#' @param PLOTFUN stripchart, boxplot or similar function (default stripchart)
#' @param ... 
#' @return results of plotfun (if any)
#' @author jefferis
#' @export
#' @seealso CollectSpikesFromSweeps
#' @examples
#' \dontrun{ 
#' spikes=CollectSpikesFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110914c4",
#'  sweeps=0:4)
#' ## stripchart
#' PlotOdourResponseFromSpikes(spikes,c(2200,2700),c(0,2000),pch=19,method='jitter',
#'  col=1:6)
#' ## boxplot, in Hz
#' PlotOdourResponseFromSpikes(spikes,c(2200,2700),c(0,2000),PlotFrequency=TRUE,
#'  PLOTFUN=boxplot)
#' }
PlotOdourResponseFromSpikes<-function(spiketimes,responseWindow,baselineWindow,
  PlotFrequency=FALSE,PLOTFUN=stripchart,...){
  nreps=length(spiketimes)
  last_wave=max(sapply(spiketimes,function(x) max(x$Wave,na.rm=TRUE)))
  # plot(NA,xlim=xlim,ylim=c(last_sweep+1,0),ylab=ylab,xlab=xlab,axes=F,...)
  # Want to collect a table which has rows for each odour
  # and re
  spikess=do.call(rbind,spiketimes)
  spikess$Sweep=rep(names(spiketimes),sapply(spiketimes,nrow))

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

#' Split a spiketimes object with multiple repeats into list with one entry per repeat
#' 
#' Only works for spiketimes from single pxp file
#' FIXME Teach this to cope with repeat blocks with different odours in different order
#' @param st The spiketimes object
#' @param blocksize number of waves per block - deduced from odour names if missing
#' @return new spiketimes object (list) with one entry for each block of odours
#' @author jefferis
#' @export
split.spiketimes<-function(st,blocksize){
	if(length(st)>1) stop("Don't know how to split a spike time list of > length 1")
	st1=st[[1]]
	st1$OldWave=st1$Wave
	odd=attr(st,'oddconf')
	if(missing(blocksize))
		blocksize=length(unique(odd$odour))
	# Find 0 indexed repetition number
	st1$Rep=floor(st1$OldWave/blocksize)
	st1$Wave=st1$OldWave%%blocksize
	
	stnew=split(st1,st1$Rep)
	mostattributes(stnew)=attributes(st)
	if(!is.null(attr(stnew,'oddconf'))){
		attr(stnew,'oddconf')=attr(stnew,'oddconf')[1:blocksize,]
		
	}
	stnew
}


#' Combine multiple spiketimes series together (to plot in single rater figure)
#' @param ... spiketimes objects 
#' @return a new spiketimes objects
#' @author jefferis
#' @export
"+.spiketimes" <- function(...) {
	cc=c(...)
	mostattributes(cc) <- attributes(list(...)[[1]])
	cc	
}

is.spiketimes<-function (x) {
	inherits(x,'spiketimes')
}

as.spiketimes<-function (x,xlim) {
	if(!is.spiketimes(x)){
		class(x)=c('spiketimes',class(x))
	}
	if(!missing(stimRange)) 
		attr(x,'stimRange')=stimRange
	x
	if(!missing(xlim)) 
		attr(x,'xlim')=xlim
	x
}
