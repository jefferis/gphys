# Functions that wrap tools from the STAR package
# 
# Author: jefferis
###############################################################################

#' Convert a STAR psth into a (multi) time series object
#' @param x STAR psth
#' @param fieldsToConvert name of fields to convert (freq, ciUp, ciLow)
#' @param ... ignored
#' @method as.ts psth
#' @family psth
#' @export
as.ts.psth<-function(x,fieldsToConvert='freq',...){
  xtsp=tsp.psth(x)
  data = if (length(fieldsToConvert)>1) do.call(cbind,x[fieldsToConvert])
      else x[[fieldsToConvert]]
  ts(data, xtsp[1L], xtsp[2L], xtsp[3L])
}

#' Find start, end and frequency of a STAR psth object
#' Checks that it is regularly sampled
#' @param x STAR psth object
#' @return numeric vector with start, end and frequency
#' @export 
#' @family psth
#' @seealso tsp
tsp.psth<-function(x){
  deltat=unique(signif(diff(x$mids),6))
  if(length(deltat)>1) stop("Unable to identify unique frequency for psth")
  c(start=x$mids[1],end=x$mids[length(x$mids)],freq=1/deltat)
}

#' Generate smooth PSTHs for a set of spike trains
#' @param spikes 
#' @param breaks 
#' @param CI 
#' @param stimRange 
#' @param stimCol 
#' @param ... 
#' @return list of psth objects
#' @author jefferis
#' @export
#' @seealso STAR::psth, STAR::plot.psth
#' @examples
#' spikes=CollectSpikesFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20120917c1",
#'   subdir="BLOCK A",stimRange=c(2000,2500))
#' smpsth(spikes)
smpsth<-function(spikes, breaks=c(bw=0.5,step=0.05), CI=0.95,
    stimRange=NULL, stimCol, ...){
  # Generate a smooth psth using the STAR package
  if(is.repeatedTrain(spikes)) rt=spikes
  else rt=as.repeatedTrain(spikes)
  if(!is.null(stimRange) && is.spiketimes(spikes) && !is.null(attr(spikes,'stimRange'))){
    # nb these come in in ms but we are using seconds with STAR
    stimRange=attr(spikes,'stimRange')/1000
  }
  x=lapply(rt,psth,breaks=breaks,CI=CI,plot=FALSE)
  if(!is.null(stimRange)) attr(x,'stimTimeCourse')=stimRange
  class(x)=c("mpsth",class(x))
  x
}

#' Convert a list of STAR psth objects into an mpsth object
#' @param x list of psth objects
#' @param ... ignored
#' @return list of class mpsth
#' @author jefferis
#' @export
#' @family mpsth
#' @seealso STAR::psth
as.mpsth<-function(x,...){
  if(is(x,'mpsth')) return(x)
  
  inherits_psth=sapply(x,inherits,'psth')
  if(!all(inherits_psth)) stop("Not all members of list have class psth")
  class(x)=c("mpsth",class(x))
  x
}

#' Plot PSTHs for multiple neurons (or multiple stimuli)
#' @param x a list of psth objects (class mpsth)
#' @param y ignored
#' @param ... additional params passed to plot methods
#' @author jefferis
#' @method plot mpsth
#' @export
#' @seealso STAR::psth, STAR::plot.psth
plot.mpsth<-function(x,stimTimeCourse = NULL, colStim = "grey80",
    colCI = NULL, xlab='Time /s', ylab='Spike Freq /Hz', xlim, ylim, lwd = 2,
    col = 1,labels=NULL,toplot=c("lines",'CI','legend'),...){
  # if there is only one psth in the list, use standard plot method
  if(length(x)==1) return(plot(x[[1]],y=y,...))
  maxFreq <- max(sapply(x,"[[",'ciUp'))
  if(missing(xlim))
    xlim=range(sapply(x,function(xx) tsp.psth(xx)[1:2]))
  if(missing(ylim))
    ylim=c(0,maxFreq)
  plot(xlim,ylim,type="n",xlab=xlab,ylab=ylab,...)
  if(is.null(stimTimeCourse)){
    if(!is.null(stc<-attr(x,'stimTimeCourse'))) stimTimeCourse=stc
  }
  if(!is.null(stimTimeCourse))
    rect(stimTimeCourse[1],0,stimTimeCourse[2],maxFreq,col=colStim,lty=0)
  if(is.function(col)) col=col(length(x))
  else if(length(x)>1 && length(col)==1) col=rep(col,length(x)) 
  if(is.null(colCI)){
    colCI=col2rgb(col,alpha=TRUE)
    # use same color as line but with alpha value 50% of original
    colCI=rgb(t(colCI[-4,]),alpha=colCI['alpha',]/10,maxColorValue=255)
  } else if(length(colCI)==1 && length(x)>1) {
    colCI=rep(colCI,length(x))
  }
  for(i in seq_along(x)){
    p=x[[i]]
    if('lines'%in%toplot)
      lines(p$mids,p$freq,col=col[i],lwd=lwd)
    if('CI'%in%toplot)
      polygon(c(p$mids,rev(p$mids)),c(p$ciLow,rev(p$ciUp)),col=colCI[i],border=NA)
  }
  if(is.null(labels)){
    if(!is.null(nx<-names(x))) labels=nx
    else {
      labels=paste('train',seq_along(x))
    }
  }
  if('legend'%in%toplot)
    legend(xlim[1],ylim[2],legend=labels,lty=1,col=col,bty="n")
}

#' Convert a multi-psth list into a (multi) time series object
#' @param x multi-psth list
#' @param ... ignored
#' @return time series or multi-time series object 
#' @author jefferis
#' @method as.ts mpsth
#' @export
#' @family mpsth
#' @seealso ts as.ts.mpsth STAR::psth
#' @examples
#' data(CAL1V,package='STAR')
#' CAL1V <- lapply(CAL1V,as.repeatedTrain)
#' psth1 <- psth(CAL1V[["neuron 1"]],breaks=c(bw=0.5,step=0.05),plot=FALSE)
#' psth2 <- psth(CAL1V[["neuron 2"]],breaks=c(bw=1,step=0.1),plot=FALSE)
#' z=as.ts(as.mpsth(list(psth1,psth2)))
#' plot(z,plot.type='single',col=1:2,ylab='Frequency /Hz')
as.ts.mpsth<-function(x,...){
  # convert all series to time series
  l=lapply(x,as.ts)
  if(length(x)==1L) return(l)
  # else we need to merge the difference time series taking care of start/end times
  
  # find the minimum start, maximum end and highest frequency for inputs
  tsps=sapply(l,tsp)
  minstart=min(tsps[1,])
  maxend=max(tsps[2,])
  # seems like upsampling is not possible so have to go with minimum frequency
  minfreq=min(tsps[3,])
  # now convert all time series to that basis
  ll=lapply(l,window,minstart,maxend,minfreq,extend=TRUE)
  # ... and cbind them together now that they are compatible
  names(ll)=names(x)
  do.call(cbind,ll)
}

#' Extract one or more psth objects to make a new mpsth list
#' @param x mpsth objec to subset
#' @param i indices to use in subsetting
#' @param ... additional arguments (currently ignored)
#' @export
#' @rdname open-brace-psth
#' @family mpsth
#' @method [ mpsth
"[.mpsth" <- function(x,i,...) {
  st=structure(NextMethod("["), class = class(x))
  mostattributes(st) <- attributes(x)
  names(st)=names(x)[i]
  st
}
