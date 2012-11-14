#' spiketimes class to store times of spikes with stimulus information
#'  
#' spiketimes objects consist of a list of dataframes reporting the time of spikes.
#' the data frames have two core columns Time and Wave. Time is the time in ms 
#' at which each spike occurred within the current sweep. Wave is the 0-indexed
#' number of the sweep within the pxp file (Igor convention). Every Wave must have
#' an entry, so if there are no spikes in e.g. Wave 2, an entry of
#' (Time=NA,Wave=2) will be required.
#' 
#' Separate dataframes
#' can be combined into a list where each dataframe is one block of waves that is
#' repeated within a single pxp file or compatible blocks from multiple pxp
#' files. Use \code{\link{split.spiketimes}} to split a spiketimes object loaded
#' from a pxp file with repeated blocks and \code{\link{+.spiketimes}} to combine
#' compatible blocks.
#' @name spiketimes
#' @family spiketimes
NULL

#' Split a spiketimes object with multiple repeats into list with one entry per repeat
#' 
#' Only works for spiketimes from single pxp file
#' FIXME Teach this to cope with repeat blocks with different odours in different order
#' @param st The spiketimes object
#' @param blocksize number of waves per block - deduced from odour names if missing
#' @return new spiketimes object (list) with one entry for each block of odours
#' @author jefferis
#' @method split spiketimes
#' @export
#' @family spiketimes
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

#' Combine multiple compatible spiketimes series (to plot as single raster)
#'
#' Details: this is designed for the case where you have loaded in multiple
#' pxp files into separate spiketimes lists, BUT the pxp files were all acquired
#' with the same ODD config (and presumably Igor protocol). It does not matter
#' if they have the same or a different number of repeats (ie individual data frames),
#' but they must have the same number of rows in the odd config.
#' Example use: Igor crashed and you ended up with one recording split into 
#' 2 cell folders XXXc0 and XXXc1. Use CollectSpikesFromSweeps to read spike
#' files from both folders independently and then join them together with this
#' function.
#' @param e1,e2 spiketimes objects 
#' @return a new spiketimes objects
#' @author jefferis
#' @rdname plus-spiketimes
#' @method + spiketimes
#' @export
#' @family spiketimes
"+.spiketimes" <- function(e1,e2) {
	cc=c(e1,e2)
	oddconf1=attr(e1,'oddconf')
	oddconf2=attr(e2,'oddconf')
	if(!is.null(oddconf1)){
		# the first one has an odd config
		if(is.null(oddconf2)) stop("Both spikeimes lists must have oddconf attributes")
		if(!isTRUE(all.equal(oddconf1,oddconf2))) stop("odd configs do not match")
	}
	mostattributes(cc) <- attributes(e1)
	cc
}

#' Extract one or more spiketimes objects as a new spiketimes list
#' @param x spiketimes objec to subset
#' @param i indices to uss in subsetting
#' @param ... additional arguments (currently ignored)
#' @export
#' @rdname open-brace-spiketimes
#' @family spiketimes
#' @method [ spiketimes
"[.spiketimes" <- function(x,i,...) {
	st=structure(NextMethod("["), class = class(x))
	mostattributes(st) <- attributes(x)
	names(st)=names(x)[i]
	attr(st,'sweeps')=names(st)
	st
}

#' Merge two spiketimes lists with different ODD configs
#'
#' FIXME - figure out how to handle different numbers of repeats more gracefully
#' @param x,y spiketimes objects to merge
#' @param ... additional params (currently ignored)
#' @return new spiketimes object
#' @method merge spiketimes
#' @family spiketimes
#' @export
#' @seealso \code{\link{+.spiketimes}} for combining repeats from the same ODD config
merge.spiketimes<-function(x,y,...){
	if(!is.spiketimes(y)) stop("Can only merge two spiketimes objects")
	if(length(x)!=length(y)) stop("Can only merge spiketimes objects with the same number of repeats")
	maxlen=max(length(x),length(y))
	
	l=list()
	# merge spike data frames
	for(i in seq(maxlen)){
		a=if(i<=length(x)) x[[i]] else NULL
		b=if(i<=length(y)) y[[i]] else NULL
		if(!is.null(b)){
			b$Wave=max(a$Wave,0,na.rm=T)+1+b$Wave
		}
		ab=rbind(a,b)
		l[[i]]=ab
	}
	
	# bring over names etc
	attributes_to_copy=if(length(x)>=length(y)) attributes(x) else attributes(y)
	mostattributes(l)=attributes_to_copy
	
	# merge odd configs
	attr(l,'oddconf')=rbind(attr(x,'oddconf'),attr(y,'oddconf'))
	l
}

#' Make a new spiketimes object containing only sweeps for an odour subset
#' 
#' NB the sweeps will be in the specified odour/channel order. If an unnamed
#' second parameter is specified it will be interpreted as vector of odours
#' if character or channel ids if numeric. 
#' NB If an odour name is duplicated in the oddconfig (e.g. ctr or oil, often)
#' then it is not possible to use them in a subset expression and the channel
#' number must be used instead.
#' @param x The old spiketimes object
#' @param odours A character vector of odours
#' @param channels Integer vector of channels
#' @param ...	further arguments to be passed to or from other methods.
#' @return spiketimes object restricted to specified odours or channels
#' @method subset spiketimes
#' @author jefferis
#' @family spiketimes
#' @export
subset.spiketimes<-function(x,odours=NULL,channels=NULL,...){
  oddconf=attr(x,'oddconf')
  # Note that waves come in 0-indexed from Igor so we'll do the same
  oddconf$OldWave=seq_len(nrow(oddconf))-1
  if(is.null(channels) && is.numeric(odours)) {channels=odours;odours=NULL}
  if(!is.null(odours)){
    if(any(duplicated(odours)))
      stop("Cannot handle duplicated odours")
    if(any(duplicated(oddconf$odour))){
      dupodours<-unique(oddconf$odour[duplicated(oddconf$odour)])
      if(any(odours%in%dupodours))
        stop("Cannot subset using odours that are duplicated in ODD config (",
            paste(dupodours,collapse=" "),")")
    }
    rownames(oddconf)=make.unique(as.character(oddconf$odour))
    newoddconf=oddconf[odours,]
  } else {
    if(is.null(channels))
      stop("Must supply either odours or channels")
    if(any(duplicated(channels)))
      stop("Cannot handle duplicated channels")
    if(any(duplicated(oddconf$chan)))
      stop("Cannot handle duplicated channels in ODD config")
    if(!all(channels%in%oddconf$chan)){
      channels=intersect(channels,oddconf$chan)
      if(length(channels))
        warning("Dropping channels that are not present in odd config")
      else {
        message("This is the ODD config:")
        print(oddconf)
        stop("None of the channels that you gave me are in this ODD config")
      }
    }
    
    rownames(oddconf)=as.character(oddconf$chan)
    newoddconf=oddconf[as.character(channels),]
  }
  # Note that waves come in 0-indexed from Igor so we'll do the same
  newoddconf$NewWave=seq_len(nrow(newoddconf))-1
  # reset the rownames to numbers - this is how they arrive
  rownames(newoddconf)=newoddconf$NewWave
  
  # Now setup to replace old wave numbers with new wave numbers
  new_waves=newoddconf$NewWave
  sel_oldwaves=newoddconf$OldWave
  newspikes=lapply(x,function(x) {
        x$OldWave=x$Wave;
        y=data.frame(Time=numeric(0), Wave=integer(0),OldWave=integer(0))
        for(i in seq_along(new_waves)){
          ssx=subset(x,Wave==sel_oldwaves[i],c(Time,Wave,OldWave))
          if(nrow(ssx)>0){
            ssx$Wave=new_waves[i]
            y=rbind(y,ssx) 
          } else {
            y=rbind(y,data.frame(Time=NA,Wave=new_waves[i],OldWave=sel_oldwaves[i]))
          }
          y=rbind(y,cbind(Time=NA,Wave=NA,OldWave=NA))
        }
        y
      })
  mostattributes(newspikes)=attributes(x)
  attr(newspikes,'oddconf')=newoddconf
  newspikes
}

#' Test if object is of class spiketimes
#' @param x Object to test
#' @return Logical indicating whether object is of class spiketimes
#' @family spiketimes
#' @export
is.spiketimes<-function (x) {
	inherits(x,'spiketimes')
}

#' Convert list of dataframes to spiketimes object
#' 
#' \code{\link{CollectSpikesFromSweeps}} will do this for you, so this only
#' needed if you are constructing \code{\link{spiketimes}} objects from scratch.
#' @param x object to convert to spiketimes list (normally already a list(
#' @param xlim recording time window for each sweep
#' @param stimRange time window over which stimulus was delivered
#' @return spiketimes object
#' @export
#' @family spiketimes
as.spiketimes<-function (x,xlim,stimRange) {
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

#' Generic function to convert spikes to STAR as.repeatedTrain objects
#' @param x Object containing spikes to convert
#' @return repeatedTrain object (list of numeric vectors)
#' @param ...	further arguments to be passed to or from other methods.
#' @export
as.repeatedTrain<-function(x,...){
  UseMethod("as.repeatedTrain")
}

#' Convert gphys spiketimes object to STAR repeatedTrain
#'
#' The STAR (Spike Train Analysis with R) package has a large number of useful
#' functions for e.g. PSTH analysis.
#' Note that the spiketimes objects are a list of data frames, where each list 
#' element will correspond to one pxp file and will have trials for different 
#' odours. We now want to turn this into a list of repeatedTrain objects, 
#' one for each odour.
#' @param x Object to convert (list of dataframes)
#' @inheritParams as.repeatedTrain
#' @export
#' @rdname as.repeatedTrain
#' @method as.repeatedTrain spiketimes
#' @aliases as.repeatedTrain
#' @seealso \code{\link[STAR]{as.repeatedTrain}}
#' @examples
#' spikes<-CollectSpikesFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110914c4",
#' subdir='Block I',sweeps=0:4)
#' rt=as.repeatedTrain(spikes)
#' rt
#' require(STAR)
#' psth(rt[['PAA']])
as.repeatedTrain.spiketimes<-function(x,...){
  # number of sweeps for each odour
  nsweeps=max(x[[1]]$Wave,na.rm=T)
  # TODO handle repeated block that Shahar uses
  nblocks=length(x)
  
  l=list()
  oddconf=attr(x,'oddconf')
  nn=oddconf$odour
  if(is.null(nn)) nn=as.character(seq(nsweeps))
  for(i in seq_along(nn)){
    # nb waves are 0 indexed in nclamp and time unit is ms not s
    spikelist=lapply(x,function(s) subset(s,Wave==(i-1),Time)[[1]]/1000)
    # remove any NAs (converting those trains to empty numeric vectors)
    spikelist=lapply(spikelist,na.omit)
    l[[nn[i]]]=as.repeatedTrain(spikelist)
  }
  l
}

#' @export 
#' @method as.repeatedTrain default
#' @importFrom STAR as.repeatedTrain
as.repeatedTrain.default<-function(x,...) {
  STAR::as.repeatedTrain(x)
}
