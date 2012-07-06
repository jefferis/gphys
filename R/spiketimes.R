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

#' Combine multiple spiketimes series together (to plot in single raster figure)
#' @param e1,e2 spiketimes objects 
#' @return a new spiketimes objects
#' @author jefferis
#' @rdname plus-spiketimes
#' @method + spiketimes
#' @export
#' @family spiketimes
"+.spiketimes" <- function(e1,e2) {
	cc=c(e1,e2)
	mostattributes(cc) <- attributes(e1)
	cc	
}

#' Make a new spiketimes object containing only sweeps for an odour subset
#' 
#' NB the sweeps will be in the specified odour/channel order. If an unnamed
#' second parameter is specified it will be interpreted as vector of odours
#' if character or channel ids if numeric.
#' @param x The old spiketimes object
#' @param odours A character vector of odours
#' @param channels Integer vector of channels
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
    if(any(duplicated(oddconf$odours)))
      stop("Cannot handle duplicated odours in ODD config")
    rownames(oddconf)=as.character(oddconf$odour)
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
#' @param x object (list of dataframes)
#' @return repeatedTrain object (list of numeric vectors)
#' @export
#' @method as.repeatedTrain spiketimes
#' @seealso \code{\link[STAR]{as.repeatedTrain}}
#' @examples
#' spikes<-CollectSpikesFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110914c4",
#' subdir='Block I',sweeps=0:4)
#' rt=as.repeatedTrain(spikes)
#' rt
#' psth(rt[['PAA']])
as.repeatedTrain.spiketimes<-function(x,...){
  # number of sweeps for each odour
  nsweeps=max(x[[1]]$Wave)
  # TODO handle repeated block that Shahar uses
  nblocks=length(x)
  
  l=list()
  oddconf=attr(x,'oddconf')
  nn=oddconf$odour
  if(is.null(nn)) nn=as.character(seq(nsweeps))
  for(i in seq_along(nn)){
    # nb waves are 0 indexed in nclamp and time unit is ms not s
    l[[nn[i]]]=as.repeatedTrain(lapply(x,function(s) subset(s,Wave==(i-1),Time)[[1]]/1000))
  }
  l
}

#' @export 
#' @method as.repeatedTrain default
#' @importFrom STAR as.repeatedTrain
as.repeatedTrain.default<-function(x,...) {
  STAR::as.repeatedTrain(x)
}
