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
"+.spiketimes" <- function(e1,e2) {
	cc=c(e1,e2)
	mostattributes(cc) <- attributes(e1)
	cc	
}

is.spiketimes<-function (x) {
	inherits(x,'spiketimes')
}

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
as.repeatedTrain.default<-function(x,...) {
  require(STAR)
  STAR::as.repeatedTrain(x)
}
