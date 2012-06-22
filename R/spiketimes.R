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
