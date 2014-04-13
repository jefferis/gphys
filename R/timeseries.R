# Functions to handle times series and Igor wave data

#' Get time series properties for an Nclamp pxp file
#' 
#' Details if path is a directory, then pxpnum must also be
#' specified
#' @param path Path to directory or specific pxp file we want
#' @param pxpnum Integer number of the pxp file (0, 1, 2 etc)
#' @param WaveToCheck Name of the wave to check (default is RecordA0)
#' @return numeric vector with 3 elements (start, end, freq)
#' @export
#' @seealso \code{\link{tsp}},\code{\link{tsp.igorwave}}
#' @importFrom IgorR read.pxp tsp.igorwave
#' @examples
#' tsp.nclamppxp(system.file("igor","WedJul407c2_001.pxp",package="IgorR"))
tsp.nclamppxp<-function(path,pxpnum,WaveToCheck="RecordA0"){
  if(missing(pxpnum)) file_we_want=path 
  else {
    pxps=dir(path,pattern='\\.pxp$')
    # named vector where names are 000 001 002 etc
    names(pxps)=sub(".*_([0-9]{3}).pxp", "\\1", pxps)
    # file corresponding to pxpnum
    file_we_want=file.path(path,pxps[sprintf("%03d",pxpnum)])
  }
  # nb only read wave headers (much faster for large waves)
  r=read.pxp(file_we_want,StructureOnly=T,regex=WaveToCheck)
  tsp.igorwave(r[[WaveToCheck]])
}

#' Take the mean of all the columns in a multi-time series (mts) object
#'
#' @param x an mts object
#' @param ... additional arguments (currently ignored)
#' @return return value Single time series (ts)
#' @export
#' @method mean mts
#' @seealso \code{\link{ts}}
#' @examples
#' require(IgorR)
#' w<-read.pxp(system.file("igor","WedJul407c2_001.pxp",package="IgorR"))
#' wts<-WaveToTimeSeries(w[c('RecordA0',"RecordA1")])
#' wm<-mean(wts)
#' summary(wm)
mean.mts<-function(x,...){
  avg_wave=rowMeans(x)
  # extract time parameters (start,end,freq)
  tspw=tsp(x)
  ts(avg_wave,start=tspw[1],frequency=tspw[3])
}

#' Rescale the y values of a (multi-)time series (ts) object
#' 
#' Linearly rescale y values to map range[1] to 0 and
#' range[2] to 1. Values outside this range will be <0 and >1 respectively.
#' @param x A \code{ts} or\code{mts} object
#' @param center Value to map to 0 (defaults to the minimum value of yrange)
#' @param scale Defaults to \code{ymax-ymin}
#' @return return value Single time series (ts)
#' @export
#' @seealso \code{\link{ts}}
#' @examples
#' w<-read.pxp(system.file("igor","WedJul407c2_001.pxp",package="IgorR"))
#' wts<-WaveToTimeSeries(w[c('RecordA0',"RecordA1")])
#' ws<-scale(wts,center=-200,scale=400)
scale.ts<-function(x, center = min(x), scale=diff(range(x))){
  centers=center # the y value to map to 0
  scales=scale
  # nb need ncol center and scale values if there is
  # more than 1 column (wave)
  if(!is.null(ncol(x))){
    centers=rep(centers,ncol(x))
    scales=rep(scales,ncol(x))
  }
  scaled_waves=scale.default(x,center=centers,scale=scales)
  # extract time parameters (start,end,freq)
  tspw=tsp(x)
  # turn scaled data back into time series using tsp
  ts(scaled_waves,start=tspw[1],frequency=tspw[3])
}

#' Boxcar smooth and decimate a time series
#'
#' @details One of \code{downsamplefactor, frequency} or \code{deltat} must be specified. 
#' \code{downsamplefactor} will take priority when available.
#' @details See \link[stats]{filter} for details about \code{sides} argument.
#' sides=1 (ie backwards) makes sense if you want to measure the start of a
#' peak (latency). sides=2 (centred, the default) makes sense if you want to 
#' measure the mid-timepoint of the peak itself.
#' @param x time series to smooth (ts or mts)
#' @param filterlength Size of smoothing kernel (in points)
#' @param downsamplefactor Factor to reduce number of points
#' @param start,end Defined start and end to remove NAs after filtering (units of time)
#' @param frequency,deltat New frequency can be specified by either 
#'  (or both if they are consistent).
#' @param extend Logical. If true, the start and end values are allowed to extend
#'  the series. If false, attempts to extend the series give a warning and are ignored.
#' @param sides 1=>convolution for past values, default 2=>centred on lag=0
#' @return time series object with call attribute
#' @export
#' @seealso \code{\link{window},\link[stats]{filter}}
#' @examples
#' x=ts(rnorm(10000)+sin(1:10000/100),start=0,deltat=0.01)
#' # smoothed
#' xs=smooth_decimate(x,filterlength=100,downsamplefactor=10,start=1,end=99)
#' # smoothed with causal filter (past values only)
#' xsc=smooth_decimate(x,filterlength=100,downsamplefactor=10,start=1,end=99,sides=1)
#' plot(x)
#' lines(xs,col='magenta')
#' lines(xsc,col='green')
smooth_decimate<-function(x,filterlength,downsamplefactor=NULL,start=NULL,end=NULL,
    frequency = NULL, deltat = NULL, extend=FALSE,sides=2){
  if(is.null(downsamplefactor) && is.null(frequency) && is.null(deltat)) 
    stop("Must supply at least one of downsamplefactor, frequency or deltat")
  filt=rep(1/filterlength,filterlength)
  filtx=stats::filter(x,filt,sides=sides)
  if(!is.null(downsamplefactor)){
    frequency=frequency(filtx)/downsamplefactor
    deltat=NULL
  }
  wfiltx=window(filtx,start,end,frequency=frequency,deltat=deltat,extend=extend)
  attr(wfiltx,'call')=match.call()
  wfiltx
}
