#' @export
#' @param ... Additional arguments (currently ignored)
divide<-function(x, ...) UseMethod("divide")

#' Divide spiketimes object with multiple repeats into list with one entry per 
#' repeat
#' 
#' divide.spiketimes is designed for the situation where a single pxp file 
#' contains multiple repeats for the same stimulus set.
#' 
#' @details Only works for spiketimes from single pxp file.
#'   
#'   The function can cope with cases where odour order has been shuffled within
#'   repeated blocks each containing all test odours (but not when the odours
#'   could appear at any point in the test sequence). 
#'   
#'   For shuffled data the Wave column will be a number from 0 to blocksize-1
#'   that indicates which of the presented channels the spikes belong to. Note
#'   that this is sorted by absolute channel number \bold{not} by the order of
#'   presentation of odours in the first block.
#'   
#'   Likwise the \code{oddconf} of the returned list of data.frames will have
#'   the odours sorted in absolute channel order.
#' @param x The spiketimes object
#' @param blocksize number of waves per block - deduced from odour names if 
#'   missing
#' @return new spiketimes object (list) with one entry for each block of odours
#' @author jefferis
#' @export
#' @family spiketimes
#' @rdname divide
divide.spiketimes<-function(x, blocksize, ...){
  if(length(x)>1) stop("Don't know how to divide a spike time list of > length 1")
  st1=x[[1]]
  st1$OldWave=st1$Wave
  odd=attr(x,'oddconf')
  if(missing(blocksize))
    blocksize=length(unique(odd$odour))
  nWaves=max(st1$OldWave, na.rm = TRUE)+1
  nblocks=round(nWaves/blocksize)
  if(nblocks*blocksize!=nWaves){
    stop("nblocks * blocksize != nWaves!")
  }
  
  # What is the correct order for the odours?
  # sorted unique channel numbers
  sorted_chans=sort(unique(odd$chan))
  

  # map the order that the waves were acquired in onto that chosen odour order.
  order_for_wave=match(odd$chan, sorted_chans)
  # now map the wave index (e.g. 0-31) through that vector to give 
  # the order in which sweeps would have been acquired if they were not shuffled
  st1$Wave=order_for_wave[st1$OldWave+1]-1

  # Find 0 indexed repetition number
  st1$Rep=floor(st1$OldWave/blocksize)

  stnew=split(st1,st1$Rep)
  mostattributes(stnew)=attributes(x)
  # We will have to make new names now that the list has more elements
  # Nb new names will look like 008.000,008.001 
  # (ie still 0-indexed for second part)
  names(stnew)=sprintf("%s.%03d",names(x),seq_along(stnew)-1)
  if(!is.null(attr(stnew,'oddconf'))){
    attr(stnew,'oddconf')=attr(stnew,'oddconf')[order_for_wave[1:blocksize],]
  }
  stnew
}

#' Split a spiketimes object with multiple repeats into list with one entry per 
#' repeat
#' 
#' \emph{Deprecated} as of gphys 0.9 and will be removed from gphys 1.0 (CRAN 
#' release).
#' 
#' Only works for spiketimes from single pxp file. FIXME Teach this to cope with
#' repeat blocks with different odours in different order
#' @param x The spiketimes object
#' @param f number of waves per block - deduced from odour names if missing
#' @param drop IGNORED
#' @param ... IGNORED
#' @return new spiketimes object (list) with one entry for each block of odours
#' @author jefferis
#' @method split spiketimes
#' @export
#' @family spiketimes
split.spiketimes<-function(x, f, drop, ...){
  .Deprecated('divide',msg = "Please switch to new gphys::divide function!")
  divide(x=x, blocksize=f)
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
`+.spiketimes` <- function(e1,e2) {
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
`[.spiketimes` <- function(x,i,...) {
  st=structure(NextMethod("["), class = class(x))
  mostattributes(st) <- attributes(x)
  names(st)=names(x)[i]
  attr(st,'sweeps')=names(st)
  st
}

#' Merge two spiketimes lists with different ODD configs
#'
#' @param x,y spiketimes objects to merge
#' @param ... additional params (currently ignored)
#' @return new spiketimes object with original attributes merged as appropriate
#'    and additional mergedwaveinfo attribute indicating origin of waves.
#' @method merge spiketimes
#' @family spiketimes
#' @export
#' @seealso \code{\link{+.spiketimes}} for combining repeats from the same ODD config
merge.spiketimes<-function(x,y,...){
  if(!is.spiketimes(y)) stop("Can only merge two spiketimes objects")
  maxlen=max(length(x),length(y))
  
  l=list()
  
  # Figure out how many waves we have in x
  xLastWave=max(x[[1]]$Wave,na.rm=T)
  # merge spike data frames
  for(i in seq(maxlen)){
    a=if(i<=length(x)) x[[i]] else NULL
    b=if(i<=length(y)) y[[i]] else NULL
    if(!is.null(b)){
      b$Wave=xLastWave+1+b$Wave
    }
    ab=rbind(a,b)
    l[[i]]=ab
  }
  
  # bring over names etc
  attributes_to_copy=if(length(x)>=length(y)) attributes(x) else attributes(y)
  mostattributes(l)=attributes_to_copy
  # fix names to conform to show that they look like 000,008 to show that they
  # contain data from 2 pxp files
  nx=names(x)
  ny=names(y)
  lx=length(x)
  ly=length(y)
  if(lx>ly){
    ny=c(ny,rep("",lx-ly))
  } else if(ly>lx){
    nx=c(nx,rep("",ly-lx))
  }
  names(l)=paste(nx,ny,sep=",")
  # merge simple attributes that it makes to sense merge
  for(att in c("sweeps",'sweepdir')){
    attr(l,att)=unique(c(attr(x,att),attr(y,att)))
  }
  
  # add a new attribute that specifies the set of pxps for each wave
  # using the new numbering
  aWaves=seq(from=0,to=xLastWave)
  yLastWave=max(y[[1]]$Wave,na.rm=T)
  bWavesOld=seq(from=0,to=yLastWave)
  bWavesNew=bWavesOld+xLastWave+1
  pxps4wave=list()
  for(w in aWaves) pxps4wave[[as.character(w)]]=names(x)
  for(w in bWavesNew) pxps4wave[[as.character(w)]]=names(y)
  mergedwaveinfo=cbind(merged=as.integer(names(pxps4wave)),
                       original=c(aWaves,bWavesOld),
                       pxps=pxps4wave)
  attr(l,'mergedwaveinfo')=mergedwaveinfo

  # merge odd configs
  attr(l,'oddconf')=rbind(attr(x,'oddconf'),attr(y,'oddconf'))
  l
}

#' Subset spiketimes object to contain only sweeps for an odour subset
#'
#' NB the sweeps will be in the specified odour/channel order. If an unnamed
#' second parameter is specified it will be interpreted as vector of odours if
#' it is a character or channel ids if it is numeric.
#'
#' If an odour name is duplicated in the oddconfig (e.g. ctr or oil, often) then
#' a warning will be given and the first matching sweep(s) will be used.
#'
#' @param x The original spiketimes object
#' @param odours A character vector of odours
#' @param channels Integer vector of channels
#' @param ... further arguments to be passed to or from other methods (currently
#'   ignored)
#' @return spiketimes object restricted to specified odours or channels
#' @author jefferis
#' @family spiketimes
#' @aliases subset
#' @export
subset.spiketimes<-function(x,odours=NULL,channels=NULL, ...){
  oddconf=attr(x,'oddconf')
  # Note that waves come in 0-indexed from Igor so we'll do the same
  oddconf$OldWave=seq_len(nrow(oddconf))-1
  if(is.null(channels) && is.numeric(odours)) {channels=odours;odours=NULL}
  if(!is.null(odours)){
    if(any(duplicated(odours)))
      stop("Cannot handle subsets that contain duplicated odours")
    if(any(duplicated(oddconf$odour))){
      dupodours<-unique(oddconf$odour[duplicated(oddconf$odour)])
      seldupodours <- intersect(odours, dupodours)
      if(length(seldupodours)){
        warning("Keeping first occurence of duplicate odour(s) in ODD config (",
            paste(seldupodours,collapse=" "),")")
      }
    }
    rownames(oddconf)=make.unique(as.character(oddconf$odour))
    newoddconf=oddconf[odours,]
  } else {
    if(is.null(channels))
      stop("Must supply either odours or channels")
    if(any(duplicated(channels)))
      stop("Cannot handle subsets that contain duplicated channels")
    if(any(duplicated(oddconf$chan))){
      # check if the channels we want are among the duplicates. If yes, warn
      duplicated_channels=unique(oddconf$chan[duplicated(oddconf$chan)])
      duplicated_channels_we_want=intersect(duplicated_channels,channels)
      if(length(duplicated_channels_we_want)){
        warning("Will use first sweep for duplicated channels: ",
                duplicated_channels_we_want)
      }
    }
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
    # nb we have checked that we are not intersted in any of the non-unique
    # row names
    rownames(oddconf)=make.unique(as.character(oddconf$chan))
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
          ssx=x[x$Wave==sel_oldwaves[i], c("Time", "Wave", "OldWave")]
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

#' Make spiketimes object (spike times + stimulus info) from list of dataframes
#' 
#' spiketimes objects consist of a list of dataframes reporting the time of
#' spikes. the data frames have two core columns Time and Wave. Time is the time
#' in ms at which each spike occurred within the current sweep. Wave is the
#' 0-indexed number of the sweep within the pxp file (Igor convention). Every
#' Wave must have an entry, so if there are no spikes in e.g. Wave 2, an entry
#' of (Time=NA,Wave=2) will be required.
#' 
#' Separate dataframes can be combined into a list where each dataframe is one 
#' block of waves that is repeated within a single pxp file or compatible blocks
#' from multiple pxp files. Use \code{\link{split.spiketimes}} to split a
#' spiketimes object loaded from a pxp file with repeated blocks and 
#' \code{\link{+.spiketimes}} to combine compatible blocks.
#' 
#' \code{\link{CollectSpikesFromSweeps}} will generate a spiketimes object for 
#' you, so this function will only needed if you are constructing 
#' \code{\link{spiketimes}} objects from scratch.
#' @param x object to convert to spiketimes list (normally already a list(
#' @param xlim recording time window for each sweep
#' @param stimRange time window over which stimulus was delivered
#' @return spiketimes object
#' @export
#' @family spiketimes
spiketimes<-function (x,xlim,stimRange) {
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
#' @param ... further arguments to be passed to or from other methods.
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
#' @inheritParams as.repeatedTrain
#' @export
#' @importFrom stats na.omit
#' @rdname as.repeatedTrain
#' @method as.repeatedTrain spiketimes
#' @aliases as.repeatedTrain
#' @seealso \code{\link[STAR]{as.repeatedTrain}}
#' @examples
#' spikes<-CollectSpikesFromSweeps(
#'   system.file('igor','spikes','nm20110914c4',package='gphys'),
#'   subdir='BLOCKI',sweeps=0:4)
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
    spikelist=lapply(x,function(s) s[s$Wave==(i-1) & !is.na(s$Wave),'Time']/1000)
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

#' Find the Igor waves matching a set of spikes (even if shuffled)
#' 
#' @param x a spiketimes object
IgorWavesForSpikes<-function(x) {
  # join up any repeat blocks
  rx=do.call(rbind, x)
  # Find unique rows after dropping the time column
  # these tell us which igor file number and wave we want
  ix=unique(rx[,-1])
  
  # find pxp files
  pxps=dir(attr(x,'sweepdir'), pattern="_[0-9]{3}\\.pxp$", full.names = TRUE)
  # name those by the sweep number
  names(pxps)=sub(".*_([0-9]{3})\\.pxp", "\\1", pxps)
  # now we can figure out which file we want
  ix$File=pxps[ix$FileNum]
  ix
}

#' Make average (analogue) waves that match a spiketimes object
#' 
#' You can use the \code{wavestem} argument to choose the analogue input channel
#' to use for calculation.
#' 
#' @param x A spiketimes object
#' @param wavestem The stem of the wave names in Igor (defaults to 
#'   \code{"RecordA"})
#' @return A \code{mts} object with as many rows as there are Waves in the 
#'   stimulus protocol (i.e. normally odours).
#' @export
#' @seealso \code{\link{CollectSpikesFromSweeps}}
MakeAverageWaves<-function(x, wavestem="RecordA"){
  ix=IgorWavesForSpikes(x)
  ix$IgorWave=paste0(wavestem, ix$OldWave)
  files_to_read=unique(ix$File)
  names(files_to_read)=sub(".*_([0-9]{3})\\.pxp", "\\1", files_to_read)
  # n
  wavelist=sapply(files_to_read, read.pxp, regex=wavestem, simplify = FALSE)
  
  avgwaves=list()
  for(w in sort(unique(ix$Wave))){
    wix=ix[ix$Wave==w, ]
    if(nrow(wix)==0) stop("Unable to find Igor waves for wave: ", w)
    thisfilenum=unique(wix$FileNum)
    # FIXME we don't want to keep this limitation for ever
    if(length(thisfilenum)>1)
      stop("Waves for a single channel come from different files. I wasn't expecting that!")
    
    # get those waves as a list
    wl=wavelist[[thisfilenum]][wix$IgorWave]
    # turn them into a timeseries
    ww=IgorR::WaveToTimeSeries(wl)
    # average them 
    avgwaves[[as.character(w)]]=mean(ww)
  }
  # now cbind all the average waves together to make our final answer
  do.call(cbind, avgwaves)
}
