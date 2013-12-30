\name{PlotRasterFromSweeps}
\alias{PlotRasterFromSweeps}
\alias{plot.spiketimes}
\title{Make a raster plot from a set of Nclamp sweeps recording odour responses}
\usage{
  PlotRasterFromSweeps(sweepdir, sweeps, subdir = "",
    subset = NULL, xlim = NULL, xaxis = TRUE, yaxis = TRUE,
    frame.plot = TRUE, xaxs = "i", yaxs = "i", main, sub,
    xlab = "Time/ms", ylab = "Odour", pch = 22,
    dotcolour = "black", dotsize = 0.5, dotwidth = 20,
    spikeheight = 0.8, odourRange = NULL,
    odourCol = rgb(1, 0.8, 0.8, 1), relabelfun = identity,
    fixChannels = NULL, IncludeChannels = FALSE,
    PlotSpikes = TRUE, PlotDividers = TRUE,
    panel.first = NULL, panel.last = NULL, ...)
}
\arguments{
  \item{xlim}{x axis range of plot}

  \item{main}{main title of plot (see \code{\link{title}})}

  \item{sub}{subtitle of plot}

  \item{xlab}{axis label (default Time/ms)}

  \item{ylab}{axis label (default odour)}

  \item{xaxis}{(default TRUE)}

  \item{yaxis}{(default TRUE)}

  \item{frame.plot}{Plot a box around the whole plot
  (default TRUE)}

  \item{xaxs,yaxs}{Whether to extend xlim,ylim by 4%% (see
  ?par, default FALSE)}

  \item{pch}{plotting character (default 22 is a square,
  see details for rect)}

  \item{dotcolour}{colour of dots in raster plot (default
  black)}

  \item{dotsize}{size of dots in raster plot (default 0.5)}

  \item{dotwidth}{Width in ms of rectangle when pch='rect'}

  \item{spikeheight}{Relative height of spike when
  pch='rect' (default 0.8)}

  \item{odourRange}{time window of odour delivery}

  \item{odourCol}{colour of odour window (default pale
  red)}

  \item{relabelfun}{function to apply to odour labels
  (default no relabelling)}

  \item{IncludeChannels}{include numeric id of odour
  channel (e.g. for blanks)}

  \item{PlotSpikes}{Whether to plot the spikes (default
  TRUE)}

  \item{PlotDividers}{Plot the dividing lines between
  odours (default TRUE)}

  \item{panel.first}{An \code{expression} to be evaluated
  after the plot axes are set up but before any plotting
  takes place}

  \item{panel.last}{An \code{expression} to be evaluated
  after spikes have been plotted}

  \item{...}{Additional parameters passed to plot}

  \item{sweepdir}{directory containing Nclamp pxp sweep
  files}

  \item{sweeps}{Vector of sweeps to include (e.g. 1:7) or
  character regex which sweeps must match.}

  \item{subdir}{subdirectory containing group of spike
  times txt files}

  \item{subset}{Numeric vector of channels or character
  vector of odours}

  \item{fixChannels}{Optional named integer vector that
  remaps some bad numeric channels to correct odours. FIXME
  shouldn't we fix channels as well.}
}
\description{
  Make a raster plot from a set of Nclamp sweeps recording
  odour responses
}
\details{
  Note that can also give a spiketimes list from
  CollectSpikesFromSweeps By default the odour stimulus is
  represented by a pale red rectangle in a layer behind the
  spikes. If pch takes the special value "rect" then
  rectangles of width dotwidth are drawn. spikewidth will
  then specify the relative size of the spikes with 1
  resulting in the top of spikes from one line at the same
  height as the base of spikes in the line above. If more
  than one stimulus is required, odourRange can be
  specified as a vector of successive start,stop times e.g.
  c(start1,stop1,start2,stop2).
}
\examples{
## Plot time range 2-4s with odour pulse 2-3s for sweeps 0,1,3
PlotRasterFromSweeps(
  system.file('igor','spikes','nm20110811c0',package='gphys'),
  c(0,1,3),xlim=c(2000,4000),odourRange=c(2000,3000))
# Use rectangles for spikes instead
PlotRasterFromSweeps(
  system.file('igor','spikes','nm20110811c0',package='gphys'),
  c(0,1,3),xlim=c(0,4000),odourRange=c(2000,3000),dotwidth=20,pch='rect')
## Fix a bad label, first define a function
relabel=function(labels) {labels[labels=="fly"]="empty";labels}
## and then use it
PlotRasterFromSweeps(
  system.file('igor','spikes','nm20110811c0',package='gphys'),
  c(0,1,3),relabelfun=relabel)
## Example with block based organisation
## i.e. spike files sorted into different subdirs for each stimulus protocol
PlotRasterFromSweeps(
  system.file('igor','spikes','nm20120514c2',package='gphys'),
  subdir='BLOCKA',odourRange=c(2000,2500),xlim=c(0,5000))
## Example of fixing one of Jonny's traces when channels were mixed up
fixVec=c(empty=31,IAA=30,cVA=29,PAA=27,`4ol`=26,ctr=25)
PlotRasterFromSweeps(
  system.file('igor','spikes','nm20110907c3',package='gphys'),
  subdir='BLOCKI',odourRange=c(2000,2500),xlim=c(0,5000),fixChannels=fixVec)
## Imagine a double odour stimulation
PlotRasterFromSweeps(
  system.file('igor','spikes','nm20110907c3',package='gphys'),
  subdir='BLOCKI',odourRange=c(2000,2500,3000,3250),xlim=c(0,5000),
  fixChannels=fixVec)
}
\author{
  jefferis
}
\seealso{
  \code{\link{CollectSpikesFromSweeps}, \link{fix.odd}} and
  \code{\link{plot.default}} for graphical parameters
}

