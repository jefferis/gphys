\name{AddLinesToRasterPlot}
\alias{AddLinesToRasterPlot}
\title{Add eg voltage traces to existing spike raster plot}
\usage{
  AddLinesToRasterPlot(waves, ylim, col = "black", ...)
}
\arguments{
  \item{waves}{an mts object}

  \item{ylim}{min and max value to plot y axis of wave data
  (eg voltage)}

  \item{col}{vector or function of colours that will be
  passed to \code{\link{lines}}}

  \item{...}{additional arguments passed to lines.ts
  function}
}
\description{
  First thing this does is scale waves to 0-1 range using
  scale.ts Assumes that number of waves and number of boxes
  (odours) on spike raster plot actually match. It doesn't
  check!
}
\details{
  If col is a function then it will be called with the
  number of waves
}
\examples{
\dontrun{
# First plot the rasters
spikes8=CollectSpikesFromSweeps('/Volumes/JData/JPeople/Shahar/Data/120308/nm20120308c0',8)
spike8_split=split(spikes8)
PlotRasterFromSweeps (spike8_split)
# Now plot the voltages
avgwaves=read.table(
  /Volumes/JData/JPeople/Shahar/Data/120308/nm20120308c0/008_Avg_RG0_A0++.txt',
  header=T)
avgwavests=ts(avgwaves,start=0,freq=10)
AddLinesToRasterPlot(avgwavests,col='red')
# same but with rainbow colouring
PlotRasterFromSweeps (spike8_split)
AddLinesToRasterPlot(avgwavests,col='red')
# same but voltage lines underneath spikes
PlotRasterFromSweeps (spike8_split, panel.first=AddLinesToRasterPlot(avgwavests,col='red'))
# same but without spikes or dividers
PlotRasterFromSweeps (spike8_split,PlotSpikes=FALSE,PlotDividers=FALSE)
AddLinesToRasterPlot(avgwavests,col='red')
}
}
\seealso{
  \code{\link{PlotRasterFromSweeps}},\code{\link{lines}}
}

