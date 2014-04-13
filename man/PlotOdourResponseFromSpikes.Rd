\name{PlotOdourResponseFromSpikes}
\alias{PlotOdourResponseFromSpikes}
\title{Boxplot of spikes within a window (optionally less a baseline)}
\usage{
PlotOdourResponseFromSpikes(spiketimes, responseWindow, baselineWindow = NULL,
  freq = FALSE, PLOTFUN = stripchart, ...)
}
\arguments{
  \item{freq}{Plot spike rate in Hz rather than counts
  (default FALSE)}

  \item{PLOTFUN}{stripchart, boxplot or similar function
  (default stripchart)}

  \item{...}{Additional arguments passed on to PLOTFUN}

  \item{spiketimes}{list of spiketimes collected by
  CollectSpikesFromSweeps}

  \item{responseWindow}{vector of start and end time of
  odour response (in ms)}

  \item{baselineWindow}{vector of start and end time of
  baseline period (in ms)}
}
\value{
results of plotfun (if any)
}
\description{
Boxplot of spikes within a window (optionally less a
baseline)
}
\examples{
\dontrun{
spikes=CollectSpikesFromSweeps(
  system.file('igor','spikes','nm20110914c4',package='gphys'),
  subdir='Block I',sweeps=0:4)
## stripchart
PlotOdourResponseFromSpikes(spikes,c(2200,2700),c(0,2000),pch=19,method='jitter',
 col=1:6)
## boxplot, in Hz
PlotOdourResponseFromSpikes(spikes,c(2200,2700),c(0,2000),freq=TRUE,
 PLOTFUN=boxplot)
}
}
\author{
jefferis
}
\seealso{
CollectSpikesFromSweeps

Other OdourResponse: \code{\link{OdourResponseFromSpikes}}
}

