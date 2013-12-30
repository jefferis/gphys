\name{OdourResponseFromSpikes}
\alias{OdourResponseFromSpikes}
\title{Produce table of spiking responses (optionally subtracting baseline)}
\usage{
  OdourResponseFromSpikes(spiketimes, responseWindow,
    baselineWindow = NULL, freq = FALSE)
}
\arguments{
  \item{spiketimes}{list of spiketimes collected by
  CollectSpikesFromSweeps}

  \item{responseWindow}{vector of start and end time of
  odour response (in ms)}

  \item{baselineWindow}{vector of start and end time of
  baseline period (in ms)}

  \item{freq}{Calculate Spike rate in Hz rather than number
  of spikes per response window}
}
\value{
  dataframe with a column for each odour and row for each
  sweep
}
\description{
  Details: If baseline window is of different duration to
  response window the baseline count is normalised to
  estimate the number of spikes that would have occurred
  during the response window. ie if you have a 1s response
  window and a 5s baseline window and you get 10 spikes in
  the response period and 5 spikes in the baseline period,
  the response will be returned as 10-5/5 = 9 spikes.
}
\examples{
spikes=CollectSpikesFromSweeps(
  system.file('igor','spikes','nm20110914c4',package='gphys'),
  subdir='BlockI',sweeps=0:4)
od=OdourResponseFromSpikes(spikes,response=c(2200,2700),baseline=c(0,2000))
summary(od)
apply(od,2,function(x) c(mean=mean(x),sd=sd(x)))

# show baseline response frequency only (by treating that as response)
od2=OdourResponseFromSpikes(spikes,response=c(0,2000),freq=TRUE)
summary(od2)
}
\author{
  jefferis
}
\seealso{
  Other OdourResponse:
  \code{\link{PlotOdourResponseFromSpikes}}
}

