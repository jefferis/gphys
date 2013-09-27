\name{CollectSpikesFromSweeps}
\alias{CollectSpikesFromSweeps}
\title{Read in Igor Pro exported text file of Nclamp spike times}
\usage{
  CollectSpikesFromSweeps(sweepdir, sweeps, subdir = "",
    xlim, stimRange, fixChannels = NULL, subset = NULL)
}
\arguments{
  \item{sweepdir}{directory containing Nclamp pxp sweep
  files}

  \item{sweeps}{Vector of sweeps to include (e.g. 1:7) or
  character regex which sweeps must match.}

  \item{subdir}{subdirectory containing group of spike
  times txt files}

  \item{xlim}{time range of sweeps}

  \item{stimRange}{time range of stimulus}

  \item{fixChannels}{Optional named integer vector that
  remaps some bad numeric channels to correct odours. FIXME
  shouldn't we fix channels as well.}

  \item{subset}{Numeric vector of channels or character
  vector of odours}
}
\value{
  list (with class spiketimes) containing times for each
  sweep
}
\description{
  The list of spiketimes has two columns, Time and Wave,
  where wave is the number of the wave within each
  sweepfile containing the spike. xlim and stimRange are
  kept as attributes that will be used for plots *
  fixChannels expects a named vector of any channels that
  need to have different odour names. This can be used to
  fix an error in the original ODD config file. * If spike
  time txt files have been placed in a subdirectory, then
  that subdirectory must be specified using the
  \code{subdir} argument. * However it is expected that the
  corresponding pxp and odd config files remain in the top
  level directory for the cell.
}
\examples{
spikes=CollectSpikesFromSweeps("/Volumes/JData/JPeople/Jonny/physiology/data/nm20110811c0",c(0,1,3))
PlotRasterFromSweeps(spikes,xlim=c(2000,4000),odourRange=c(2000,3000))
# example of collecting only from one of Jonny's subdirectories
spikes=CollectSpikesFromSweeps('/Volumes/JData/JPeople/Jonny/physiology/data/nm20120514c2',subdir='BLOCK B')
# example of setting data directory
options(gphys.datadir='/Volumes/JData/JPeople/Jonny/physiology/data')
spikes=CollectSpikesFromSweeps('nm20120514c2',subdir='BLOCK B')
}
\author{
  jefferis
}

