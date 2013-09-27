\name{spiketimes}
\alias{spiketimes}
\title{spiketimes class to store times of spikes with stimulus information}
\description{
  spiketimes objects consist of a list of dataframes
  reporting the time of spikes. the data frames have two
  core columns Time and Wave. Time is the time in ms at
  which each spike occurred within the current sweep. Wave
  is the 0-indexed number of the sweep within the pxp file
  (Igor convention). Every Wave must have an entry, so if
  there are no spikes in e.g. Wave 2, an entry of
  (Time=NA,Wave=2) will be required.
}
\details{
  Separate dataframes can be combined into a list where
  each dataframe is one block of waves that is repeated
  within a single pxp file or compatible blocks from
  multiple pxp files. Use \code{\link{split.spiketimes}} to
  split a spiketimes object loaded from a pxp file with
  repeated blocks and \code{\link{+.spiketimes}} to combine
  compatible blocks.
}
\seealso{
  Other spiketimes: \code{\link{+.spiketimes}},
  \code{\link{[.spiketimes}}, \code{\link{as.spiketimes}},
  \code{\link{is.spiketimes}},
  \code{\link{merge.spiketimes}},
  \code{\link{split.spiketimes}},
  \code{\link{subset.spiketimes}}
}

