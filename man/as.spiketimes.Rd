\name{as.spiketimes}
\alias{as.spiketimes}
\title{Convert list of dataframes to spiketimes object}
\usage{
  as.spiketimes(x, xlim, stimRange)
}
\arguments{
  \item{x}{object to convert to spiketimes list (normally
  already a list(}

  \item{xlim}{recording time window for each sweep}

  \item{stimRange}{time window over which stimulus was
  delivered}
}
\value{
  spiketimes object
}
\description{
  \code{\link{CollectSpikesFromSweeps}} will do this for
  you, so this only needed if you are constructing
  \code{\link{spiketimes}} objects from scratch.
}
\seealso{
  Other spiketimes: \code{\link{+.spiketimes}},
  \code{\link{[.spiketimes}}, \code{\link{is.spiketimes}},
  \code{\link{merge.spiketimes}}, \code{\link{spiketimes}},
  \code{\link{split.spiketimes}},
  \code{\link{subset.spiketimes}}
}

