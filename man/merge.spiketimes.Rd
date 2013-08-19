\name{merge.spiketimes}
\alias{merge.spiketimes}
\title{Merge two spiketimes lists with different ODD configs}
\usage{
  \method{merge}{spiketimes} (x, y, ...)
}
\arguments{
  \item{x,y}{spiketimes objects to merge}

  \item{...}{additional params (currently ignored)}
}
\value{
  new spiketimes object
}
\description{
  FIXME - figure out how to handle different numbers of
  repeats more gracefully
}
\seealso{
  \code{\link{+.spiketimes}} for combining repeats from the
  same ODD config

  Other spiketimes: \code{\link{+.spiketimes}},
  \code{\link{[.spiketimes}}, \code{\link{as.spiketimes}},
  \code{\link{is.spiketimes}}, \code{\link{spiketimes}},
  \code{\link{split.spiketimes}},
  \code{\link{subset.spiketimes}}
}

