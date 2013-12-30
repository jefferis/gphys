\name{subset.spiketimes}
\alias{subset.spiketimes}
\title{Make a new spiketimes object containing only sweeps for an odour subset}
\usage{
  \method{subset}{spiketimes} (x, odours = NULL,
    channels = NULL, ...)
}
\arguments{
  \item{x}{The old spiketimes object}

  \item{odours}{A character vector of odours}

  \item{channels}{Integer vector of channels}

  \item{...}{further arguments to be passed to or from
  other methods.}
}
\value{
  spiketimes object restricted to specified odours or
  channels
}
\description{
  NB the sweeps will be in the specified odour/channel
  order. If an unnamed second parameter is specified it
  will be interpreted as vector of odours if character or
  channel ids if numeric. NB If an odour name is duplicated
  in the oddconfig (e.g. ctr or oil, often) then it is not
  possible to use them in a subset expression and the
  channel number must be used instead.
}
\author{
  jefferis
}
\seealso{
  Other spiketimes: \code{\link{+.spiketimes}},
  \code{\link{[.spiketimes}}, \code{\link{is.spiketimes}},
  \code{\link{merge.spiketimes}}, \code{\link{spiketimes}},
  \code{\link{split.spiketimes}}
}

