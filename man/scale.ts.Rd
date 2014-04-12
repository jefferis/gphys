\name{scale.ts}
\alias{scale.ts}
\title{Rescale the y values of a (multi-)time series (ts) object}
\usage{
\method{scale}{ts}(x, center = yrange[1], scale = diff(yrange),
  yrange = range(x))
}
\arguments{
  \item{x}{A \code{ts} or\code{mts} object}

  \item{center}{Value to map to 0 (defaults to the minimum
  value of yrange)}

  \item{scale}{Defaults to \code{ymax-ymin}}

  \item{yrange}{The y range, defaults to full scale of all
  waves}
}
\value{
return value Single time series (ts)
}
\description{
Linearly rescale y values to map range[1] to 0 and range[2]
to 1. Values outside this range will be <0 and >1
respectively.
}
\examples{
w<-read.pxp(system.file("igor","WedJul407c2_001.pxp",package="IgorR"))
wts<-WaveToTimeSeries(w[c('RecordA0',"RecordA1")])
ws<-scale.ts(wts,yrange=c(-200,200))
}
\seealso{
\code{\link{ts}}
}

