\name{mean.mts}
\alias{mean.mts}
\title{Take the mean of all the columns in a multi-time series (mts) object}
\usage{
\method{mean}{mts}(x, ...)
}
\arguments{
  \item{x}{an mts object}

  \item{...}{additional arguments (currently ignored)}
}
\value{
return value Single time series (ts)
}
\description{
Take the mean of all the columns in a multi-time series
(mts) object
}
\examples{
require(IgorR)
w<-read.pxp(system.file("igor","WedJul407c2_001.pxp",package="IgorR"))
wts<-WaveToTimeSeries(w[c('RecordA0',"RecordA1")])
wm<-mean(wts)
summary(wm)
}
\seealso{
\code{\link{ts}}
}

