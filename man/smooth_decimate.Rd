\name{smooth_decimate}
\alias{smooth_decimate}
\title{Boxcar smooth and decimate a time series}
\usage{
  smooth_decimate(x, filterlength, downsamplefactor = NULL,
    start = NULL, end = NULL, frequency = NULL,
    deltat = NULL, extend = FALSE, sides = 2)
}
\arguments{
  \item{x}{time series to smooth (ts or mts)}

  \item{filterlength}{Size of smoothing kernel (in points)}

  \item{downsamplefactor}{Factor to reduce number of
  points}

  \item{start,end}{Defined start and end to remove NAs
  after filtering (units of time)}

  \item{frequency,deltat}{New frequency can be specified by
  either (or both if they are consistent).}

  \item{extend}{Logical. If true, the start and end values
  are allowed to extend the series. If false, attempts to
  extend the series give a warning and are ignored.}

  \item{sides}{1=>convolution for past values, default
  2=>centred on lag=0}
}
\value{
  time series object with call attribute
}
\description{
  Boxcar smooth and decimate a time series
}
\details{
  One of \code{downsamplefactor, frequency} or
  \code{deltat} must be specified. \code{downsamplefactor}
  will take priority when available.
}
\examples{
x=ts(rnorm(10000)+sin(1:10000/100),start=0,deltat=0.01)
# smoothed
xs=smooth_decimate(x,filterlength=100,downsamplefactor=10,start=1,end=99)
# smoothed with causal filter (past values only)
xsc=smooth_decimate(x,filterlength=100,downsamplefactor=10,start=1,end=99,sides=1)
plot(x)
lines(xs,col='magenta')
lines(xsc,col='green')
}
\seealso{
  \code{\link{window},\link[stats]{filter}}
}

