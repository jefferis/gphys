\name{tsp.nclamppxp}
\alias{tsp.nclamppxp}
\title{Get time series properties for an Nclamp pxp file}
\usage{
tsp.nclamppxp(path, pxpnum, WaveToCheck = "RecordA0")
}
\arguments{
  \item{path}{Path to directory or specific pxp file we
  want}

  \item{pxpnum}{Integer number of the pxp file (0, 1, 2
  etc)}

  \item{WaveToCheck}{Name of the wave to check (default is
  RecordA0)}
}
\value{
numeric vector with 3 elements (start, end, freq)
}
\description{
Details if path is a directory, then pxpnum must also be
specified
}
\examples{
tsp.nclamppxp(system.file("igor","WedJul407c2_001.pxp",package="IgorR"))
}
\seealso{
\code{\link{tsp}},\code{\link{tsp.igorwave}}
}

