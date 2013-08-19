\name{fix.odd}
\alias{fix.odd}
\title{Fix the odour labels in an odd config}
\usage{
  fix.odd(odddf, fixChannels)
}
\arguments{
  \item{odddf}{odour config as read in by read.odd}

  \item{fixChannels}{Named vector of old integer channels
  with new odour names}
}
\value{
  dataframe with fixed odd config
}
\description{
  Fix the odour labels in an odd config
}
\examples{
odd<-read.odd(system.file("igor","oddfiles","simple_odd.txt",package="gphys"))
fixVec=c(empty=31,IAA=30,cVA=29,PAA=27,`4ol`=26,ctr=25)
fixed=fix.odd(odd,fixVec)
}
\author{
  jefferis
}

