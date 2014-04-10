\name{as.ts.mpsth}
\alias{as.ts.mpsth}
\title{Convert a multi-psth list into a (multi) time series object}
\usage{
\method{as.ts}{mpsth}(x, ...)
}
\arguments{
  \item{x}{multi-psth list}

  \item{...}{ignored}
}
\value{
time series or multi-time series object
}
\description{
Convert a multi-psth list into a (multi) time series object
}
\examples{
data(CAL1V,package='STAR')
CAL1V <- lapply(CAL1V,as.repeatedTrain)
psth1 <- psth(CAL1V[["neuron 1"]],breaks=c(bw=0.5,step=0.05),plot=FALSE)
psth2 <- psth(CAL1V[["neuron 2"]],breaks=c(bw=1,step=0.1),plot=FALSE)
z=as.ts(as.mpsth(list(psth1,psth2)))
plot(z,plot.type='single',col=1:2,ylab='Frequency /Hz')
}
\author{
jefferis
}
\seealso{
ts as.ts.mpsth STAR::psth

Other mpsth: \code{\link{[.mpsth}}; \code{\link{as.mpsth}}
}

