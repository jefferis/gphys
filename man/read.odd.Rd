\name{read.odd}
\alias{read.odd}
\title{Read in the ODD text file describing odour presentation sequence}
\usage{
read.odd(oddfile, fill = TRUE, stringsAsFactors = FALSE, Verbose = FALSE)
}
\arguments{
  \item{oddfile}{Path to ODD file}

  \item{stringsAsFactors}{Whether to convert odours to
  strings (default) or factors}

  \item{fill}{Whether to fill in missing delay/duration
  values with NAs}

  \item{Verbose}{Show number of records read (default
  TRUE)}
}
\value{
dataframe containing a list of odours / presentation times
}
\description{
Read in the ODD text file describing odour presentation
sequence
}
\author{
jefferis
}

