# gphys
[![DOI](https://img.shields.io/badge/doi-10.5281%2Fzenodo.10300-blue.svg)](http://dx.doi.org/10.5281/zenodo.10300)
[![Build Status](https://travis-ci.org/jefferis/gphys.svg)](https://travis-ci.org/jefferis/gphys)

This R package provides relatively specialised routines for analysing electrophysiological data determined by the research in [our lab](http://jefferislab.org) using whole cell patch clamp techniques to analyse olfactory processing in the fly. Nevertheless it is possible that others, especially anyone using Jason Rothman's [Neuromatic](http://www.neuromatic.thinkrandom.com) data analysis and acquisition package for Igor Pro, may find some functionality useful. It can also be used to reproduce some of the analysis / figures in our published work
e.g. in the package [frulhns](https://github.com/jefferis/frulhns).

For further details see the R [DESCRIPTION](DESCRIPTION) file.
 
## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/).You can use 
the **devtools** package to install either the latest **release** or **development** version:

```r
# install devtools if required
install.packages("devtools")

library(devtools)
# latest release (recommended)
install_github("jefferis/gphys")

# develop branch may occasionally have changes that have not been merged to master
install_github("jefferis/gphys@develop")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.

## Acknowledgements
This tool naturally depends on a number of other R packages including my own package [IgorR](https://github.com/jefferis/IgorR). However the most sophisticated and domain-specific dependency is the [STAR](http://cran.r-project.org/web/packages/STAR) package of Christophe Pouzat – this offers a great deal of exciting functionality that we hope to use in future.
