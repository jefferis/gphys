# gphys
This R package provides relatively specialised routines for analysing electrophysiological data determined by the research in [our lab](http://jefferislab.org) using whole cell patch clamp techniques to analyse olfactory processing in the fly. Nevertheless it is possible that others, especially anyone using Jason Rothman's [Neuromatic](http://www.neuromatic.thinkrandom.com) data analysis and acquisition package for Igor Pro, may find some functionality useful. It can also be used to reproduce some of the analysis / figures in our forthcoming work.

For further details see the R [DESCRIPTION](DESCRIPTION) file.
 
## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/).

### Released versions
The recommendation is to install from our lab repository:

```r
install.packages("gphys",repos='http://jefferislab.org/R',type='source')
```

### Bleeding Edge
You can, however, download the [tar ball](https://github.com/jefferis/gphys/tarball/develop), and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
# install devtools if required
install.packages("devtools")

library(devtools)
# nb develop branch will typically be the most up to date since master
# is now reserved for released versions.
install_github("gphys", "jefferis",ref='develop')
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.

## Acknowledgements
This tool naturally depends on a number of other R packages including my own package [IgorR]. However the most sophisticated and domain-specific dependency is the [STAR](http://cran.r-project.org/web/packages/STAR) package of Christophe Pouzat – this offers a great deal of exciting functionality that we hope to use in future.