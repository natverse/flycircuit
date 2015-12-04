# flycircuit
[![Build Status](https://travis-ci.org/jefferis/flycircuit.svg)](https://travis-ci.org/jefferis/flycircuit)

Plot and analyse neurons skeletonised from image data released by 
[FlyCircuit](http://flycircuit.tw).

## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/).

### Bleeding Edge
You can, however, download the [tar ball](https://github.com/jefferis/flycircuit/tarball/master), and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
# if necessary
install.packages("devtools")

# install latest version of nat package (rather than CRAN version)
devtools::install_github("jefferis/nat")
devtools::install_github("jefferis/flycircuit", dependencies=TRUE)
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.

## Getting Started
Take a look at the package vignette (available through R's help system).
