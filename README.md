# flycircuit
[![Build Status](https://travis-ci.org/jefferis/flycircuit.svg)](https://travis-ci.org/jefferis/flycircuit)

Plot and analyse neurons skeletonised from image data released by 
[FlyCircuit](http://flycircuit.tw).

## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/),
but you can use the **devtools** package to install the development version:

```r
# if necessary
if(!requireNamespace('devtools')) install.packages("devtools")
devtools::install_github("jefferis/flycircuit", dependencies=TRUE)
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.

## Getting Started
Take a look at the [package vignette](vignettes/quick-start.Rmd) (also available in fullly processed format through R's help system).

## Getting the data
We have pre-packaged a complete registered/skeletonised version of 16129 neurons from the flycircuit dataset. We are also distributing the all by all NBLAST scores for these neruons. Further details are available at https://gist.github.com/jefferis/bbaf5d53353b3944c090.

You can get the data like so:

```
devtools::source_gist("bbaf5d53353b3944c090")
```
## Acknowledgements
Run
```r
citation("flycircuit")
```
to get details of how to acknowledge the package