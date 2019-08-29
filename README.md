[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.github.io)
[![Build Status](https://travis-ci.org/jefferis/flycircuit.svg)](https://travis-ci.org/jefferis/flycircuit)
<img src="man/figures/logo.svg" align="right" height="139" />

# flycircuit

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

```r
# read FlyCircuit neurons
?fc_read_neurons

# open FlyCircuit
?fc_page

# get FlyCircuit IDs
?fc_get_ids
```

## Getting the data
We have pre-packaged a complete registered/skeletonised version of 16129 neurons from the flycircuit dataset. We are also distributing the all by all NBLAST scores for these neurons. Further details are available at https://gist.github.com/jefferis/bbaf5d53353b3944c090.

You can get the data like so:

```
devtools::source_gist("bbaf5d53353b3944c090")
```

You can also read skeletons hosted on the FlyCircuit website, as so

You can get all the data like so:

``` r
 # Let's read a neuron from the FlyCircuit database
 library(nat.flybrains)
 fcn <- fc_read_neurons("Gad1-F-200234")
 plot3d(fcn)
 plot3d(FCWB)
 
 # We can also read all neurons
 clear3d()
 fc.ids = fc_get_ids()
 fcns <- fc_read_neurons(fc.ids)
 plot3d(fcns)
 plot3d(FCWB, alpha = 0.1)
 
 # Now mirror all neurons to the right of the brain
 left.somas <- function(neuron,bound = boundingbox(FCWB.surf)[1,1]+((boundingbox(FCWB.surf)[2,1]-boundingbox(FCWB.surf)[1,1])/2)){
     r = nat::rootpoints(neuron)
     position = nat::xyzmatrix(neuron$d[r,])
     position[,"X"]>bound
 }
 leftsomas = unlist(nat::nlapply(fcns,left.somas))
 fcsleft = nat.templatebrains::mirror_brain(fcns[leftsomas], brain = FCWB)
 fcns = c(fcns[!names(fcns)%in%names(fcsleft)],fcsleft)
```

Acknowledging the data and tools
--------------------------------

Any work that uses data from this package should cite

**Chiang, Ann-Shyn, Chih-Yung Lin, Chao-Chun Chuang, Hsiu-Ming Chang, Chang-Huain Hsieh, Chang-Wei Yeh, Chi-Tin Shih, et al.** 2011. *Three-Dimensional Reconstruction of Brain-Wide Wiring Networks in Drosophila at Single-Cell Resolution.* Current Biology: CB 21 (1): 1–11.

**Lee, Ping-Chang, Chao-Chun Chuang, Ann-Shyn Chiang, and Yu-Tai Ching.** 2012. *High-Throughput Computer Method for 3D Neuronal Structure Reconstruction from the Image Stack of the Drosophila Brain and Its Applications.* PLoS Computational Biology 8 (9): e1002658.

This package was created by [James Manton](https://scholar.google.co.uk/citations?user=iYVk_psAAAAJ&hl=en), [Marta Costa](https://scholar.google.co.uk/citations?user=yE5yjP0AAAAJ&hl=en), [Alexander Shakeel Bates](https://scholar.google.com/citations?user=BOVTiXIAAAAJ&hl=en) and [Dr. Gregory Jefferis](https://en.wikipedia.org/wiki/Gregory_Jefferis). You can cite this package as:

``` r
citation(package = "flycircuit")
```
