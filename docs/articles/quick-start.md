<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Quick-start guide}
-->

# Quick-start guide
Data for various neurons can be downloaded from jefferislab.org and analysed using this package. The data include information on the location of the neurons in 3D space, details on innervation in different neuropils, and NBLAST scores. This quick-start guide is designed to give a brief examples of a subset of the analyses that can be carried out using this package, using an example dataset of 20 Kenyon cells.

## Set up environment

```r
# Allow production of figures using RGL
library(knitr)
knit_hooks$set(rgl = hook_rgl)
opts_chunk$set(dev = c("png", "pdf"))
# sidestep a bug in knitr that fails to produce output dir for rgl snapshots
dir.create("figure")
```


### Load flycircuit and nat (NeuronAnatomy Toolbox) packages

```r
library(flycircuit)
```

```
## Loading required package: rgl
```

```r
library(nat)
```


### Package options
By default, any remote data will be downloaded to the 'extdata' subdirectory in the package's directory tree within the R library. If you wish to save data to a different location then this can be set using: 

```r
options(flycircuit.localroot = "/my/favourite/directory")
```

For example, you may wish to do this if you want to cluster all of the neurons in the dataset, therefore needing to download the 2 GB all by all score matrix. To download and load this matrix, you would need to run:

```r
fc_download_data("http://jefferislab.org/si/nblast/flycircuit/abc2.normdmat.desc", 
    type = "bigmat")
fc_attach_bigmat("abc2.normdmat")
```


## Inspect Kenyon cell NBLAST scores
The package comes with some sample NBLAST scores for a set of 20 Kenyon cells.
Each score is a pairwise comparison between two neurons. The score is asymmetric
i.e. S(A,B)!=S(B,A). See ?kcs20scores for details.

Show the scores for the first five neurons:

```r
kcs20scores[1:5, 1:5]
```

```
##                         FruMARCM-M001205_seg002 GadMARCM-F000122_seg001
## FruMARCM-M001205_seg002                  3234.5                    36.8
## GadMARCM-F000122_seg001                   743.0                  3382.6
## GadMARCM-F000050_seg001                   125.7                   244.9
## GadMARCM-F000142_seg002                  1132.1                   212.5
## FruMARCM-F000270_seg001                   214.3                   261.4
##                         GadMARCM-F000050_seg001 GadMARCM-F000142_seg002
## FruMARCM-M001205_seg002                  -935.3                  -251.5
## GadMARCM-F000122_seg001                  -515.5                  -262.2
## GadMARCM-F000050_seg001                  4259.6                   480.4
## GadMARCM-F000142_seg002                   309.9                  4043.2
## FruMARCM-F000270_seg001                  1962.1                   254.8
##                         FruMARCM-F000270_seg001
## FruMARCM-M001205_seg002                  -460.0
## GadMARCM-F000122_seg001                  -145.3
## GadMARCM-F000050_seg001                  2048.0
## GadMARCM-F000142_seg002                   334.4
## FruMARCM-F000270_seg001                  4202.6
```


## Download other Kenyon cell data
At the moment we have the NBLAST scores for the Kenyon cells, but no other information about them. Let's download some.

```r
# Download the data
kcs20 <- read.neuronlistfh("http://jefferislab.org/si/nblast/flycircuit/kcs20.rds", 
    getOption("flycircuit.datadir"))
```


Now we can plot these 20 neurons and see where they are located in the brain:

```r
# set default view to frontal
r3dDefaults$userMatrix = structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 
    0, 0, 1), .Dim = c(4L, 4L))
open3d()
```

```
## glX 
##   1
```

```r
plot3dfc(names(kcs20), db = kcs20)
```

```
## Error: zero-length inputs cannot be mixed with those of non-zero length
```

```r
fcwbsurf()
```

![plot of chunk plot-kcs](figure/plot-kcs.png) 


We'll be plotting these Kenyon cells a lot, so let's set an option that will save us some typing:

```r
options(nat.default.neuronlist = "kcs20")
```


## Plotting NBLAST hits
Let's plot the two neurons that NBLAST thinks are most similar.

Find the indices of the minimum score (i.e. least distance) in the score matrix:

```r
ind <- which(kcs20scores[, ] == min(kcs20scores[, ]), arr.ind = T)
queryneuron <- colnames(kcs20scores)[ind[2]]
targetneuron <- rownames(kcs20scores)[ind[1]]
```

What was the query?

```r
queryneuron
```

```
## [1] "FruMARCM-F000188_seg001"
```


What was the target?

```r
targetneuron
```

```
## [1] "FruMARCM-M001205_seg002"
```


What do they look like and where in the brain are they?

```r
clear3d()
plot3dfc(c(queryneuron, targetneuron), db = kcs20)
```

```
## Error: zero-length inputs cannot be mixed with those of non-zero length
```

```r
fcwbsurf()
```

![plot of chunk plot-top-hits](figure/plot-top-hits.png) 


## Cluster Kenyon cells by morphology

```r
# nb this works because options(flycircuit.scoremat='kcs20scores') can also
# set the score matrix to use explicitly hckc <-
# hclustfc(rownames(kcs20scores), scoremat=kcs20scores)
hckc <- hclustfc(rownames(kcs20scores))
library(dendroextras)
plot(colour_clusters(hckc, k = 3))
```

![plot of chunk cluster-kcs](figure/cluster-kcs.png) 


Now let's look at the neurons:


```r
clear3d()
plot3d(hckc, k = 3, db = kcs20)
```

```
## Error: zero-length inputs cannot be mixed with those of non-zero length
```

![plot of chunk rgl-cluster-kcs](figure/rgl-cluster-kcs.png) 

Good, we can see the 3 main classes (gamma, alpha'/beta' and alpha/beta).

## More NBLASTing
We know that FruMARCM-F000188_seg001 and FruMARCM-M001205_seg002 are the two most similar neurons in our subset of 20 Kenyon cells, but how do other neurons compare with FruMARCM-F000188_seg001? Let's NBLAST it against all the other neurons and look at the scores.

```r
results <- fc_nblast(queryneuron, names(kcs20), "kcs20scores")
par(las = 2)  # Horizontal labels
par(mar = c(5, 14, 2, 2))  # Extra margin for labels
barplot(results, horiz = T)
```

![plot of chunk blasting](figure/blasting.png) 

Unsurprisingly, FruMARCM-F000188_seg001 itself has the lowest score (seeing as it should have zero distance from itself). Interestingly, while FruMARCM-M001205_seg002 has the second-lowest score, FruMARCM-M000115_seg001 has a similarly small score, closely followed by FruMARCM-F000085_seg001 and GadMARCM-F000442_seg002. Let's plot them all, with the query neuron in black, to see how similar they are:

```r
clear3d()
plot3d(queryneuron, col = "black")
```

```
## Error: zero-length inputs cannot be mixed with those of non-zero length
```

```r
plot3dfc(names(sort(results, decreasing = TRUE)[2:5]))
```

```
## Error: zero-length inputs cannot be mixed with those of non-zero length
```

![plot of chunk plot-good-hits](figure/plot-good-hits.png) 

Hmm, they look pretty similar. Let's plot the four worst hits for comparison:

```r
clear3d()
plot3d(queryneuron, col = "black")
```

```
## Error: zero-length inputs cannot be mixed with those of non-zero length
```

```r
plot3dfc(names(sort(results)[1:4]))
```

```
## Error: zero-length inputs cannot be mixed with those of non-zero length
```

![plot of chunk plot-bad-hits](figure/plot-bad-hits.png) 

Definitely different.
