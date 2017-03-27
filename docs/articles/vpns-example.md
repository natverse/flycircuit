<!--
\VignetteEngine{knitr::knitr}
\VignetteIndexEntry{VPN analysis guide}
-->

# A short guide to analysing visual projection neurons

## Set up environment

```r
# Allow production of figures using RGL
library(knitr)
knit_hooks$set(rgl = hook_rgl)
opts_chunk$set(dev = c("png", "pdf"))
# sidestep a bug in knitr that fails to produce output dir for rgl snapshots
dir.create("figure")
```


### Load flycircuit and nat packages

```r
library(flycircuit)
```

```
## Loading required package: rgl
```

```r
library(nat)
```



## Load VPN NBLAST scores
Download NBLAST scores for VPNs and load them into the workspace.


```r
# Download scores...
fc_download_data("http://jefferislab.org/si/nblast/flycircuit/vpnsblastcv2.5.ff", 
    type = "ff")
```

```
## [1] TRUE
```


```r
# ...and set an option to use them as the default
options(flycircuit.scoremat = "vpnsblastcv2.5.ff")
```


Show the top 10 scores for the a neuron:

```r
scores = fc_nblast("ChaMARCM-F000003_seg001")
```

```
## attaching: vpnsblastcv2.5
## opening ff /Library/Frameworks/R.framework/Versions/3.0/Resources/library/flycircuit/extdata/ff/vpnsblastcv2.5.ff
```

```r
sort(scores, decreasing = TRUE)[1:10]
```

```
##    ChaMARCM-F000003_seg001    GadMARCM-F000623_seg002 
##                       6253                       3607 
##    ChaMARCM-F000289_seg001    FruMARCM-M000008_seg001 
##                       3470                       3411 
##    ChaMARCM-F000826_seg001 DvGlutMARCM-F002277_seg001 
##                       3343                       3321 
## DvGlutMARCM-F002956_seg001    ChaMARCM-F000087_seg002 
##                       3302                       3251 
## DvGlutMARCM-F004615_seg001 DvGlutMARCM-F004068_seg001 
##                       3232                       3150
```

Notice that the top score was of course the 

## Download VPN morphologies
At the moment we have the NBLAST scores for the VPNs, but no other information about them. 
Let's download the neurons. Or rather the neurons, but an object that we can save to our
local disk that contains information about what neurons are present.

```r
# Download the data
vpns <- read.neuronlistfh("http://jefferislab.org/si/nblast/flycircuit/vpns.rds", 
    getOption("flycircuit.datadir"))
# show what kind of data we have for the neurons
head(vpns)
```

```
##                            idid                 gene_name soma_side
## 5HT1bMARCM-F000019_seg001 11625 5HT1bMARCM-F000019_seg001         R
## 5HT1bMARCM-F000024_seg001 11193 5HT1bMARCM-F000024_seg001         L
## 5HT1bMARCM-F000026_seg001 11196 5HT1bMARCM-F000026_seg001         L
## 5HT1bMARCM-M000046_seg001  2500 5HT1bMARCM-M000046_seg001         L
## ChaMARCM-F000003_seg001   11568   ChaMARCM-F000003_seg001         L
## ChaMARCM-F000008_seg001    6140   ChaMARCM-F000008_seg001         R
##                           flipped
## 5HT1bMARCM-F000019_seg001    TRUE
## 5HT1bMARCM-F000024_seg001   FALSE
## 5HT1bMARCM-F000026_seg001   FALSE
## 5HT1bMARCM-M000046_seg001   FALSE
## ChaMARCM-F000003_seg001     FALSE
## ChaMARCM-F000008_seg001      TRUE
##                                                                                          neuronidurl
## 5HT1bMARCM-F000019_seg001 http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&idid=11625
## 5HT1bMARCM-F000024_seg001 http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&idid=11193
## 5HT1bMARCM-F000026_seg001 http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&idid=11196
## 5HT1bMARCM-M000046_seg001  http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&idid=2500
## ChaMARCM-F000003_seg001   http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&idid=11568
## ChaMARCM-F000008_seg001    http://flycircuit.tw/modules.php?name=clearpage&op=detail_table&idid=6140
##                                      Name      Driver
## 5HT1bMARCM-F000019_seg001 5-HT1B-F-400000 5-HT1B-Gal4
## 5HT1bMARCM-F000024_seg001 5-HT1B-F-500013 5-HT1B-Gal4
## 5HT1bMARCM-F000026_seg001 5-HT1B-F-500016 5-HT1B-Gal4
## 5HT1bMARCM-M000046_seg001 5-HT1B-M-400004 5-HT1B-Gal4
## ChaMARCM-F000003_seg001      Cha-F-200000    Cha-Gal4
## ChaMARCM-F000008_seg001      Cha-F-000004    Cha-Gal4
##                                         Gender_Age
## 5HT1bMARCM-F000019_seg001 Female / Adult 5~15 days
## 5HT1bMARCM-F000024_seg001 Female / Adult 5~15 days
## 5HT1bMARCM-F000026_seg001 Female / Adult 5~15 days
## 5HT1bMARCM-M000046_seg001   Male / Adult 5~15 days
## ChaMARCM-F000003_seg001   Female / Adult 5~15 days
## ChaMARCM-F000008_seg001   Female / Adult 5~15 days
##                                      Soma_Coordinate
## 5HT1bMARCM-F000019_seg001  X: -212 , Y: -70 , Z: -90
## 5HT1bMARCM-F000024_seg001   X: 217 , Y: -61 , Z: -93
## 5HT1bMARCM-F000026_seg001  X: 258 , Y: -40 , Z: -118
## 5HT1bMARCM-M000046_seg001  X: 201 , Y: -49 , Z: -107
## ChaMARCM-F000003_seg001    X: 147 , Y: -160 , Z: -58
## ChaMARCM-F000008_seg001   X: -194 , Y: -167 , Z: -11
##                           Putative_neurotransmitter Putative_birth_time
## 5HT1bMARCM-F000019_seg001                   unknown               day 4
## 5HT1bMARCM-F000024_seg001                   unknown               day 5
## 5HT1bMARCM-F000026_seg001                   unknown               day 5
## 5HT1bMARCM-M000046_seg001                   unknown               day 4
## ChaMARCM-F000003_seg001               Acetylcholine               day 2
## ChaMARCM-F000008_seg001               Acetylcholine              embryo
##                              Author       Stock Lineage Gender
## 5HT1bMARCM-F000019_seg001 AS_Chiang 5-HT1B-Gal4              F
## 5HT1bMARCM-F000024_seg001 AS_Chiang 5-HT1B-Gal4              F
## 5HT1bMARCM-F000026_seg001 AS_Chiang 5-HT1B-Gal4              F
## 5HT1bMARCM-M000046_seg001 AS_Chiang 5-HT1B-Gal4              M
## ChaMARCM-F000003_seg001   AS_Chiang    Cha-Gal4              F
## ChaMARCM-F000008_seg001   AS_Chiang    Cha-Gal4              F
##                                       Age       Date Soma_X Soma_Y Soma_Z
## 5HT1bMARCM-F000019_seg001 Adult 5~15 days 2009-05-26   -212    -70    -90
## 5HT1bMARCM-F000024_seg001 Adult 5~15 days 2009-05-31    217    -61    -93
## 5HT1bMARCM-F000026_seg001 Adult 5~15 days 2009-05-31    258    -40   -118
## 5HT1bMARCM-M000046_seg001 Adult 5~15 days 2009-05-26    201    -49   -107
## ChaMARCM-F000003_seg001   Adult 5~15 days 2009-05-26    147   -160    -58
## ChaMARCM-F000008_seg001   Adult 5~15 days 2010-01-25   -194   -167    -11
##                                                                                                   lsmjpgurl
## 5HT1bMARCM-F000019_seg001 http://flycircuit.tw/flycircuitImage/LSM_upload/5HT1bMARCM-F000019_seg001_lsm.jpg
## 5HT1bMARCM-F000024_seg001 http://flycircuit.tw/flycircuitImage/LSM_upload/5HT1bMARCM-F000024_seg001_lsm.jpg
## 5HT1bMARCM-F000026_seg001 http://flycircuit.tw/flycircuitImage/LSM_upload/5HT1bMARCM-F000026_seg001_lsm.jpg
## 5HT1bMARCM-M000046_seg001 http://flycircuit.tw/flycircuitImage/LSM_upload/5HT1bMARCM-M000046_seg001_lsm.jpg
## ChaMARCM-F000003_seg001     http://flycircuit.tw/flycircuitImage/LSM_upload/ChaMARCM-F000003_seg001_lsm.jpg
## ChaMARCM-F000008_seg001     http://flycircuit.tw/flycircuitImage/LSM_upload/ChaMARCM-F000008_seg001_lsm.jpg
##                                                                                             lsmjpg300url
## 5HT1bMARCM-F000019_seg001 http://flycircuit.tw/flycircuitImage/LSM_300/5HT1bMARCM-F000019_seg001_lsm.jpg
## 5HT1bMARCM-F000024_seg001 http://flycircuit.tw/flycircuitImage/LSM_300/5HT1bMARCM-F000024_seg001_lsm.jpg
## 5HT1bMARCM-F000026_seg001 http://flycircuit.tw/flycircuitImage/LSM_300/5HT1bMARCM-F000026_seg001_lsm.jpg
## 5HT1bMARCM-M000046_seg001 http://flycircuit.tw/flycircuitImage/LSM_300/5HT1bMARCM-M000046_seg001_lsm.jpg
## ChaMARCM-F000003_seg001     http://flycircuit.tw/flycircuitImage/LSM_300/ChaMARCM-F000003_seg001_lsm.jpg
## ChaMARCM-F000008_seg001     http://flycircuit.tw/flycircuitImage/LSM_300/ChaMARCM-F000008_seg001_lsm.jpg
##                                                                               ziplsmurl
## 5HT1bMARCM-F000019_seg001 http://flycircuit.tw/zipLSM/5HT1bMARCM-F000019_seg001.lsm.zip
## 5HT1bMARCM-F000024_seg001 http://flycircuit.tw/zipLSM/5HT1bMARCM-F000024_seg001.lsm.zip
## 5HT1bMARCM-F000026_seg001 http://flycircuit.tw/zipLSM/5HT1bMARCM-F000026_seg001.lsm.zip
## 5HT1bMARCM-M000046_seg001 http://flycircuit.tw/zipLSM/5HT1bMARCM-M000046_seg001.lsm.zip
## ChaMARCM-F000003_seg001     http://flycircuit.tw/zipLSM/ChaMARCM-F000003_seg001.lsm.zip
## ChaMARCM-F000008_seg001     http://flycircuit.tw/zipLSM/ChaMARCM-F000008_seg001.lsm.zip
##                                                     lsm ziplsmsize
## 5HT1bMARCM-F000019_seg001 5HT1bMARCM-F000019_seg001.lsm  118198735
## 5HT1bMARCM-F000024_seg001 5HT1bMARCM-F000024_seg001.lsm  131777383
## 5HT1bMARCM-F000026_seg001 5HT1bMARCM-F000026_seg001.lsm  104434363
## 5HT1bMARCM-M000046_seg001 5HT1bMARCM-M000046_seg001.lsm  105427290
## ChaMARCM-F000003_seg001     ChaMARCM-F000003_seg001.lsm   76352938
## ChaMARCM-F000008_seg001     ChaMARCM-F000008_seg001.lsm  107805672
##                                                                                          flashurl
## 5HT1bMARCM-F000019_seg001 http://flycircuit.tw/lsm_movie/female/5HT1bMARCM-F000019_seg001_lsm.flv
## 5HT1bMARCM-F000024_seg001 http://flycircuit.tw/lsm_movie/female/5HT1bMARCM-F000024_seg001_lsm.flv
## 5HT1bMARCM-F000026_seg001 http://flycircuit.tw/lsm_movie/female/5HT1bMARCM-F000026_seg001_lsm.flv
## 5HT1bMARCM-M000046_seg001   http://flycircuit.tw/lsm_movie/male/5HT1bMARCM-M000046_seg001_lsm.flv
## ChaMARCM-F000003_seg001     http://flycircuit.tw/lsm_movie/female/ChaMARCM-F000003_seg001_lsm.flv
## ChaMARCM-F000008_seg001     http://flycircuit.tw/lsm_movie/female/ChaMARCM-F000008_seg001_lsm.flv
##                           flashsize       download_time
## 5HT1bMARCM-F000019_seg001   2224773 2011-01-26 09:33:49
## 5HT1bMARCM-F000024_seg001   2482342 2011-01-26 03:12:52
## 5HT1bMARCM-F000026_seg001   2253510 2011-01-26 03:12:59
## 5HT1bMARCM-M000046_seg001   2442094 2011-01-25 21:45:29
## ChaMARCM-F000003_seg001     2370166 2011-01-26 09:33:33
## ChaMARCM-F000008_seg001     2341834 2011-01-26 00:39:38
##                                  last_updated  SpaceCoding rAL lAL rAMMC
## 5HT1bMARCM-F000019_seg001 2011-01-28 18:13:54   PPPPUU3333   0   0     0
## 5HT1bMARCM-F000024_seg001 2011-01-28 18:13:54    pppsss444   0   0     0
## 5HT1bMARCM-F000026_seg001 2011-01-28 18:13:54      pppp444   0   0     0
## 5HT1bMARCM-M000046_seg001 2011-01-28 18:13:54 ffffpppqqqqq   0   0     0
## ChaMARCM-F000003_seg001   2011-01-28 18:13:54      ppppuuu   0   0     0
## ChaMARCM-F000008_seg001   2011-01-28 18:13:54    PPPPQUUUU   0   0     0
##                           lAMMC rCAL lCAL rCCP lCCP rCMP lCMP rCVLP lCVLP
## 5HT1bMARCM-F000019_seg001     0    0    0    0    0    0    0     0     0
## 5HT1bMARCM-F000024_seg001     0    0    0    0    0    0    0     0     0
## 5HT1bMARCM-F000026_seg001     0    0    0    0    0    0    0     0     0
## 5HT1bMARCM-M000046_seg001     0    0    0    0    0    0    0     0  3283
## ChaMARCM-F000003_seg001       0    0    0    0    0    0    0     0     0
## ChaMARCM-F000008_seg001       0    0    0    0    0    0    0     0     0
##                           rDLP lDLP rDMP lDMP rEB lEB rFB lFB rFSPP lFSPP
## 5HT1bMARCM-F000019_seg001    0    0    0    0   0   0   0   0     0     0
## 5HT1bMARCM-F000024_seg001    0    0    0    0   0   0   0   0     0     0
## 5HT1bMARCM-F000026_seg001    0    0    0    0   0   0   0   0     0     0
## 5HT1bMARCM-M000046_seg001    0    0    0    0   0   0   0   0     0     0
## ChaMARCM-F000003_seg001      0    0    0    0   0   0   0   0     0     0
## ChaMARCM-F000008_seg001      0    0    0    0   0   0   0   0     0     0
##                           rIDFP lIDFP rIDLP lIDLP rLAT lLAT rLH lLH  rLOB
## 5HT1bMARCM-F000019_seg001     0     0     0     0    0    0   0   0  7057
## 5HT1bMARCM-F000024_seg001     0     0     0     0    0    0   0   0     0
## 5HT1bMARCM-F000026_seg001     0     0     0     0    0    0   0   0     0
## 5HT1bMARCM-M000046_seg001     0     0     0     0    0    0   0   0     0
## ChaMARCM-F000003_seg001       0     0     0     0    0    0   0   0     0
## ChaMARCM-F000008_seg001       0     0     0     0    0    0   0   0 11158
##                           lLOB rLOP  lLOP rMB lMB rMED lMED rNOD lNOD  rOG
## 5HT1bMARCM-F000019_seg001    0    0     0   0   0    0    0    0    0   59
## 5HT1bMARCM-F000024_seg001 2341    0     0   0   0    0  921    0    0    0
## 5HT1bMARCM-F000026_seg001 7676    0     0   0   0    0    0    0    0    0
## 5HT1bMARCM-M000046_seg001 1696    0 47378   0   0    0    0    0    0    0
## ChaMARCM-F000003_seg001   8838    0     0   0   0    0    0    0    0    0
## ChaMARCM-F000008_seg001      0    5     0   0   0    0    0    0    0 3636
##                            lOG rOPTU lOPTU rPAN lPAN rPCB lPCB rSDFP lSDFP
## 5HT1bMARCM-F000019_seg001    0     0     0    0    0    0    0     0     0
## 5HT1bMARCM-F000024_seg001    0     0     0    0    0    0    0     0     0
## 5HT1bMARCM-F000026_seg001    0     0     0    0    0    0    0     0     0
## 5HT1bMARCM-M000046_seg001    0     0     0    0    0    0    0     0     0
## ChaMARCM-F000003_seg001   1892     0     0    0    0    0    0     0     0
## ChaMARCM-F000008_seg001      0     0     0    0    0    0    0     0     0
##                           rSOG lSOG rSPP lSPP rVLP lVLP rVMP lVMP     X
## 5HT1bMARCM-F000019_seg001    0    0    0    0 3248    0    0    0 408.6
## 5HT1bMARCM-F000024_seg001    0    0    0    0    0 2712    0    0 415.4
## 5HT1bMARCM-F000026_seg001    0    0    0    0    0 1713    0    0 440.5
## 5HT1bMARCM-M000046_seg001    0    0    0    0    0    0    0    0 412.1
## ChaMARCM-F000003_seg001      0    0    0    0    0    0    0    0 372.9
## ChaMARCM-F000008_seg001      0    0    0    0    0    0    0    0 398.9
##                               Y     Z
## 5HT1bMARCM-F000019_seg001 150.4 84.57
## 5HT1bMARCM-F000024_seg001 144.7 85.29
## 5HT1bMARCM-F000026_seg001 134.9 96.17
## 5HT1bMARCM-M000046_seg001 142.8 88.28
## ChaMARCM-F000003_seg001   198.7 68.02
## ChaMARCM-F000008_seg001   199.6 49.04
```


We'll be plotting these neurons a lot, so let's set an option that will save us some typing:

```r
options(nat.default.neuronlist = "vpns")
# also make sure default view is frontal to fly brain
r3dDefaults$userMatrix = structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 
    0, 0, 1), .Dim = c(4L, 4L))
```

### Simple plot
Let's plot (automagically downloading) the first 3 neurons

```r
open3d()
```

```
## glX 
##   1
```

```r
plot3d(vpns[1:3])
# and add brain surface
fcwbsurf()
```

![plot of chunk plt-3-vpns](figure/plt-3-vpns.png) 


## Cluster cells by morphology
Let's find all the bilateral lobula complex neurons, insisting on at least 500 
voxels in each optic lobula complex.

```r
bilateral = subset(vpns, (rLOP + rLOB) > 500 & (lLOP + lLOB) > 500, rval = "names")
hcbilateral <- hclustfc(bilateral)
# two big clusters
library(dendroextras)
plot(colour_clusters(hcbilateral, k = 2))
```

![plot of chunk plot-bilateral](figure/plot-bilateral1.png) 

```r
# or 7 finer scale ones
plot(colour_clusters(hcbilateral, k = 7))
```

![plot of chunk plot-bilateral](figure/plot-bilateral2.png) 


### Plot 2 major clusters
Now let's look at the two major clusters:

```r
open3d()
```

```
## glX 
##   2
```

```r
plot3d(hcbilateral, k = 2, db = vpns)
# add lobula
fcwbnpsurf("LO_L", alpha = 0.5)
```

![plot of chunk rgl-cluster-bilat-2](figure/rgl-cluster-bilat-2.png) 


Looks like Lobula vs Medulla (not that much in Lobula Plate)

```r
# remove lobula
pop3d()
# ... and add lobula plate
fcwbnpsurf("LOP_L", alpha = 0.5)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

### Plot 7 finer scale clusters

```r
clear3d()
plot3d(hcbilateral, k = 7, db = vpns)
```

![plot of chunk rgl-cluster-bilat-7](figure/rgl-cluster-bilat-7.png) 


## More NBLASTing
How do other neurons compare with 5HT1bMARCM-F000019_seg001? Let's NBLAST it against all the other neurons and look at the distribution of scores.

```r
results <- fc_nblast(names(vpns)[1], names(vpns))
hist(results, breaks = 100)
```

![plot of chunk blasting](figure/blasting.png) 

The little bump at 5717.3936 is for 5HT1bMARCM-F000019_seg001 itself, which has the highest score possible as there should be no distance between it and itself. Let's plot the next four hits, along with the query neuron in black, to see how similar they are:

```r
open3d()
```

```
## glX 
##   3
```

```r
plot3d(names(vpns)[1], col = "black")
plot3dfc(names(sort(results, decreasing = TRUE)[2:5]))
```

![plot of chunk plot-good-hits](figure/plot-good-hits.png) 

Hmm, they look pretty similar. Let's plot the four worst hits for comparison:

```r
open3d()
```

```
## glX 
##   4
```

```r
plot3d(names(vpns)[1], col = "black")
plot3dfc(names(sort(results)[1:4]))
```

![plot of chunk plot-bad-hits](figure/plot-bad-hits.png) 

Definitely different.
