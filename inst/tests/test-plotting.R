context("Plotting")

library(nat)
kcs20 <- read.neuronlistfh('http://jefferislab.org/si/nblast/flycircuit/kcs20.rds', getOption('flycircuit.datadir'), quiet=TRUE)
options(nat.default.neuronlist = 'kcs20')

open3d()

test_that("FlyCircuit neurons can be plotted", {
  plot3d(names(kcs20[1]))
})

test_that("hclust objects are plottable in 3d", {
  hclustres <- hclustfc(names(kcs20))
  clear3d()
  expect_is(plot3d(hclustres, h=3),'list')
})

test_that("fcwbsurf works", {
  clear3d()
  fcwbsurf()
})

test_that("fcwbnpsurf works", {
  clear3d()
  fcwbnpsurf()
})

rgl.close()