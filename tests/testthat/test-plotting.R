context("Plotting")

library(nat)
kcs20 <- read.neuronlistfh('http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/kcs20.rds', getOption('flycircuit.datadir'), quiet=TRUE)
op=options(nat.default.neuronlist = 'kcs20', flycircuit.scoremat = 'kcs20scores')

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

test_that("plot3dfc produces a warning for missing neurons", {
  clear3d()
  expect_is(plot3dfc(names(kcs20)[1:5]),'list')
  expect_warning(plot3dfc('rhubarb'))
})

rgl.close()

options(op)