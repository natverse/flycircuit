context("Plotting")

fc_download_data('http://jefferislab.org/si/nblast/flycircuit/kcs20scores.desc', type='bigmat', quiet=TRUE)
library(nat)
kcs20 <- read.neuronlistfh('http://jefferislab.org/si/nblast/flycircuit/kcs20.rds', getOption('flycircuit.datadir'), quiet=TRUE)
options(nat.default.neuronlist = 'kcs20')

test_that("FlyCircuit neurons can be plotted", {
  plot3d(names(kcs20[1]))
})

test_that("hclust objects are plottable in 3d", {
  hclustres <- hclustfc(names(kcs20))
  expect_true({plot3d(hclustres, h=3); TRUE})
})

test_that("fcwbsurf works", {
  fcwbsurf()
})

test_that("fcwbnpsurf works", {
  fcwbnpsurf()
})
