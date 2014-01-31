context("Test .onLoad")

test_that("onLoad setup is correct", {
  expect_equal(options('flycircuit.localroot')[[1]], '~/projects/flycircuit/')
  expect_equal(options('flycircuit.remoteloc')[[1]], 'http://flybrain.mrc-lmb.cam.ac.uk/flycircuit/')
})
