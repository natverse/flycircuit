context(".onLoad")

test_that("onLoad setup is correct", {
  expect_equal(getOption('flycircuit.remoteloc'), 'http://flybrain.mrc-lmb.cam.ac.uk/flycircuit')
  expect_equal(getOption('nat.default.neuronlist'), 'dps')
})
