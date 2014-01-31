context(".onLoad")

test_that("onLoad setup is correct", {
  expect_equal(getOption('flycircuit.localroot'), system.file(package='flycircuit'))
  expect_equal(getOption('flycircuit.remoteloc'), 'http://flybrain.mrc-lmb.cam.ac.uk/flycircuit/')
  expect_equal(getOption('flycircuit.datadir'), 'data')
  expect_equal(getOption('flycircuit.dbdir'), 'db')
  expect_equal(options('flycircuit.bigmatdir')[[1]], 'data/bigmat')
  expect_equal(getOption('nat.default.neuronlist'), 'dps')
})
