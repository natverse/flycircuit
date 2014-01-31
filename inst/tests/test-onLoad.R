context(".onLoad")

test_that("onLoad setup is correct", {
  expect_equal(getOption('flycircuit.localroot'), system.file(package='flycircuit'))
  expect_equal(getOption('flycircuit.remoteloc'), 'http://flybrain.mrc-lmb.cam.ac.uk/flycircuit/')
  expect_equal(getOption('flycircuit.datadir'), file.path(getOption('flycircuit.localroot'), 'data'))
  expect_equal(getOption('flycircuit.dbdir'), file.path(getOption('flycircuit.localroot'), 'db'))
  expect_equal(getOption('flycircuit.bigmatdir'), file.path(getOption('flycircuit.localroot'), 'data/bigmat'))
  expect_equal(getOption('nat.default.neuronlist'), 'dps')
})
