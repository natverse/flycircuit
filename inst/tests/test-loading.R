context("Loading data")

test_that("load_fcdata can load local file", {
  load_fcdata('fcidtable')
  expect_true(exists('fcidtable'))
})

test_that("load_fcdb can load local database", {
  load_fcdb('annotation')
  expect_true(exists('annotation'))
})

context("Downloading data")

test_that("can download data from remote lcoation", {
  datadir <- tempdir()
  dbdir <- tempdir()
  bigmatdir <- tempdir()

  # Overwrite directory options for this test
  op=options(flycircuit.datadir=datadir, flycircuit.dbdir=dbdir, flycircuit.bigmatdir=bigmatdir)
  on.exit(options(op))
  
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/data', type='data', quiet=TRUE)
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/db', type='db', quiet=TRUE)
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/bigmat.desc', type='bigmat', quiet=TRUE)

  expect_true(file.exists(file.path(datadir, 'data')))
  expect_true(file.exists(file.path(dbdir, 'db')))
  expect_true(file.exists(file.path(bigmatdir, 'bigmat.desc')))
  expect_true(file.exists(file.path(bigmatdir, 'bigmat')))
})