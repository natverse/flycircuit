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
  ffdir <- tempdir()

  # Overwrite directory options for this test
  op=options(flycircuit.datadir=datadir, flycircuit.dbdir=dbdir, flycircuit.bigmatdir=bigmatdir, flycircuit.ffdir=ffdir)
  on.exit(options(op))
  
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/data', type='data', quiet=TRUE)
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/db', type='db', quiet=TRUE)
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/bigmat.desc', type='bigmat', quiet=TRUE)
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/mat.ffrds', type='ff', quiet=TRUE)

  expect_true(file.exists(file.path(datadir, 'data')))
  expect_true(file.exists(file.path(dbdir, 'db')))
  expect_true(file.exists(file.path(bigmatdir, 'bigmat.desc')))
  expect_true(file.exists(file.path(bigmatdir, 'bigmat')))
  expect_true(file.exists(file.path(ffdir, 'mat.ff')))
  expect_true(file.exists(file.path(ffdir, 'mat.ffrds')))
})

test_that("downloaded ff objects have their backing path corrected", {
  # Overwrite directory options for this test
  ffdir <- tempdir()
  op=options(flycircuit.ffdir=ffdir)
  on.exit(options(op))
  
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/mat.ff', type='ff', quiet=TRUE)
  mat=fc_attach_ff('mat')
  expect_equivalent(attr(attr(mat, 'physical'), 'filename'), paste0(getOption('flycircuit.ffdir'), '/mat.ff'))
})

context("Attaching ff")

test_that("can attach an ff object", {
  datadir <- tempdir()
  dbdir <- tempdir()
  bigmatdir <- tempdir()
  ffdir <- tempdir()
  # Overwrite directory options for this test
  op=options(flycircuit.datadir=datadir, flycircuit.dbdir=dbdir, flycircuit.bigmatdir=bigmatdir, flycircuit.ffdir=ffdir)
  on.exit(options(op))
  fc_attach_ff('mat')
  expect_true(exists('mat',envir=.extdata))
})
