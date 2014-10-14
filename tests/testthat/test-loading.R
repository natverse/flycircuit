context("Loading data")

test_that("load_fcdata can load local file", {
  savepath=file.path(getOption("flycircuit.dbdir"),'fcidtable_copy.rds')
  on.exit(unlink(savepath))
  saveRDS(fcidtable,file=savepath)
  expect_equal(load_fcdb('fcidtable_copy'), fcidtable)
  rdapath=file.path(getOption("flycircuit.datadir"),'fcidtable.rda')
  save(fcidtable,file=rdapath)
  expect_equal(load_fcdata('fcidtable'), NULL)
  expect_equal(load_fcdata('fcidtable', Force = T), 'fcidtable')
  on.exit(unlink(rdapath), add = TRUE)
})

context("Downloading data")

test_that("can download data from remote location", {
  
  datadir <- tempfile('flycircuit_testdata')
  dbdir <- file.path(datadir,'db')
  bigmatdir <- file.path(datadir,'bigmat')
  ffdir <- file.path(datadir,'ff')
  dir.create(datadir)
  dir.create(dbdir)
  dir.create(bigmatdir)
  dir.create(ffdir)
  on.exit(unlink(datadir, recursive=TRUE))

  # Overwrite directory options for this test
  op=options(flycircuit.datadir=datadir, flycircuit.dbdir=dbdir, 
             flycircuit.bigmatdir=bigmatdir, flycircuit.ffdir=ffdir)
  on.exit(options(op), add=TRUE)
  
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/data', 
                   type='data', quiet=TRUE)
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/db', 
                   type='db', quiet=TRUE)
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/bigmat.desc', 
                   type='bigmat', quiet=TRUE)
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/mat.ff', 
                   type='ff', quiet=TRUE)

  expect_true(file.exists(file.path(datadir, 'data')))
  expect_true(file.exists(file.path(dbdir, 'db')))
  expect_true(file.exists(file.path(bigmatdir, 'bigmat.desc')))
  expect_true(file.exists(file.path(bigmatdir, 'bigmat')))
  expect_true(file.exists(file.path(ffdir, 'mat.ff')))
  expect_true(file.exists(file.path(ffdir, 'mat.ffrds')))
  
  expect_error(fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/wurgle', 
                   type='data', quiet=TRUE))
})

test_that("downloaded ff can be attached and have their backing path corrected", {
  # Overwrite directory options for this test
  ffdir <- file.path(tempfile(),'ff')
  dir.create(ffdir, recursive=TRUE)
  on.exit(unlink(ffdir, recursive=TRUE))
  op=options(flycircuit.ffdir=ffdir)
  on.exit(options(op), add=TRUE)
  
  fc_download_data('http://flybrain.mrc-lmb.cam.ac.uk/fcremtest/mat.ff',
                   type='ff', quiet=TRUE)
  mat=fc_attach_ff('mat')
  expect_true(exists('mat',envir=.extdata))
  expect_equivalent(attr(attr(mat, 'physical'), 'filename'), 
                    file.path(getOption('flycircuit.ffdir'), 'mat.ff'))
})
