context("Test loading data")

test_that("load_fcdata can load local file", {
  load_fcdata('fcidtable')
  expect_true(exists('fcidtable'))
})

test_that("load_fcdb can load local database", {
  load_fcdb('annotation')
  expect_true(exists('annotation'))
})
