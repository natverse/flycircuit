context("Test loading data")

test_that("load_fcdata can load local file", {
  load_fcdata('fcidtable')
  expect_true(exists('fcidtable'))
})