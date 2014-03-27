context("FlyCircuit identifiers")

test_that('convert between different FlyCircuit identifiers',{
  expect_equal(fc_gene_name(1), "FruMARCM-M002262_seg001")
  expect_equal(fc_gene_name("FruMARCM-M002262_seg001"), "FruMARCM-M002262_seg001")
  expect_equal(fc_gene_name("fru-M-200266"), "FruMARCM-M002262_seg001")
  expect_equal(fc_idid("FruMARCM-M002262_seg001"), 1L)
  expect_equal(fc_idid("fru-M-200266"), 1L)
  expect_equal(fc_idid(1L), 1L)
  expect_equal(fc_neuron(1L), "fru-M-200266")
  expect_equal(fc_neuron("fru-M-200266"), "fru-M-200266")
  expect_equal(fc_neuron("FruMARCM-M002262_seg001"), "fru-M-200266")
})

test_that('turn a path into a FlyCircuit gene_name',{
  expect_equal(fcgn_forfile(file.path('FruMARCM-M002373_seg002_03.nrrd-files',
                                      'FruMARCM-M002373_seg002_03.nrrd.am'),
                            checkExists = TRUE),
               'FruMARCM-M002373_seg002')
})