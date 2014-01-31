context("flycircuit identifiers")
test_that('convert between different flycircuit identifiers',{
  expect_equal(fc_gene_name(1),"FruMARCM-M002262_seg001")
  expect_equal(fc_idid("FruMARCM-M002262_seg001"),1L)
  expect_equal(fc_idid("fru-M-200266"),1L)
  expect_equal(fc_neuron(1L),"fru-M-200266")
})
