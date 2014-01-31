context("Clustering")

oldbigmatdir <- getOption('flycircuit.bigmatdir')
options('flycircuit.bigmatdir' = '~/projects/ChiangReanalysis/data/bigmatrix/')

test_that("hclustfc can cluster based on gene names", {
  hclustres <- hclustfc(fc_gene_name(1:20))
  expect_equal(length(hclustres$order), 20)
})

options('flycircuit.bigmatdir' = oldbigmatdir)
