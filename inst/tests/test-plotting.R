context("Plotting")

test_that("FlyCircuit neurons can be plotted", {
  plot3d(fc_gene_name(1))
})

test_that("hclust objects are plottable in 3d", {
  hclustres <- hclustfc(fc_gene_name(1:20))
  expect_true({plot3d(hclustres, h=3); TRUE})
})

test_that("fcwbsurf works", {
  fcwbsurf()
})
