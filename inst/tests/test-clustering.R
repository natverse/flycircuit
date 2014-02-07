context("Clustering")

op=options(flycircuit.scoremat = 'kcs20scores')

test_that("hclustfc can cluster based on gene names", {
  data(kcs20, package='nat')
  expect_is(hclustres <- hclustfc(fc_gene_name(names(kcs20)), scoremat=kcs20scores), 'hclust')
  baseline = structure(list(merge = structure(c(-13L, -3L, -7L, -5L, 1L, -12L, 
  -18L, -6L, -4L, -16L, 8L, 6L, -8L, -11L, -2L, -1L, 5L, 13L, 17L, 
  -14L, -10L, -15L, -20L, 2L, -17L, -19L, 3L, -9L, 4L, 10L, 7L, 
  9L, 12L, 14L, 15L, 11L, 16L, 18L), .Dim = c(19L, 2L)), height = c(0.285343779083225, 
  0.318120876435248, 0.327012982805359, 0.334222346213642, 0.388433860338725, 
  0.505688402980099, 0.512102456584254, 0.529974332350765, 0.530270577109945, 
  0.669047775041585, 0.708738525974897, 0.716663180928045, 0.793166642479063, 
  0.807770254798785, 0.834929323267274, 0.905968380843084, 1.29189778465844, 
  1.56042566417595, 3.74081739964096), order = c(13L, 14L, 3L, 
  10L, 6L, 7L, 15L, 16L, 5L, 20L, 8L, 4L, 9L, 1L, 2L, 11L, 12L, 
  17L, 18L, 19L), labels = c("FruMARCM-M001205_seg002", "GadMARCM-F000122_seg001", 
  "GadMARCM-F000050_seg001", "GadMARCM-F000142_seg002", "FruMARCM-F000270_seg001", 
  "FruMARCM-F001115_seg002", "FruMARCM-M001051_seg002", "GadMARCM-F000423_seg001", 
  "ChaMARCM-F000586_seg002", "FruMARCM-M001339_seg001", "GadMARCM-F000476_seg001", 
  "FruMARCM-F000085_seg001", "FruMARCM-F000706_seg001", "FruMARCM-M000842_seg002", 
  "FruMARCM-F001494_seg002", "FruMARCM-F000188_seg001", "GadMARCM-F000071_seg001", 
  "FruMARCM-M000115_seg001", "GadMARCM-F000442_seg002", "FruMARCM-F001929_seg001"
  )), .Names = c("merge", "height", "order", "labels"))
  
  expect_equal(hclustres[1:4], baseline)
})

options(op)