context("Blasting")

fc_download_data('http://jefferislab.org/si/nblast/flycircuit/kcs20scores.desc', type='bigmat', quiet=TRUE)
library(nat)
kcs20 <- read.neuronlistfh('http://jefferislab.org/si/nblast/flycircuit/kcs20.rds', getOption('flycircuit.datadir'), quiet=TRUE)

test_that("fc_nblast returns correct scores", {
  scores <- fc_nblast(names(kcs20[1:5]), names(kcs20[1:5]), 'kcs20scores')
  expectedscores <- structure(c(0, 0.879701774928044, 1.09036313166767, 0.856098554157773, 1.02160805621394, 0.879701774928044, 0, 1.02431095296207, 1.00101232979235, 0.978638909712389, 1.09036313166767, 1.02431095296207, 0, 0.904209455784958, 0.526020923421957, 0.856098554157773, 1.00101232979235, 0.904209455784958, 0, 0.928713797099109, 1.02160805621394, 0.978638909712389, 0.526020923421957, 0.928713797099109, 0), .Dim = c(5L, 5L), .Dimnames = list(c("FruMARCM-M001205_seg002", "GadMARCM-F000122_seg001", "GadMARCM-F000050_seg001", "GadMARCM-F000142_seg002", "FruMARCM-F000270_seg001"), c("FruMARCM-M001205_seg002", "GadMARCM-F000122_seg001", "GadMARCM-F000050_seg001", "GadMARCM-F000142_seg002", "FruMARCM-F000270_seg001")))

  expect_equal(scores, expectedscores)
})

test_that('',{
  scores=readRDS("../testdata/kcs20scores.rds")
  nn=rownames(scores)
  expect_equivalent(fc_subscoremat(nn,nn,scoremat=scores),scores)
  
  expect_equivalent(fc_subscoremat(nn,nn[1],scoremat=scores),scores[1,])
  expect_equivalent(fc_subscoremat(nn[1],nn,scoremat=scores),scores[,1])
  
  distances5 <- structure(c(0, 0.879701774928044, 1.09036313166767, 0.856098554157773, 1.02160805621394, 0.879701774928044, 0, 1.02431095296207, 1.00101232979235, 0.978638909712389, 1.09036313166767, 1.02431095296207, 0, 0.904209455784958, 0.526020923421957, 0.856098554157773, 1.00101232979235, 0.904209455784958, 0, 0.928713797099109, 1.02160805621394, 0.978638909712389, 0.526020923421957, 0.928713797099109, 0), .Dim = c(5L, 5L), .Dimnames = list(c("FruMARCM-M001205_seg002", "GadMARCM-F000122_seg001", "GadMARCM-F000050_seg001", "GadMARCM-F000142_seg002", "FruMARCM-F000270_seg001"), c("FruMARCM-M001205_seg002", "GadMARCM-F000122_seg001", "GadMARCM-F000050_seg001", "GadMARCM-F000142_seg002", "FruMARCM-F000270_seg001")))
  expect_equivalent(fc_subscoremat(nn[1:5],nn[1:5],scoremat=scores,distance=T,normalisation='mean'),distances5)
})
