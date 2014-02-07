context("Blasting")

fc_download_data('http://jefferislab.org/si/nblast/flycircuit/kcs20scores.desc', type='bigmat', quiet=TRUE)
library(nat)
kcs20 <- read.neuronlistfh('http://jefferislab.org/si/nblast/flycircuit/kcs20.rds', getOption('flycircuit.datadir'), quiet=TRUE)

# Load raw NBLAST scores
scores <- readRDS("../testdata/kcs20scores.rds")
nn=rownames(scores)

test_that('fc_subscoremat subsets correctly',{
  expect_equivalent(fc_subscoremat(nn,nn,scoremat=scores),scores)
  expect_equivalent(fc_subscoremat(nn,nn[1],scoremat=scores),scores[1,])
  expect_equivalent(fc_subscoremat(nn[1],nn,scoremat=scores),scores[,1])
})
  
test_that('fc_subscoremat complains if asked to return distances without normalisation', {
  # The scores are the average of the forward and reverse scores, and so there
  # is no way of obtaining a non-'normalised' distance
  expect_error(fc_subscoremat(nn[1:5],nn[1:5],scoremat=scores,distance=T))
})

normscores <- apply(scores[1:5, 1:5], 2, function(x) x / max(x))
test_that('fc_subscoremat can return normalised forward scores', {
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:5], scoremat=scores, distance=F, normalisation='normalised'), normscores)
})

avgscores <- (normscores + t(normscores)) / 2
test_that('fc_subscoremat can return scores that are the average of the forwards and reverse directions', {
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:5], scoremat=scores, distance=F, normalisation='mean'), avgscores)
})

distances <- 1 - avgscores
test_that('fc_subscoremat can return distances created from the average of the forwards and reverse directions', {
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:5], scoremat=scores, distance=T, normalisation='mean'), distances)
})


test_that('we can supply an ff matrix and get expected results', {
  scoresff=as.ff(scores)
  expect_equivalent(fc_subscoremat(nn,nn,scoremat=scoresff),scores)
  expect_equivalent(fc_subscoremat(nn,nn[1],scoremat=scoresff),scores[1,])
  expect_equivalent(fc_subscoremat(nn[1],nn,scoremat=scoresff),scores[,1])
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:5], scoremat=scoresff, distance=F,
                                   normalisation='normalised'), normscores)
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:5], scoremat=scoresff, distance=T,
                                   normalisation='mean'), distances)
})
