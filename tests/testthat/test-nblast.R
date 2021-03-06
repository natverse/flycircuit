context("Blasting")

library(nat)
kcs20 <- read.neuronlistfh('http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/kcs20.rds', getOption('flycircuit.datadir'), quiet=TRUE)

op=options(flycircuit.scoremat = 'kcs20scores')

# Use sample raw NBLAST scores from package
scores <- kcs20scores
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

test_that('fc_subscoremat for square matrices gives same as fetching scores independently', {
  dists5.mean=fc_subscoremat(nn[1:5], nn[1:5], scoremat=scores, distance=T, normalisation='mean')
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:2], scoremat=scores, distance=T, normalisation='mean'), dists5.mean[1:2,])
  expect_equivalent(fc_subscoremat(nn[1:2], nn[1:5], scoremat=scores, distance=T, normalisation='mean'), dists5.mean[,1:2])
  
  expect_equivalent(fc_subscoremat(nn[1], nn[1], scoremat=scores, distance=T, normalisation='mean'), dists5.mean[1,1])
  
  dists5.normdist=fc_subscoremat(nn[1:5], nn[1:5], scoremat=scores, distance=T, normalisation='normalised')
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:2], scoremat=scores, distance=T, normalisation='normalised'), dists5.normdist[1:2,])
  expect_equivalent(fc_subscoremat(nn[1:2], nn[1:5], scoremat=scores, distance=T, normalisation='normalised'), dists5.normdist[,1:2])

  dists5.norm=fc_subscoremat(nn[1:5], nn[1:5], scoremat=scores, normalisation='normalised')
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:2], scoremat=scores, normalisation='normalised'), dists5.norm[1:2,])
  expect_equivalent(fc_subscoremat(nn[1:2], nn[1:5], scoremat=scores, normalisation='normalised'), dists5.norm[,1:2])
  
  dists5.raw=fc_subscoremat(nn[1:5], nn[1:5], scoremat=scores, normalisation='raw')
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:2], scoremat=scores, normalisation='raw'), dists5.raw[1:2,])
  expect_equivalent(fc_subscoremat(nn[1:2], nn[1:5], scoremat=scores, normalisation='raw'), dists5.raw[,1:2])
})

test_that('we can supply an ff matrix and get expected results', {
  library(ff)
  scoresff=as.ff(scores)
  expect_equivalent(fc_subscoremat(nn,nn,scoremat=scoresff),scores)
  expect_equivalent(fc_subscoremat(nn,nn[1],scoremat=scoresff),scores[1,])
  expect_equivalent(fc_subscoremat(nn[1],nn,scoremat=scoresff),scores[,1])
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:5], scoremat=scoresff, distance=F,
                                   normalisation='normalised'), normscores)
  expect_equivalent(fc_subscoremat(nn[1:5], nn[1:5], scoremat=scoresff, distance=T,
                                   normalisation='mean'), distances)
})

test_that('fc_subscoremat and fc_nblast agree', {
  expect_equal(fc_nblast(nn[1],nn),fc_subscoremat(nn[1],nn,scoremat=scores))
  expect_equal(fc_nblast(nn,nn[1]),fc_subscoremat(nn,nn[1],scoremat=scores))
  expect_equal(fc_nblast(nn,nn[1],normalisation='mean'),
               fc_subscoremat(nn,nn[1],scoremat=scores,normalisation='mean',distance=FALSE))
})

options(op)
