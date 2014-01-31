#' Return (normalised) nblast similarity scores for FlyCircuit neurons.
#'
#' Precomputed scores are stored in bigmatrix form on disk. The bigmatrix object
#' is created in the global environment once per session and then reused. When
#' normalised is TRUE, scores are in the interval [-1, 1] or, more exactly,
#' range(scorematrix). Higher is always better. NB scores are the mean of the
#' normalised forward and reverse scores (as used for clustering in hclustfc)
#' but are similarity scores (not distances).
#' @param query,target Vectors of FlyCircuit identifiers
#' @param normalised Logical indicating whether to return normalised scores.
#' @return Matrix of scores, columns are query neurons, rows, target.
#' @seealso \code{\link{hclustfc},\link{fc_sub_distmat}}
fc_nblast <- function(query, target, normalised=FALSE, ...){
  require(bigmemory)
  # Note global assignment
  scorematname <- if(normalised) "abc2.normdmat" else "allbyallblastcv2.5.bin"
  scoremat <- fc_attach_bigmat(scorematname)
  available_gns <- rownames(scoremat)
  if(missing(target)) target <- rownames(scoremat)
  else {
    # Check what we were given
    target <- fc_gene_name(target)
    target_missing <- setdiff(target, available_gns)
    if(length(target_missing) > 0){
      warning("Dropping ", length(target_missing), " target neurons")
      target <- intersect(target, available_gns)
    }
  }
  
  if(missing(query)) query <- rownames(scoremat)
  else {
    query <- fc_gene_name(query)
    query_missing <- setdiff(query, available_gns)
    if(length(query_missing) > 0) {
      warning("Dropping ", length(query_missing), " query neurons")
      query <- intersect(query, available_gns)
    }
  }
  if(normalised)
    1 - scoremat[target, query]
  else
    scoremat[target, query]
}
