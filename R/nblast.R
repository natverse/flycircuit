#' Return (normalised) nblast similarity scores for FlyCircuit neurons
#' 
#' Precomputed scores are stored in bigmatrix form on disk. The bigmatrix object
#' is created in the global environment once per session and then reused. When 
#' normalised is TRUE, scores are in the interval [-1, 1] or, more exactly, 
#' range(scorematrix). Higher is always better. NB normalised scores are the 
#' mean of the normalised forward and reverse scores (as used for clustering in 
#' hclustfc) but are similarity scores (not distances).
#' @details See the package vignette for an examlple of how to download a 
#'   precomputed score matrix.
#' @param query,target Vectors of FlyCircuit identifiers
#' @param scorematname The score matrix to use. When NULL will default to 
#'   "abc2.normdmat" if \code{normalised=TRUE}, \code{allbyallblastcv2.5.bin} 
#'   otherwise.
#' @param normalised Logical indicating whether to return normalised scores.
#' @param ... A
#' @return Matrix of scores, columns are query neurons, rows, target.
#' @seealso \code{\link{hclustfc},\link{fc_sub_distmat}}
#' @export
fc_nblast <- function(query, target, scorematname=NULL, normalised=FALSE){
  if(is.null(scorematname))
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
  if(normalised){
    # check if we have been given a distance or similarity matrix by looking at
    # the first diagonal - should be 1 for a similarity matrix, 0 for distance
    first_diagonal_term=scoremat[1, 1]
    if(!first_diagonal_term%in%c(0,1)){
      warning("Normalised matrices should have a 0 or 1 diagonal. ",
              "Assuming this is a similarity matrix")
      scoremat[target, query]
    } else if(first_diagonal_term==0){
      # a distance matrix - we want a similarity matrix
      1 - scoremat[target, query]
    } else scoremat[target, query]
  }
  else scoremat[target, query]
}
