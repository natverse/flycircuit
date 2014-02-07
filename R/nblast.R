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
  scoremat <- if(is.character(scorematname)) fc_attach_bigmat(scorematname) else scorematname
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


#' Return scores (or distances) for given query and target neurons
#' 
#' Scores can either be returned as raw numbers, normalised such that a self-hit
#' has score 1, or as the average of the normalised scores in both the forwards 
#' & reverse directions (i.e. \code{|query->target| + |target->query| / 2}). 
#' Distances are returned as either \code{1 - normscore} in the forwards 
#' direction, or as \code{1 - normscorebar}, where \code{normscorebar} is 
#' \code{normscore} averaged across both directions.
#' @param query,target Vectors of FlyCircuit identifiers
#' @param scoremat A matrix, ff matrix, bigmatrix or a character vector
#'   specifiying the name of an ff matrix containing the all by all score
#'   matrix. Defaults to \code{'allbyallblastcv2.5.ff'}.
#' @param distance Logical indicating whether to return distances or scores.
#' @param normalisation The type of normalisation procedure that should be 
#'   carried out, selected from  \code{'raw'}, \code{'normalised'} or 
#'   \code{'mean'} (i.e. the average of normalised scores in both directions). 
#'   If \code{distance=TRUE} then this cannot be raw.
#' @export
#' @seealso \code{\link{big.matrix}, \link{ff}}
fc_subscoremat<-function(query, target, scoremat="allbyallblastcv2.5.bin",
                         distance=FALSE,
                         normalisation=c('raw', 'normalised', 'mean')){
  normalisation <- match.arg(normalisation)
  if(distance && normalisation=='raw')
    stop("raw scores are always similarity scores")
  
  if(is.character(scoremat)) scoremat <- fc_attach_bigmat(scoremat)
  
  available_gns <- rownames(scoremat)
  if(missing(target)) target <- available_gns
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
  
  # subsetting big matrices by name is slow
  qidxs=match(query,available_gns)
  tidxs=match(target,available_gns)
  fwdscores=scoremat[tidxs, qidxs, drop = FALSE]
  
  x <- if(normalisation %in% c('mean', 'normalised')) {
    # normalise fwdscores
    self_matches=diagonal(scoremat,qidxs)
    fwdscores = scale(fwdscores, center=FALSE, scale=self_matches)
    
    if(normalisation == 'mean') {
      # fetch reverse scores
      revscores=scoremat[qidxs, tidxs, drop = FALSE]
      # normalise revscores
      self_matches=diagonal(scoremat,tidxs)
      revscores = scale(revscores, center=FALSE, scale=self_matches)
      (fwdscores+t(revscores))/2
    } else {
      fwdscores
    }
  } else {
    fwdscores
  }
  # drop dimensions in the standard R way (including names etc)
  if(nrow(x)==1 || ncol(x)==1) x = x[seq_len(nrow(x)), seq_len(ncol(x))]
  if(distance) 1-x else x
}

# utility function to extract diagonal terms from matrices
#' @importFrom bigmemory is.big.matrix
diagonal<-function(x, indices=NULL){
  if(is.character(indices)) indices=match(indices,rownames(x))
  if(is.logical(indices)) indices=which(indices)
  ndiags <- if(is.null(indices)){
    nrow(x)
  } else {
    length(indices)
  }
  
  if(is.ff(x)){
    # convert array indices to vector indices
    vidxs=arrayIndex2vectorIndex(cbind(indices,indices),dim=dim(x))
    x[vidxs]
  } else if(is.big.matrix(x)) {
    diags=rep(NA,ndiags)
    for(i in seq_len(ndiags)){
      idx=indices[i]
      diags[i]=x[idx, idx]
    }
    diags
  } else {
    if(is.null(indices)) diag(x) else diag(x)[indices]
  }
}
