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
#' @inheritParams fc_subscoremat
#' @param normalised Logical indicating whether to return normalised scores.
#' @return Matrix of scores, columns are query neurons, rows, target.
#' @seealso \code{\link{hclustfc}}
#' @export
fc_nblast <- function(query, target, scoremat=getOption('flycircuit.scoremat'),
                      normalised=FALSE){
  fc_subscoremat(query, target, scoremat=scoremat,
                 normalisation=ifelse(normalised,'mean','raw'))
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
#'   matrix. Defaults to value of \code{options(flycircuit.scoremat)}.
#' @param distance Logical indicating whether to return distances or scores.
#' @param normalisation The type of normalisation procedure that should be 
#'   carried out, selected from  \code{'raw'}, \code{'normalised'} or 
#'   \code{'mean'} (i.e. the average of normalised scores in both directions). 
#'   If \code{distance=TRUE} then this cannot be raw.
#' @export
#' @seealso \code{\link{big.matrix}, \link{ff}}
fc_subscoremat<-function(query, target, scoremat=getOption('flycircuit.scoremat'),
                         distance=FALSE, normalisation=c('raw', 'normalised', 'mean')){
  normalisation <- match.arg(normalisation)
  if(distance && normalisation=='raw')
    stop("raw scores are always similarity scores")
  
  if(is.character(scoremat)) scoremat <- fc_attach_bigmat(scoremat)
  
  nat.nblast::sub_score_mat(query = query, target = target, 
                            scoremat = scoremat, distance = distance,
                            normalisation = normalisation)
}

# utility function to extract diagonal terms from matrices
# uses the 'diagonal' attribute when available
#' @importFrom bigmemory is.big.matrix
#' @importFrom ff is.ff arrayIndex2vectorIndex
diagonal<-function(x, indices=NULL){
  if(!isTRUE(nrow(x)==ncol(x))) stop("x is not a square matrix!")
  
  if(is.character(indices)) indices=match(indices,rownames(x))
  if(!is.null(xdiag<-attr(x,'diagonal'))){
    return(if(is.null(indices)) xdiag else xdiag[indices])
  }

  if(is.logical(indices)) indices=which(indices)
  
  if(is.ff(x)){
    # convert array indices to vector indices
    if(is.null(indices)) indices=seq.int(nrow(x))
    vidxs=arrayIndex2vectorIndex(cbind(indices,indices),dim=dim(x))
    x[vidxs]
  } else if(is.big.matrix(x)) {
    ndiags <- if(is.null(indices)){
      nrow(x)
    } else {
      length(indices)
    }
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
