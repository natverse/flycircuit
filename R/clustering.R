#' Cluster a set of FlyCircuit neurons identified by gene_name
#' 
#' @description Given a vector of gene/neuron names or neuronids use hclust to 
#'   carry out a hierarchical clustering. The default value of distfun will 
#'   handle square distance matrices and R.
#'   
#' @details when \code{method} is ward, ward.D or ward.D2 it may make sense to 
#'   unsquare the resultant distance before plotting.
#'   
#' @param gns FlyCircuit identifiers (passed to fc_gene_name).
#' @param unsquare Whether to return the square root of the distance calculated
#'   by \code{hclust} (see details, default \code{FALSE}).
#' @inheritParams nat.nblast::nhclust
#'   
#' @export
#' @family scoremats
#' @seealso \code{\link{fc_gene_name}, \link{hclust}, \link{dist}, 
#'   \link[nat.nblast]{plot3d.hclust}}
#' @examples
#' data(kcs20, package='nat')
#' hckcs=hclustfc(names(kcs20))
#' # plot basic dendrogram
#' plot(hckcs)
#' # plot dendrogram using unsquared distance
#' plot(hclustfc(names(kcs20), unsquare=TRUE))
#' # divide hclust object into 3 groups
#' library(dendroextras)
#' plot(colour_clusters(hckcs, k=3))
#' # 3d plot of neurons in those clusters (with matching colours)
#' library(nat)
#' plot3d(hckcs, k=3, db=kcs20)
#' # names of neurons in 3 groups
#' subset(hckcs, k=3)
hclustfc <- function(gns, method='ward', scoremat=getOption('flycircuit.scoremat'),
                     unsquare=FALSE, distfun=as.dist, ..., maxneurons=4000) {
  if(is.character(scoremat)) scoremat <- fc_attach_bigmat(scoremat)
  hc=nat.nblast::nhclust(fc_gene_name(gns), method=method, scoremat=scoremat,
                      distfun=distfun, maxneurons=maxneurons, ...)
  if(unsquare) hc$height=sqrt(hc$height)
  hc
}
