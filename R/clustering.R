#' Cluster a set of FlyCircuit neurons identified by gene_name
#' 
#' Given a vector of gene/neuron names or neuronids use hclust to carry out a 
#' hierarchical clustering. The default value of distfun will handle square 
#' distance matrices and R.
#' 
#' @param gns FlyCircuit identifiers (passed to fc_gene_name).
#' @inheritParams nat.nblast::nhclust
#' 
#' @export
#' @family scoremats
#' @seealso \code{\link{fc_gene_name}, \link{hclust}, \link{dist}, \link[nat.nblast]{plot3d.hclust}}
#' @examples
#' data(kcs20, package='nat')
#' hckcs=hclustfc(names(kcs20))
#' # dividide hclust object into 3 groups
#' library(dendroextras)
#' plot(colour_clusters(hckcs, k=3))
#' # 3d plot of neurons in those clusters (with matching colours)
#' library(nat)
#' plot3d(hckcs, k=3, db=kcs20)
#' # names of neurons in 3 groups
#' subset(hckcs, k=3)
hclustfc <- function(gns, method='ward', scoremat=getOption('flycircuit.scoremat'), 
                     distfun=as.dist, ..., maxneurons=4000) {
  if(is.character(scoremat)) scoremat <- fc_attach_bigmat(scoremat)
  nat.nblast::nhclust(fc_gene_name(gns), method=method, scoremat=scoremat,
                      distfun=distfun, maxneurons=maxneurons, ...)
}
