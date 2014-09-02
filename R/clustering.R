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


#' Convert APResult (apcluster output) to dataframe
#' 
#' This can then be used for plots, clustering etc.
#' 
#' @param x an \code{APResult} object.
#' @param row.names ignored.
#' @param optional logical. If \code{TRUE}, setting row names and converting column names (to syntactic names: see make.names) is optional.
#' @param clusters a character vector of the names of the exemplars to include.
#' @param ... extra arguments to pass to \code{data.frame}.
#' @return A \code{data.frame} with columns \code{exemplar}, \code{cluster},
#'   \code{idx}, \code{item}.
#' @export
#' @seealso \code{\link[apcluster]{apcluster}},\code{\link[apcluster]{APResult}}
as.data.frame.APResult <- function(x, row.names=NULL, optional=FALSE, clusters, ...) {
  if(missing(clusters))
    exemplars=names(x@exemplars)
  else
    exemplars=intersect(clusters,names(x@exemplars))
  
  clusterids=which(names(x@exemplars)%in%exemplars)
  
  clusters=x@clusters[clusterids]
  cls=sapply(clusters,length)
  ulc=unlist(clusters)
  df=data.frame(exemplar=factor(rep(exemplars,cls)),
                cluster=rep(clusterids,cls),
                idx=ulc,item=names(ulc),row.names=names(ulc),optional=optional,stringsAsFactors=FALSE,...=...)
  df
}