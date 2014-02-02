#' Cluster a set of FlyCircuit neurons identified by gene_name
#'
#' Given a vector of gene/neuron names or neuronids use hclust to carry out a
#' hierarchical clustering. The default value of distfun will handle square
#' distance matrices and R.
#' @param gns FlyCircuit identifiers (passed to fc_gene_name).
#' @param meth Clustering method (default Ward's).
#' @param distmat The distance matrix (default allbyallblast.canon).
#' @param distfun Function to convert distmat into R dist object (default=as.dist).
#' @param ... Additional parameters passed to hclust.
#' @return An object of class \code{\link{hclust}} which describes the tree produced by the clustering process.
#' @export
#' @seealso \code{\link{fc_gene_name}}, \code{\link{hclust}}, \code{\link{dist}}
hclustfc <- function(gns, method='ward', distmat="abc2.normdmat", distfun=as.dist, ..., maxneurons=4000) {
  subdistmat <- fc_sub_distmat(gns, distmat, maxneurons=maxneurons)
  if(min(subdistmat) < 0)
    stop("Negative distances not allowed. Are you sure this is a distance matrix?")
  hclust(as.dist(subdistmat), method=method, ...)
}

#' Return a subset of a distance matrix stored in a file-backed matrix
#'
#' @param gns FlyCircuit identifiers (passed to fc_gene_name).
#' @param distmat The distance matrix (default allbyallblast.canon).
#' @param form The type of object to return.
#' @return return An object of class matrix or dist (as determined by the form
#'   argument), corresponding to a subset of the distance matrix
#' @export
fc_sub_distmat <- function(gns, distmat="abc2.normdmat", form=c('matrix', 'dist'), maxneurons=NA){
  form <- match.arg(form)
  distmat <- fc_attach_bigmat(distmat)
  gns <- fc_gene_name(gns)
  gns.sel <- intersect(rownames(distmat), gns)
  if(!is.na(maxneurons) && length(gns.sel) > maxneurons) {
    stop("Too many neurons! Use maxneurons to override if you're sure.")
  }
  nmissing <- length(gns) - length(gns.sel)
  if(nmissing > 0)
    warning("Dropping ", nmissing, "neurons")
  d <- distmat[gns.sel,gns.sel]
  if(form=='matrix') d
  else as.dist(d)
}

#' Plot dotprops coloured by groups cut from an hclust object
#'
#' @details Note that the colours are in the order of the dendrogram as assigned
#'   by colour_clusters.
#' @param hc An hclust object.
#' @param k Number of clusters to cut from hclust object.
#' @param h Height to cut hclust object.
#' @param groups Numeric vector of groups to plot.
#' @param col Colours for groups (directly specified or a function).
#' @return List of rgl ids for plotted objects.
#' @export
#' @seealso \code{\link{hclust},\link{slice},\link{colour_clusters}}
#' @importFrom dendroextras slice
plot3d.hclust <- function(hc, k=NULL, h=NULL, groups, col=rainbow, ...) {
  # Cut the dendrogram into k groups of neurons. Note that these will now have
  # the neurons in dendrogram order
  kgroups <- slice(hc,k,h)
  k <- max(kgroups)
  if(is.function(col))
    col <- col(k)

  neurons <- names(kgroups)

  if(!missing(groups)){
    matching <- kgroups%in%groups
    kgroups <- kgroups[matching]
    neurons <- neurons[matching]
  }
  plot3dfc(neurons, col=col[kgroups], ...)
}
