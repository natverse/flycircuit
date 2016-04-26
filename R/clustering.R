#' Cluster a set of FlyCircuit neurons identified by gene_name
#' 
#' @description Given a vector of flycircuit gene/neuron names or neuronids use 
#'   hclust to carry out a hierarchical clustering. The default value of distfun
#'   will handle square distance matrices and R dist objects. Note that hclustfc
#'   is a thin wrapper around the \code{\link[nat.nblast]{nhclust}} function and that 
#'   is what you want to use if you have calculated a score matrix yourself e.g.
#'   for a set of all by all pairwise neurons similarities computed by
#'   \code{nblast} or \code{nblast_allbyall}.
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
#' @seealso \code{\link{fc_gene_name}, \link[nat.nblast]{nhclust}, 
#'   \link{hclust}, \link{dist}, \link[nat.nblast]{plot3d.hclust}}
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


#' Plot exemplar neurons/all cluster members after clustering by affinity
#' propagation
#' 
#' For each cluster, savefun receives the exemplar, the other neurons for the 
#' current cluster, all exemplars and the cluster_id (numeric). selected is a
#' list with 0 or 1 named entries for each neuron. An entry can contain up to 3
#' fields: * keepcluster (TRUE/FALSE) * keep        (character vector of
#' neurons) * reject      (character vector of neurons)
#' @param x An APResult object from apcluster
#' @param plot Whether to plot exemplars or all members of each cluster
#' @param suppressPlot logical indicating that plots should be suppressed.
#' @param savefun Optional function called after each plot is shown
#' @param clusters Names of subset of clusters to plot
#' @param soma Whether to plot somata (default TRUE)
#' @param Verbose logical indicating that output should be verbose.
#' @param selected A list selecting some of the neurons from each cluster (see
#'   Details)
#' @param selected_file an optional path to a yaml file to load a selection
#'   from.
#' @param yaml logical indicating that selections should be saved as yaml files,
#'   not rda files.
#' @param col Optional argument passed to \code{plot3dfc}.
#' @param ... options passed to plot3dfc when plot=exemplars
#' @return list with names of selected neurons for each cluster
#' @importFrom yaml yaml.load_file
#' @export
#' @seealso \code{\link[apcluster]{apcluster}}
#' @examples
#' \dontrun{
#' myselection=plot3d(apres15kv2ns,'bycluster')
#' myselection=LoadObjsFromRda('myselectionfile.rda')[[1]]
#' mynewselection=plot3d(apres15kv2ns,'bycluster',selected=myselection)
#' }
plot3d.APResult<-function(x,plot=c("exemplars","bycluster","all"),suppressPlot=FALSE,
                          savefun=NULL,clusters=NULL,soma=TRUE,Verbose=TRUE,selected=list(),
                          selected_file=NULL, yaml=TRUE, col=NULL,...){
  plot=match.arg(plot)
  if(is.null(clusters)){
    exemplars=names(x@exemplars)
  } else {
    if(is.numeric(clusters)){
      # assume that we've been given the numeic id of clusters
      # (nb not flycircuit idids)
      exemplars=names(x@exemplars)[clusters]
    } else exemplars=intersect(clusters,names(x@exemplars))
  }
  

  if(plot=='exemplars'){
    return(plot3dfc(exemplars,soma=soma,col=col,...))
  }
  if(plot=='all'){
    df=as.data.frame(x,exemplars)
    if(!is.null(clusters)) df <- droplevels(df[df$cluster %in% clusters, ])
    if(is.null(col)) col=rainbow(nlevels(df$exemplar))[df$exemplar]
    return(plot3dfc(df$item,soma=soma,col=col,...))
  }
  
  if(length(selected)==0 && !is.null(selected_file) && file.exists(selected_file)) {
    selected=yaml.load_file(selected_file)
    if(!all(names(selected) %in%names(x@exemplars))) stop("Mismatch between exemplars in selection file and apres")
  }
  i=1
  
  cluster_ids=match(exemplars,names(x@exemplars))
  while(i <= length(exemplars) & i>0 ){
    exemplar=exemplars[i]
    cluster_id=match(exemplar,names(x@exemplars))
    j=which(names(x@exemplars)==exemplar)
    if(Verbose) {
      cat("Cluster:",j,'exemplar:',exemplar,"\n")
      selex=selected[[exemplar]]
      extrafields=setdiff(names(selex),c("keep",'selected'))
      if(!is.null(selex) && length(extrafields))
        print(selex[extrafields])
    }
    others=setdiff(names(x@clusters[[j]]),exemplar)
    if(!suppressPlot) pl=plot3dfc(exemplar,lwd=4,soma=soma,...)
    if(length(others)>0){
      if(!is.null(selected[[exemplar]][['keep']])){
        # we have a subset of neurons that we have selected from this cluster
        # just plot those
        others=intersect(others,selected[[exemplar]]$keep)
      } 
      if(!suppressPlot) pl=c(pl,plot3dfc(others,soma=soma,...))
    }
    
    if(is.null(savefun)){
      chc=readline("Return (or f) [forward], b [back], c [cancel], s [select], p [pause], j [jump], k [keep individuals], r [reject individuals], l [clear individuals], d [save to disk]: ")
      if(chc=="c") {
        rgl.pop(unlist(pl),type='shape')
        break
      }
      if(chc=="s") {
        if(is.null(selected[[exemplar]]$keepcluster)){
          message("Selected cluster: ",exemplar)
          selected[[exemplar]]$keepcluster=TRUE
        } else {
          # remove the selection field for this exemplar
          message("Deselected cluster: ",exemplar)
          selected[[exemplar]]$keepcluster=NULL
          # if there are no other fields in the sublist for this cluster, 
          # remove the sublist as well
          if(length(selected[[exemplar]])==0) selected[[exemplar]]=NULL
        }
      }
      if(chc=='l'){
        selected[[exemplar]]$keep=NULL
        if(length(selected[[exemplar]])==0) selected[[exemplar]]=NULL
        message("Clearing selected individuals for cluster: ",exemplar)
      }
      else if(chc=='d'){
        # save selection to disk
        if(is.null(selected_file)) selected_file=file.choose(new=TRUE)
        if(yaml){
          if(!grepl("\\.yaml$",selected_file)) selected_file=paste(selected_file,sep="",".yaml")
          message("Saving selection to disk as ",selected_file)
          writeLines(as.yaml(selected),con=selected_file)
        } else {
          if(!grepl("\\.rda$",selected_file)) selected_file=paste(selected_file,sep="",".rda")
          save(selected,file=selected_file)
          message("Saving selection to disk as ",selected_file)
        }
      }
      else if(chc=='p') {
        message("You can now carry out interactive R commands. ",
                "To assign to global variables, use <<-.\n",
                "To continue plotting, press return on an empty line (?browser for more info)")
        browser()
      }
      else if(chc=='k' || chc=='r') {
        cur_rgl_window=rgl.cur()
        # get camera orientation
        op=par3d()[c("userMatrix","zoom","windowRect")]
        nopen3d()
        # use same camera orientation
        par3d(op)
        plot3dfc(exemplar,lwd=3,soma=soma)
        selected_from_scan=dpscan(others,soma=soma)
        if(!is.null(selected_from_scan)){
          # invert selection if we asked to reject not select
          if(chc=='r') selected_from_scan=setdiff(others,selected_from_scan)
          if(length(selected_from_scan)==0)
            selected[[exemplar]]$keep=NULL
          else 
            selected[[exemplar]]$keep=selected_from_scan
          rejected=setdiff(others,selected_from_scan)
          if(length(selected_from_scan))
            plot3dfc(selected_from_scan,soma=soma)
          message("Selected neurons: ",paste(selected[[exemplar]]$keep,collapse=" "))
          message("Rejected neurons: ",paste(rejected,collapse=" "))
          readline("Showing current selection. Press return to continue")
        } else {
          readline("Aborted selection process. Press return to continue")
        }
        rgl.close()
        rgl.set(cur_rgl_window)
      }
      else if(chc=="b") i=i-1
      else if(chc=="j") {
        # we're going to jump
        while(TRUE){
          jumptarget=readline("Cluster to jump to or c to cancel:")
          if(jumptarget=='c') break
          # numeric
          if(!is.na(as.integer(jumptarget))){
            # assume this refers to cluster_id of
            jumptarget=as.integer(jumptarget)
            if(jumptarget%in%cluster_ids){
              i=which(cluster_ids==jumptarget)
              break
            }
            else message(jumptarget," is outside range of current clusters")
          } else {
            # assume character
            cand=pmatch(jumptarget,exemplars)
            if(is.na(cand))
              message(jumptarget," does not partially match any current exemplar")
            else {
              i=cand
              break
            }
          }
        }
      }
      else if(nchar(chc)>1){
        # maybe a note that we are going to keep
        if(grepl("=",chc)){
          key=sub("^([^=]+)=.*$","\\1",chc)
          value=sub("^[^=]+=(.*)$","\\1",chc)
          if(is.null(selected[[exemplar]]))
            selected[[exemplar]]=list()
          selected[[exemplar]][[key]]=value
        }
      }
      else i=i+1
    } else {
      savefun(exemplar,others,names(x@exemplars),cluster_id)
      i=i+1
    }
    if(!suppressPlot) try(rgl.pop(unlist(pl),type='shape'))
  }
  selected
}


#' Affinity propagation clustering of FlyCircuit neurons
#'
#' @details Given a vector of gene_names/neuron names or neuronids use apcluster
#' to carry out a hierarchical clustering. The default value of FUN
#' will handle square distance matrices and R.
#' 
#' The default input preference of 0 has been chosen because an nblast2 score
#' greater than 0 indicates that pair of neurons show some similarity.
#' 
#' Note that the distance matrix will be converted to a similarity matrix before
#' use with apcluster by calculating 1-d.
#' 
#' maxneurons of 4000 has been chosen to prevent inadvertent clustering of huge
#' numbers of neurons. 4000 is reasonable on a decent laptop.
#' @param gns flycircuit identifiers (passed to \code{\link{fc_gene_name}}).
#' @param p input preference (default 0).
#' @param ... additional parameters passed to \code{\link[apcluster]{apcluster}}.
#' @param scoremat name of a file backed matrix containing raw scores.
#' @param FUN an (optional) function to apply to the mean normalised scores 
#' returned by \code{fc_subscoremat}. Default \code{NULL}.
#' @param maxneurons error out if we have more than this many neurons.
#' @return An object of class \code{\link[apcluster]{APResult}}.
#' @importFrom apcluster apcluster
#' @export
#' @seealso \code{\link[apcluster]{apcluster},\link{fc_gene_name}}
#' @examples
#' \dontrun{
#' library(nat)
#' 
#' apres <- apclusterfc(names(kcs20))
#' 
#' # Plot cluster exemplars
#' plot3d(apres, db=kcs20)
#' 
#' # Interactively step through clusters, with the examplar plotted in black
#' clear3d()
#' plot3d(apres, db=kcs20, plot='bycluster')
#' }
apclusterfc <- function(gns, p=0, ..., scoremat=getOption('flycircuit.scoremat'), 
                      FUN=NULL, maxneurons=4000) {
  if (!is.na(maxneurons) && length(gns) > maxneurons) {
    stop("Too many neurons! Use maxneurons to override if you're sure.")
  }
  scores=fc_subscoremat(gns, gns, scoremat=scoremat, distance=FALSE,
                        normalisation='mean')
  # nb apcluster requires square similarity matrix
  if(!is.null(FUN)) {
    FUN=match.fun(FUN)
    scores=FUN(scores)
  }
  apcluster(scores, p=p, ...)
}