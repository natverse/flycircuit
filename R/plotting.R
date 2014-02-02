#' Plot FlyCircuit neurons using rgl
#'
#' * Plotted soma positions are a prediction based on location in the FlyCircuit
#' template brain mapped onto my selected template. The positions are looked up 
#' in the df attribute of the neuronlist or in a dataframe called somapos.
#' * Colour is rainbow(length(id)) when more than one neuron is plotted. It can
#' be specified as a function, a vector (length(id)) or a single recycled value.
#' * By default when more than 200 neurons are plotted in a single call rgl 
#' redraw is suspended. This can be >10x faster.
#' * Note that the ids of objects on the rgl stack are stored in a variable in
#' the global environment called \code{.last.plot3dfc} that can be used by
#' pop3dfc() to remove all objects plotted in last call to plot3dfc.
#' @param id Character vector of FlyCircuit gene names.
#' @param col Function or vector specifying colour.
#' @param db Object of class neuronlist (only tested for dotprops objects).
#' @param flip Logical vector (whether to use list containing flipped neurons). 
#'   Recycled if length 1.
#' @param soma Whether to show the estimated cell body position.
#' @param skipRedraw Numeric threshold above which to disable rgl redraw.
#' @return List of rgl stack ids (see rgl::plot3d).
#' @export
#' @seealso \code{\link[rgl]{plot3d}}
plot3dfc <- function(id, col, db=NULL, flip=F, soma=F, alpharange=NULL, skipRedraw=200, ...) {
  if(soma && (is.null(attr(dps, 'df')) && !exists('somapos', envir=.GlobalEnv))) {
    message("load_fcdb('somapos') to allow plotting of predicted soma positions")
    soma=FALSE
  }
  if(!is.list(id) && length(id) > 1) {
    if(missing(col))
      col <- rainbow
    if(is.function(col))
      col <- col(length(id))
    # Speed up drawing when there are lots of neurons
    if(is.numeric(skipRedraw)) skipRedraw <- ifelse(length(id) > skipRedraw, TRUE, FALSE)
    if(is.logical(skipRedraw)) {
      op <- par3d(skipRedraw=skipRedraw)
      on.exit(par3d(op))
    }
    rval <- mapply(plot3dfc, id, flip=flip, soma=soma, col=col, MoreArgs=list(db=db, alpharange=alpharange), ...)
    assign(".last.plot3dfc", rval, envir=.plotted3d)
    return(invisible(rval))
  }
  if(is.null(db)) {
    if(flip) db <- dpsflip
    else db <- dps
  }
  
  if(missing(col)) col <- 'black'
  id <- fc_gene_name(id)
  n <- db[[id]]
  rlist <- NULL
  if(!is.null(n)) {
    rlist <- plot3d(n, col=col, alpharange=alpharange, ...)
    if(soma) {
      df <- attr(db,'df')
      if(!is.null(df)) {
        # Attached dataframe gives soma positions
        rlist <- c(rlist, spheres3d(df[id, c("X", "Y", "Z")], radius=2, col=col))
      } else rlist <- c(rlist, spheres3d(somapos[id, c("X", "Y", "Z")], radius=2, col=col))
    }
  }
  # Save this info so we can pop stuff later
  assign(".last.plot3dfc", rlist, envir=.plotted3d)
  invisible(rlist)
}

#' Remove plotted FlyCircuit neurons
#' 
#' If no neurons are specified, the last plotted are removed.
#' @param x Neurons to remove
#' @export
#' @seealso \code{\link[rgl]{pop3d}}
#' @importFrom rgl pop3d
pop3dfc <- function(x, slow=FALSE, type='shapes') {
  if(missing(x)){
    if(exists(".last.plot3dfc", envir=.plotted3d))
      x <- get(".last.plot3dfc", envir=.plotted3d)
    else x <- NULL
  }
  if(slow) invisible(sapply(x, function(x) try(pop3d(x, type=type))))
  else try(pop3d(unlist(x), type=type))
}

#' Plot a 3D surface of the FlyCircuit reference brain
#'
#' @param col The colour of the surface
#' @param alpha The opacity of the surface
#' @param ... Extra arguments to pass to plot3d
#' @importFrom rgl plot3d
#' @export
fcwbsurf <- function(col='grey', alpha=0.3, ...) {
  if(!exists('FCWBSurf')) FCWBSurf <<- read.hxsurf(file.path(getOption('flycircuit.resourcesdir'), 'FCWB.surf'))
  plot3d(FCWBSurf, col=col, alpha=alpha, ...)
}

#' Plot 3D surfaces of the FlyLight neuropil segmentation in FlyCircuit coordinate space
#'
#' @param ... Extra arguments to pass to plot3d
#' @export
#' @importFrom rgl plot3d
fcwbnpsurf <- function(...) {
  if(!exists('FCWBNPSurf')) load(file.path(getOption('flycircuit.resourcesdir'), 'FCWBNPSurf.rda'), envir=globalenv())
  plot3d(FCWBNPSurf, ...)
}
