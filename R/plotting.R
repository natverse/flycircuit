#' Plot FlyCircuit neurons using rgl
#' 
#' @details \itemize{
#'   
#'   \item Plotted soma positions are a prediction based on location in the
#'   FlyCircuit template brain mapped onto my selected template. The positions
#'   are looked up in the df attribute of the neuronlist or in a dataframe
#'   called somapos.
#'   
#'   \item Colour is rainbow(length(id)) when more than one neuron is plotted. It
#'   can be specified as a function, a vector (length(id)) or a single recycled 
#'   value.
#'   
#'   \item By default when more than 200 neurons are plotted in a single call 
#'   rgl redraw is suspended. This can be >10x faster.
#'   
#'   \item Note that the ids of objects on the rgl stack are stored in a 
#'   variable in the global environment called \code{.last.plot3dfc} that can be
#'   used by \code{pop3dfc()} to remove all objects plotted in last call to 
#'   plot3dfc. }
#' @param id Vector of FlyCircuit gene or neuron names or idids (passed to 
#'   \code{\link{fc_gene_name}})
#' @param col Function or vector specifying colour.
#' @param db Object of class neuronlist (only tested for dotprops objects).
#' @param flip Logical vector (whether to use list containing flipped neurons). 
#'   Recycled if length 1.
#' @param soma Whether to show the estimated cell body position.
#' @param alpharange Range of alpha values for which vectors/dots should be 
#'   plotted..
#' @param skipRedraw Numeric threshold above which to disable rgl redraw.
#' @param ... Additional arguments for \code{plot3d}
#' @return List of rgl stack ids (see rgl::plot3d).
#' @export
#' @seealso \code{\link[rgl]{plot3d}, \link{pop3dfc}}
plot3dfc <- function(id, col, db=get(getOption('nat.default.neuronlist')), flip=F, soma=F, alpharange=NULL, skipRedraw=200, ...) {
  id=fc_gene_name(id)
  # drop any missing ids with a warning
  missing_ids=setdiff(id, names(db))
  if(nmissing<-length(missing_ids)){
    warning("Dropping ", nmissing,' neurons that are not present in db!')
    id=setdiff(id, missing_ids)
  }
  # bail if we have nothing to plot
  if(!length(id)){
    warning("No valid neurons to plot!")
    return(invisible(NULL))
  }
  
  if(!is.list(id) && length(id) > 1) {
    # set colours
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
        if(!is.na(df[id, 'X'])) rlist <- c(rlist, spheres3d(df[id, c("X", "Y", "Z")], radius=2, col=col))
      }
    }
  }
  # Save this info so we can pop stuff later
  assign(".last.plot3dfc", rlist, envir=.plotted3d)
  invisible(rlist)
}

#' Remove plotted FlyCircuit neurons (deprecated, see nat::npop3d)
#' 
#' If no neurons are specified, the last plotted are removed.
#' @param x Neurons to remove
#' @param slow Whether to remove neurons one by one (slowly) default: FALSE
#' @param type Type of objects to remove see \code{pop3d}.
#' @export
#' @seealso \code{\link[rgl]{pop3d}}
#' @importFrom rgl pop3d
pop3dfc <- function(x, slow=FALSE, type='shapes') {
  .Deprecated('nat::npop3d', msg = 'Please use npop3d for identical functionality that can work with any neuronlist')
  nat::npop3d(x=x, slow=slow, type=type)
}

#' Plot a 3D surface of the FlyCircuit reference brain
#'
#' @param col The colour of the surface
#' @param alpha The opacity of the surface
#' @param ... Extra arguments to pass to plot3d
#' @importFrom rgl plot3d
#' @export
fcwbsurf <- function(col='grey', alpha=0.3, ...) {
  plot3d(FCWB.surf, col=col, alpha=alpha, ...)
}

#' Plot 3D surfaces of the FlyLight neuropil segmentation in FlyCircuit coordinate space
#'
#' @param ... Extra arguments to pass to plot3d
#' @export
#' @importFrom rgl plot3d
fcwbnpsurf <- function(...) {
  plot3d(FCWBNP.surf, ...)
}

#' Creates a new surface containing specified regions from a given surface
#'
#' @param regionNames A list of regions to include in the surface
#' @param surf A surface which contains a superset of the regions
#' @return A surface containing the specified regions
#' @export
getRegionSurf <- function(regionNames, surf) {
  regionIdxs <- match(regionNames,surf$RegionList)
  if (any(is.na(regionIdxs))) stop("Some regionNames are not present in surf")
  surfNew <- list()
  surfNew$Regions <- surf$Regions[regionNames]
  surfNew$Vertices <- surf$Vertices
  surfNew$RegionList <- surf$RegionList[regionIdxs]
  surfNew$RegionColourList <- surf$RegionColourList[regionIdxs]
  # TODO define a [.surf method
  class(surfNew) <- class(surf)
  surfNew
}

#' Draw surface corresponding to a given list of regions from surface object
#'
#' @param regionNames A list of regions to draw
#' @param surf A surface containing a superset of the regions to draw
#' @param ... Additional arguments passed to plot3d
#' @export
drawRegionSurf <- function(regionNames, surf, ...) {
  surf <- getRegionSurf(regionNames, surf)
  plot3d(surf, ...)
}

#' Return a list of regions contained in a surface
#'
#' @param surf The surface to examine
#' @return Returns a list of the regions in the surface object
#' @export
getRegionsFromSurf <- function(surf) {
  surf$RegionList
}

#' Select regions from a surface
#' 
#' @details if selfun is not set (i.e. it is NULL) then a call will be made to 
#'   select3d() with a message prompting for the user to draw a selection box.
#' @param surf The surface from which regions should be selected
#' @param selfun (optional) function returning determining whether x,y,z coords 
#'   are within a selection region.
#' @export
#' @seealso \code{\link{select3d}}
selectRegionsFromSurf <- function(surf, selfun=NULL) {
  if(is.null(selfun)){
    message("Draw a selection box in the RGL window")
    selfun <- select3d()
  }
  selverts <- selfun(surf$Vertices[, 1:3])
  selpointnums <- surf$Vertices[selverts,'PointNo']
  surf$RegionList[lapply(surf$Regions, function(x) any(selpointnums %in% unlist(x))) == TRUE]
}
