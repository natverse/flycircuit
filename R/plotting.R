#' Plot FlyCircuit neurons using rgl
#' 
#' @details \itemize{
#'   
#'   \item Colour defaults to black for a single neuron or rainbow(length(id)) 
#'   when more than one neuron is plotted. It can be specified as a function, a 
#'   vector (length(id)) or a single recycled value.
#'   
#'   \item By default when more than 200 neurons are plotted in a single call 
#'   rgl redraw is suspended. This can be >10x faster.
#'   
#'   \item \code{\link[nat]{npop3d}} can be used to remove the last plotted 
#'   neurons.
#'   
#'   \item Plotted soma positions (when \code{soma=TRUE}) is passed as an 
#'   additional argument for plot3d.neuronlist are a prediction based on 
#'   location in the FlyCircuit template brain mapped onto my selected template.
#'   The positions are looked up in the df attribute of the neuronlist.
#'   
#'   }
#' @param id Vector of FlyCircuit gene or neuron names or idids (passed to 
#'   \code{\link{fc_gene_name}})
#' @param col Function or vector specifying colour.
#' @param db Object of class \code{neuronlist} or a character vector naming a 
#'   neuronlist that will be passed to \code{link{get}}.
#' @param ... Additional arguments for \code{plot3d}
#' @return List of rgl stack ids (see rgl::plot3d).
#' @export
#' @seealso \code{\link[rgl]{plot3d}, \link{pop3dfc}, \link{fc_gene_name}}
#' @examples
#' open3d()
#' library(nat)
#' plot3dfc("fru-M-300145", col='green', db=kcs20)
#' npop3d()
#' plot3dfc("fru-M-100014", col='red', db=kcs20, soma=TRUE)
#' rgl.close()
#' @importFrom rgl plot3d
plot3dfc <- function(id, col, db=getOption('nat.default.neuronlist'), ...) {
  if(is.null(db)) stop("Must specify a neuronlist")
  if(is.character(db)) db=get(db)
  
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
  
  if(missing(col)) {
    col = if(length(id)>1) rainbow else 'black'
  }
  do.call(plot3d, args = list(x=id, col=col, db=db, ...))
}

#' Remove plotted FlyCircuit neurons (deprecated, see nat::npop3d)
#' 
#' If no neurons are specified, the last plotted are removed.
#' @param x Neurons to remove
#' @param slow Whether to remove neurons one by one (slowly) default: FALSE
#' @param type Type of objects to remove see \code{pop3d}.
#' @export
#' @seealso \code{\link[rgl]{pop3d}}
pop3dfc <- function(x, slow=FALSE, type='shapes') {
  .Deprecated('nat::npop3d', msg = 'Please use npop3d for identical functionality that can work with any neuronlist')
  nat::npop3d(x=x, slow=slow, type=type)
}

#' Plot a 3D surface of the FlyCircuit reference brain
#'
#' @param col The colour of the surface
#' @param alpha The opacity of the surface
#' @param ... Extra arguments to pass to plot3d
#' @export
fcwbsurf <- function(col='grey', alpha=0.3, ...) {
  plot3d(flycircuit::FCWB.surf, col=col, alpha=alpha, ...)
}

#' Plot 3D surfaces of the FlyLight neuropil segmentation in FlyCircuit coordinate space
#'
#' @param ... Extra arguments to pass to plot3d
#' @export
fcwbnpsurf <- function(...) {
  plot3d(flycircuit::FCWBNP.surf, ...)
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
#' @importFrom rgl select3d
selectRegionsFromSurf <- function(surf, selfun=NULL) {
  if(is.null(selfun)){
    message("Draw a selection box in the RGL window")
    selfun <- select3d()
  }
  selverts <- selfun(surf$Vertices[, 1:3])
  selpointnums <- surf$Vertices[selverts,'PointNo']
  surf$RegionList[lapply(surf$Regions, function(x) any(selpointnums %in% unlist(x))) == TRUE]
}


#' Scan through a set of flycircuit neurons
#' 
#' Can also choose to select specific neurons along the way and navigate 
#' forwards and backwards. NB this is simply a wrapper for 
#' \code{nat::\link[nat]{nlscan}}, with the additional function of converting all 
#' neuron identifiers to standard flycircuit identifiers.
#' 
#' @param neurons vector of flycircuit identifiers to plot *(anything that 
#'   \code{\link{fc_gene_name}} understands)
#' @param db the neuronlist containing the neurons. Defaults to the list named 
#'   by \code{options("nat.default.neuronlist")}.
#' @param col the colour with which to plot the neurons (default \code{'red'}).
#' @param Verbose logical indicating that info about each selected neuron should
#'   be printed (default \code{TRUE}).
#' @param Wait logical indicating that there should be a pause between each 
#'   displayed neuron.
#' @param sleep time to pause between each displayed neuron when 
#'   \code{Wait=TRUE}.
#' @param extrafun an optional function called when each neuron is plotted, with
#'   args \code{gene_name} and \code{selected}.
#' @param selected_file an optional path to a \code{yaml} file that already 
#'   contains a selection.
#' @param selected_col the color in which selected neurons (such as those 
#'   specified in \code{selected_file}) should be plotted.
#' @param yaml a logical indicating that selections should be saved to disk in 
#'   \code{yaml} rather than \code{rda} format.
#' @param ... extra arguments to pass to \code{\link{plot3dfc}}.
#'   
#' @return A character vector of names of any selected neurons, of length 0 if 
#'   none selected.
#' @importFrom yaml yaml.load_file
#' @importFrom yaml as.yaml
#' @export
#' @seealso \code{\link[nat]{nlscan}}, \code{\link{fc_gene_name}}
#' @examples
#' \dontrun{
#' # numeric idids, specifying db explicitly
#' dpscan(c(1024L, 10616L, 8399L), db=kcs20)
#' 
#' # using an option to set db
#' op<-options(nat.default.neuronlist='kcs20')
#' dpscan(c("fru-M-500112", "Gad1-F-900005", "Gad1-F-100010"))
#' options(op)
#' 
#' }
dpscan <- function(neurons, db=NULL, col='red', 
                   Verbose=T, Wait=T, sleep=0.1, extrafun=NULL, 
                   selected_file=NULL, selected_col='green', yaml=TRUE, ...) {
  if(is.null(db))
    db=get(getOption('nat.default.neuronlist',
                   default=stop('Option "nat.default.neuronlist" is not set.',
                                ' See ?nat for details.')))
  if(!is.neuronlist(db))
    stop("Please set options(nat.default.neuronlist='myfavneuronlist'). ",
         "See ?nat for details.")
  
  # convert identifiers to standard flycircuit ids
  neurons=fc_gene_name(neurons)
  # drop any missing ids with a warning
  missing_ids=setdiff(neurons, names(db))
  if(nmissing<-length(missing_ids)){
    warning("Dropping ", nmissing,' neurons that are not present in db!')
    neurons=setdiff(neurons, missing_ids)
  }

  nlscan(neurons, db=db, col=col, Verbose=Verbose, Wait=Wait, sleep=sleep,
         extrafun=extrafun, selected_file=selected_file, selected_col=selected_col,
         yaml=yaml, ...)
}

