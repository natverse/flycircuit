# Set local directory for data storage and remote location for data download
.onLoad <- function(libname, pkgname) {
  # start by establishing location of data directory
  if(is.null(getOption('flycircuit.datadir'))) 
    options(flycircuit.datadir=file.path(system.file(package='flycircuit'), 'extdata'))
  op<-options()
  dd=getOption('flycircuit.datadir')
  
  # then set the other options (if they have not already been set by the user)
  op.flycircuit=list(
    flycircuit.dbdir=file.path(dd,'db'),
    flycircuit.bigmatdir=file.path(dd,'bigmat'),
    flycircuit.ffdir=file.path(dd,'ff'),
    flycircuit.scoremat='kcs20scores'
  )
  
  toset <- !(names(op.flycircuit) %in% names(op))
  if(any(toset)) options(op.flycircuit[toset])
  
  # Create directories if they do not already exist
  # NB this will never be acceptable behaviour for a CRAN package
  dir.create(file.path(getOption('flycircuit.datadir')), showWarnings=FALSE)
  dir.create(file.path(getOption('flycircuit.dbdir')), showWarnings=FALSE)
  dir.create(file.path(getOption('flycircuit.bigmatdir')), showWarnings=FALSE)
  dir.create(file.path(getOption('flycircuit.ffdir')), showWarnings=FALSE)
}

# Set default neuronlist for plotting
.onAttach <- function(libname, pkgname) {
  options('nat.default.neuronlist' = 'dps')
}

# Will store stack of plotted rgl objects, ready for popping
.plotted3d <- new.env()
