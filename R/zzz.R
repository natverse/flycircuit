# Set local directory for data storage and remote location for data download
.onLoad <- function(libname, pkgname) {
  options('flycircuit.datadir' = file.path(system.file(package='flycircuit'), 'extdata'))
  options('flycircuit.dbdir' = file.path(getOption('flycircuit.datadir'), 'db'))
  options('flycircuit.bigmatdir' = file.path(getOption('flycircuit.datadir'), 'bigmat'))
  options('flycircuit.ffdir' = file.path(getOption('flycircuit.datadir'), 'ff'))
  options('flycircuit.resourcesdir' = file.path(getOption('flycircuit.datadir'), 'resources'))
  options('flycircuit.remoteloc' = 'http://flybrain.mrc-lmb.cam.ac.uk/flycircuit')

  # Create directories if they do not already exist
  dir.create(file.path(getOption('flycircuit.datadir')), showWarnings=FALSE)
  dir.create(file.path(getOption('flycircuit.dbdir')), showWarnings=FALSE)
  dir.create(file.path(getOption('flycircuit.bigmatdir')), showWarnings=FALSE)
  dir.create(file.path(getOption('flycircuit.ffdir')), showWarnings=FALSE)
  dir.create(file.path(getOption('flycircuit.resourcesdir')), showWarnings=FALSE)
}

# Set default neuronlist for plotting
.onAttach <- function(libname, pkgname) {
  options('nat.default.neuronlist' = 'dps')
}

# Will store stack of plotted rgl objects, ready for popping
.plotted3d <- new.env()
