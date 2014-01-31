# Set local directory for data storage and remote location for data download
.onLoad <- function(libname, pkgname) {
  options('flycircuit.localroot' = system.file(package='flycircuit'))
  options('flycircuit.datadir' = file.path(getOption('flycircuit.localroot'), 'data'))
  options('flycircuit.dbdir' = file.path(getOption('flycircuit.localroot'), 'db'))
  options('flycircuit.bigmatdir' = file.path(getOption('flycircuit.datadir'), 'bigmat'))
  options('flycircuit.remoteloc' = 'http://flybrain.mrc-lmb.cam.ac.uk/flycircuit')
}

# Set default neuronlist for plotting
.onAttach <- function(libname, pkgname) {
  options('nat.default.neuronlist' = 'dps')
}

# Will store stack of plotted rgl objects, ready for popping
.plotted3d <- new.env()
