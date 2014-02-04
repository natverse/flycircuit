\name{selectRegionsFromSurf}
\alias{selectRegionsFromSurf}
\title{Select regions from a surface}
\usage{
selectRegionsFromSurf(surf, selfun = NULL)
}
\arguments{
  \item{surf}{The surface from which regions should be
  selected}

  \item{selfun}{(optional) function returning determining
  whether x,y,z coords are within a selection region.}
}
\description{
Select regions from a surface
}
\details{
if selfun is not set (i.e. it is NULL) then a call will be
made to select3d() with a message prompting for the user to
draw a selection box.
}
\seealso{
\code{\link{select3d}}
}

