\name{plot3dfc}
\alias{plot3dfc}
\title{Plot FlyCircuit neurons using rgl}
\usage{
plot3dfc(id, col, db = get(getOption("nat.default.neuronlist")), flip = F,
  soma = F, alpharange = NULL, skipRedraw = 200, ...)
}
\arguments{
  \item{id}{Vector of FlyCircuit gene or neuron names or
  idids (passed to \code{\link{fc_gene_name}})}

  \item{col}{Function or vector specifying colour.}

  \item{db}{Object of class neuronlist (only tested for
  dotprops objects).}

  \item{flip}{Logical vector (whether to use list
  containing flipped neurons). Recycled if length 1.}

  \item{soma}{Whether to show the estimated cell body
  position.}

  \item{alpharange}{Range of alpha values for which
  vectors/dots should be plotted..}

  \item{skipRedraw}{Numeric threshold above which to
  disable rgl redraw.}

  \item{...}{Additional arguments for \code{plot3d}}
}
\value{
List of rgl stack ids (see rgl::plot3d).
}
\description{
Plot FlyCircuit neurons using rgl
}
\details{
\itemize{

\item Plotted soma positions are a prediction based on
location in the FlyCircuit template brain mapped onto my
selected template. The positions are looked up in the df
attribute of the neuronlist or in a dataframe called
somapos.

\item Colour is rainbow(length(id)) when more than one
neuron is plotted. It can be specified as a function, a
vector (length(id)) or a single recycled value.

\item By default when more than 200 neurons are plotted in
a single call rgl redraw is suspended. This can be >10x
faster.

\item Note that the ids of objects on the rgl stack are
stored in a variable in the global environment called
\code{.last.plot3dfc} that can be used by \code{pop3dfc()}
to remove all objects plotted in last call to plot3dfc. }
}
\seealso{
\code{\link[rgl]{plot3d}, \link{pop3dfc}}
}

