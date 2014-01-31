\name{plot3d.hclust}
\alias{plot3d.hclust}
\title{Plot dotprops coloured by groups cut from an hclust object.}
\usage{
\method{plot3d}{hclust}(hc, k = NULL, h = NULL, groups, col = rainbow,
  ...)
}
\arguments{
  \item{hc}{An hclust object.}

  \item{k}{Number of clusters to cut from hclust object.}

  \item{h}{Height to cut hclust object.}

  \item{groups}{Numeric vector of groups to plot.}

  \item{col}{Colours for groups (directly specified or a
  function).}
}
\value{
List of rgl ids for plotted objects.
}
\description{
Plot dotprops coloured by groups cut from an hclust object.
}
\details{
Note that the colours are in the order of the dendrogram as
assigned by colour_clusters.
}
\seealso{
\code{\link{hclust},\link{slice},\link{colour_clusters}}
}

