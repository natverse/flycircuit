\name{load_fcdata}
\alias{load_fcdata}
\title{Load an rda object cached on disk into the Global Environment}
\usage{
load_fcdata(data, Force = FALSE, folder = c("data", "db"))
}
\arguments{
  \item{data}{Name of object (and the stem of rda file).}

  \item{folder}{Name of the project subfolder containing
  object.}

  \item{Force}{Whether to load even if table already exists
  (default FALSE).}
}
\value{
A character vector of the names of objects created,
invisibly or NULL if nothing loaded.
}
\description{
Load an rda object cached on disk into the Global
Environment
}

