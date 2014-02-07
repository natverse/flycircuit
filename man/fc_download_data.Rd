\name{fc_download_data}
\alias{fc_download_data}
\title{Download a data file from a remote location}
\usage{
fc_download_data(url, type = c("data", "db", "bigmat", "ff"), ...)
}
\arguments{
  \item{url}{The location of the remote file}

  \item{type}{The type of file (data, db, or bigmat)}

  \item{...}{Additional arguments passed to download.file
  (e.g. quiet)}
}
\description{
Download a data file from a remote location
}
\examples{
\dontrun{
fc_download_data("http://myurl.com/data", quiet=TRUE)
}
}
\seealso{
\code{\link{download.file}}
}

