\name{fc_download_data}
\alias{fc_download_data}
\title{Download a data file from a remote location}
\usage{
fc_download_data(url, type = c("data", "db", "bigmat", "ff"),
  update = FALSE, ...)
}
\arguments{
  \item{url}{The location of the remote file}

  \item{type}{The type of file (data, db, or bigmat)}

  \item{update}{Whether to overwrite an existing file
  (default: FALSE)}

  \item{...}{Additional arguments passed to download.file
  (e.g. quiet)}
}
\description{
Download a data file from a remote location
}
\details{
the \code{update} argument could be more intelligently
implemented e.g. by comparing the etag reported by the url
header to see if the file has changed.
}
\examples{
\dontrun{
fc_download_data("http://myurl.com/data", quiet=TRUE)
fc_download_data("http://myurl.com/data.ff", type='ff')
}
}
\seealso{
\code{\link{download.file}}
}

