% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{add_link}
\alias{add_link}
\title{Add a hyperlink}
\usage{
add_link(url_path, url_text)
}
\arguments{
\item{url_path}{string, URL}

\item{url_text}{string, URL link text to display}
}
\value{
html URL link
}
\description{
Writes html code for a URL link to R Markdown format
}
\examples{
add_link("https://www.cvmedlab.org/", "cvmedlab")

}
