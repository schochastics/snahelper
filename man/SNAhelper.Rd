% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SNAhelper.R
\name{SNAhelper}
\alias{SNAhelper}
\alias{SNAhelperGadget}
\alias{SNAhelperAddin}
\title{SNAhelper}
\usage{
SNAhelperGadget(graph)

SNAhelperAddin()
}
\arguments{
\item{graph}{An igraph object to visualize}
}
\value{
\code{SNAhelper} returns a character vector with code.
}
\description{
\code{SNAhelper} is a RStudio-Addin that provides a graphical interface for network analysis and visualization.
}
\details{
To run the addin, highlight an igraph-object in your current script and select \code{SNAhelper} from the Addins-menu within RStudio. After terminating the addin, a character string containing the code for visualization is inserted in your current script. Alternatively you can run it with SNAhelperGadget(graph) from the console.
}
\examples{
if (interactive()) {
  graph <- igraph::sample_gnp(100,0.2)
  SNAhelperGadget(graph)
}
}
