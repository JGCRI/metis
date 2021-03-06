% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metis.ic.R
\name{metis.ic}
\alias{metis.ic}
\title{metis.ic}
\usage{
metis.ic(
  data = NULL,
  icSectors = NULL,
  icColors = NULL,
  dirOutputs = paste(getwd(), "/outputs", sep = ""),
  fname = "ic",
  nameAppend = "",
  folderName = NULL,
  saveFile = F,
  printFig = F
)
}
\arguments{
\item{data}{Default = NULL. Matrix with rows showing from and cols to. Or dataframe with from, to and value.}

\item{icSectors}{Default = NULL,}

\item{icColors}{Default = NULL,}

\item{dirOutputs}{Default = paste(getwd(),"/outputs",sep=""),}

\item{fname}{Default = "ic"}

\item{nameAppend}{Default = ""}

\item{folderName}{Default = NULL,}

\item{saveFile}{Default = F. If want csv output then change to T}

\item{printFig}{Default = F.}
}
\value{
dataframe with index
}
\description{
This function calculates an interconnectivity index (ic).
}
\keyword{grid,}
\keyword{polygon}
\keyword{shape,}
