% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metis.index.R
\name{metis.index}
\alias{metis.index}
\title{metis.index}
\usage{
metis.index(
  data = NULL,
  colIndex = NULL,
  colValue = NULL,
  numerators = NULL,
  denominators = NULL,
  meanYearsDenominator = NULL,
  scenariosSelect = NULL,
  indexName = "index",
  dirOutputs = paste(getwd(), "/outputs", sep = ""),
  fname = "index",
  nameAppend = "",
  folderName = NULL,
  saveFile = F
)
}
\arguments{
\item{data}{Default = NULL. Full path to grid file.}

\item{colIndex}{Default = NULL,}

\item{colValue}{Default = NULL,}

\item{numerators}{Default = NULL,}

\item{denominators}{Default = NULL, (Choose 1 to use Numerator Value)}

\item{meanYearsDenominator}{Default = NULL,}

\item{scenariosSelect}{Default = NULL,}

\item{indexName}{Default = "index",}

\item{dirOutputs}{Default = paste(getwd(),"/outputs",sep=""),}

\item{fname}{Default = "index"}

\item{nameAppend}{Default = ""}

\item{folderName}{Default = NULL,}

\item{saveFile}{Default = F. If want csv output then change to T}
}
\value{
dataframe with index
}
\description{
This function calculates indices based on a given numerator and denominator
}
\keyword{grid,}
\keyword{polygon}
\keyword{shape,}
