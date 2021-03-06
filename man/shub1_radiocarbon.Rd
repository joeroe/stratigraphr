% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{shub1_radiocarbon}
\alias{shub1_radiocarbon}
\title{Radiocarbon dates from Shubayqa 1}
\format{
A data frame with 27 rows and 8 variables:
\describe{
\item{lab_id}{character; standardised lab code uniquely identifying the dated sample.}
\item{context}{integer; schematic context number the sample was found in (see details).}
\item{phase}{character; phase the sample was assigned to.}
\item{sample}{character; description of the sample context, including its real context number.}
\item{material}{character; description of the sample material.}
\item{cra}{integer; conventional radiocarbon age of the sample in years cal BP.}
\item{error}{integer; standard error associated with the radiocarbon measurement in ± years cal BP.}
\item{outlier}{logical; whether the sample is considered an outlier.}
}
}
\source{
\insertCite{Richter2017-xy;textual}{stratigraphr}
}
\usage{
shub1_radiocarbon
}
\description{
Radiocarbon dates from Shubayqa 1, an Epipalaeolithic site in eastern Jordan,
from \insertCite{Richter2017-xy;textual}{stratigraphr}.
}
\details{
\code{context} refers to the schematic stratigraphy included in the \link{shub1} dataset;
the real context number is described in \code{sample}.
}
\references{
\insertAllCited{}
}
\seealso{
\link{shub1}
}
\keyword{datasets}
