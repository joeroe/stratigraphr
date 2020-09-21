# Documentation for datasets
#' @importFrom Rdpack reprompt
NULL

#-------------------------------------------------------------------------
#' Schematic stratigraphy of Shubayqa 1
#'
#' A simplified version of the stratigraphy of Shubayqa 1, an Epipalaeolithic
#' site in eastern Jordan, after \insertCite{Richter2017-xy;textual}{stratigraphr}.
#'
#' @format A data frame with 30 rows, representing contexts, and 7 variables:
#' \describe{
#'   \item{context}{integer; a unique identifier of the context.}
#'   \item{type}{character; type of context, i.e. deposit, fill, cut, or structural.}
#'   \item{above}{integer vector; context(s) stratigraphically above this one.}
#'   \item{below}{integer vector; context(s) stratigraphically below this one.}
#'   \item{equal}{integer vector; context(s) stratigraphically equal to this one.}
#'   \item{phase}{character; Phase assigned to the context, for contexts that aren't structural.}
#'   \item{structure}{character; for structural contexts, the name of the structure they belong to.}
#' }
#'
#' @details
#'
#' The stratigraphy is a simplified version derived from the schematic section
#' in \insertCite{Richter2017-xy;textual}{stratigraphr}, figure 2.
#' Context numbers were arbitrarily assigned and the stratigraphic relations are
#' based on those evident in the diagram.
#' Phase and structure names are also based on the diagram.
#'
#' @seealso [shub1_radiocarbon]
#'
#' @source \insertCite{Richter2017-xy;textual}{stratigraphr}
#'
#' @references
#' \insertAllCited{}
"shub1"

#-------------------------------------------------------------------------
#' Radiocarbon dates from Shubayqa 1
#'
#' Radiocarbon dates from Shubayqa 1, an Epipalaeolithic site in eastern Jordan,
#' from \insertCite{Richter2017-xy;textual}{stratigraphr}.
#'
#' @format A data frame with 27 rows and 8 variables:
#' \describe{
#'   \item{lab_id}{character; standardised lab code uniquely identifying the dated sample.}
#'   \item{context}{integer; schematic context number the sample was found in (see details).}
#'   \item{phase}{character; phase the sample was assigned to.}
#'   \item{sample}{character; description of the sample context, including its real context number.}
#'   \item{material}{character; description of the sample material.}
#'   \item{cra}{integer; conventional radiocarbon age of the sample in years cal BP.}
#'   \item{error}{integer; standard error associated with the radiocarbon measurement in ± years cal BP.}
#'   \item{outlier}{logical; whether the sample is considered an outlier.}
#' }
#'
#' @details
#'
#' `context` refers to the schematic stratigraphy included in the [shub1] dataset;
#' the real context number is described in `sample`.
#'
#' @seealso [shub1]
#'
#' @source \insertCite{Richter2017-xy;textual}{stratigraphr}
#'
#' @references
#' \insertAllCited{}
"shub1_radiocarbon"