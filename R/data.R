# Documentation for datasets
#' @importFrom Rdpack reprompt
NULL


# shub1 -------------------------------------------------------------------
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
#' @source \insertCite{Richter2017-xy;textual}{stratigraphr}
#'
#' @references
#' \insertAllCited{}
"shub1"


# harris12 ----------------------------------------------------------------
#' Stratigraphy from Harris Figure 12
#'
#' A model stratigraphic sequence used by Edward
#'   \insertCite{Harris1979-qs;textual}{stratigraphr} to illustrate the
#'   construction of a Harris matrix.
#'
#' @format A data frame with 10 rows and 4 variables:
#' \describe{
#'   \item{context}{Label of the stratigraphic unit.}
#'   \item{above}{Unit stratigraphically above.}
#'   \item{below}{Unit stratigraphically below.}
#'   \item{equal}{Unit stratigraphically equal.}
#' }
#'
#' @source \insertCite{Harris1979-qs;textual}{stratigraphr}, p. 39.
#'
#' @references
#' \insertAllCited{}
"harris12"
