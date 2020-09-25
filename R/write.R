# Functions for writing stratigraphic and chronological data

#' Write CQL to a file
#'
#' Writes a CQL script (from [cql()]) to an .oxcal file, for input to OxCal.
#'
#' @param cql   A `cql` object. See [cql()].
#' @param file  Path to a file.
#'
#' @return
#' Returns `cql` invisibly.
#'
#' @family CQL functions
#' @family write functions
#'
#' @export
write_oxcal <- function(cql, file) {
  checkmate::assert_class(cql, "cql")

  if(!stringr::str_ends(file, ".oxcal")) {
    file <- paste0(file, ".oxcal")
    message("Writing to ", file)
  }

  out <- utils::capture.output(print(cql))
  readr::write_lines(out, file, sep = "\r\n")

  invisible(cql)
}