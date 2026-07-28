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
#'
#' @examples
#' \dontrun{
#' script <- cql(
#'   cql_r_date("ABC-001", 10100, 50),
#'   cql_r_date("ABC-002", 10200, 50)
#' )
#' write_oxcal(script, tempfile(fileext = ".oxcal"))
#' }
write_oxcal <- function(cql, file) {
  checkmate::assert_class(cql, "cql")

  if (!stringr::str_ends(file, ".oxcal")) {
    file <- paste0(file, ".oxcal")
    message("Writing to ", file)
  }

  out <- utils::capture.output(print(cql))
  vroom::vroom_write_lines(out, file, eol = "\r\n")

  invisible(cql)
}
