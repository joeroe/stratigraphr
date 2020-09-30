# Functions for reading stratigraphic and chronological data

# LST files (BASP Harris, Stratify, ArchED) -------------------------------
# Reference: http://archaeologic.al/wiki/Harris_Matrix#LST

#' Read an LST file
#'
#' Reads stratigraphic data in LST format (used by BASP Harris, Stratify, and
#'   ArchEd) into a data frame.
#'
#' @param file    Path to an LST file.
#' @param locale  [readr::locale()] object specifying the character encoding and
#'                 other region-specific settings used in the file. Defaults to
#'                 [readr::default_locale()], which should be fine in most cases.
#'
#' @return
#' A data frame. Each row represents one stratum. Columns contain the name of
#'   the stratum and the attributes associated with it (typically "above",
#'   "contemporary_with", "equal_to" and "below").
#'
#' @references
#' <http://archaeologic.al/wiki/Harris_Matrix#LST>
#'
#' @family read functions
#'
#' @export
#'
#' @examples
read_lst <- function(file, locale = readr::default_locale()) {
  # TODO: Add type checking
  lst <- readr::read_lines(file, locale = locale)

  header <- lst_extract_header(lst)

  # Split into chunks of 5 lines per stratum
  # TODO: May be a bad assumption for "extended" LST files?
  strata <- lst[4:length(lst)]
  strata <- split(strata, ceiling(seq_along(strata) / 5))
  strata <- purrr::map_dfr(strata, lst_extract_stratum)

  attr(strata, "dataset_name") <- header$dataset_name
  return(strata)
}

#' Extract header from an LST file
#'
#' @param lst Character vector of lines from an LST file (e.g. from [readr::read_lines()])
lst_extract_header <- function(lst) {
  # TODO: Add type checking
  header <- lst[1:3]
  name <- header[1]
  name <- stringr::str_trim(name)
  #TODO: What are the other two lines for?
  return(
    list(
      dataset_name = name
    )
  )
}

#' Extract a single stratum from an LST file
#'
#' @param lst Character vector of lines from an LST file (e.g. from [readr::read_lines()])
#'              representing a single stratum.
#'
#' @details
#' Currently lst_extract_stratum() assumes each stratum is exactly five lines
#'   long: the first line with the name of the stratum, and subsequent lines
#'   containing its attributes (passed to [lst_extract_attribute()]). This
#'   may not be a sound assumption for the "extended" format mentioned in
#'   <http://archaeologic.al/wiki/Harris_Matrix#LST>.
#'
#' @return
#' Data frame with one row and columns representing the name and attributes of
#'   a stratum.
lst_extract_stratum <- function(stratum) {
  # TODO: Add type checking
  # TODO: Extract name, label and site code separately – need some test data
  name <- stratum[1]
  attrs <- purrr::map_dfc(stratum[2:5], lst_extract_attribute)

  stratum <- cbind(
    name = name,
    attrs
  )
  return(stratum)
}

#' Extract a single attribute from an LST file
#'
#' @param attr A single attribute line from an LST file.
#'
#' @return
#' A named list containing the attribute name and value.
lst_extract_attribute <- function(attr) {
  checkmate::assert_character(attr, pattern = ".*:.*", len = 1)

  # Extract attribute name and value
  attr <- stringr::str_trim(attr)
  attr <- stringr::str_split(attr, stringr::coll(":"))
  attr <- unlist(attr)
  name <- attr[1]
  value <- attr[2]

  # Normalise attribute names for R
  name <- stringr::str_replace_all(name, stringr::coll(" "), "_")

  # Remove whitespace from values
  value <- stringr::str_remove_all(value, stringr::coll(" "))

  # Make missing values explicit
  if(value == "") {
    value <- NA
  }

  # Vectorise multiple attribute values (if value is not missing)
  # else if(stringr::str_detect(value, stringr::coll(","))) {
  #   value <- stringr::str_split(value, stringr::coll(","))
  # }

  attr <- list(value)
  names(attr) <- name
  return(attr)
}

# OxCal -------------------------------------------------------------------
