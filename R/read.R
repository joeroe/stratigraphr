# Functions for reading stratigraphic and chronological data

# LST files (BASP Harris, Stratify, ArchED) -------------------------------
# Reference: http://archaeologic.al/wiki/Harris_Matrix#LST

#' Read an LST file
#'
#' Reads stratigraphic data in LST format (used by BASP Harris, Stratify, and
#'   ArchEd) into a data frame.
#'
#' @param file Path to an LST file.
#' @param split Controls how attributes that contain multiple values are
#'   handled. If `TRUE`, all attributes except `name` will be split if their
#'   values contain the character specified by `sep`. If `FALSE`, no
#'   splitting will be done. Alternatively, specify a vector of column names
#'   (corresponding to LST attribute names, see details) that should be split.
#' @param sep Delimiter used to separate multiple values in attributes.
#'   multiple values. Ignored if `split = FALSE`. Default: `","`.
#' @param locale [readr::locale()] object specifying the character encoding and
#'   other region-specific settings used in the file. Defaults to
#'   [readr::default_locale()].
#'
#' @return
#' A data frame. Each row represents one stratum. Columns contain the name of
#' the stratum and the attributes associated with it (typically "above",
#' "contemporary_with", "equal_to" and "below").
#'
#' Attribute names are standardised when read into column names. Specifically,
#' they are transformed to lower case and spaces are replaced with an underscore
#' (_).
#'
#' Splitting multiple values into vectors is useful if the output is to be used
#' to construct a stratigraphic graph with [stratigraph()], but will convert
#' affected columns from atomic vectors to lists, which can make them awkward
#' to work with in other contexts. Set `split = FALSE` to avoid this. You can
#' manually split them later with [stringr::str_split()].
#'
#' @details
#' This function supports both the original LST format used by BASP Harris and
#' the "extended" format used by ArchEd and Stratify.
#'
#' @references
#' <http://archaeologic.al/wiki/Harris_Matrix#LST>
#'
#' @family read functions
#'
#' @export
#'
#' @examples
#' # Example data from https://github.com/lparchaeology/harris2graph
#' # Simple LST (BASP Harris)
#' basp_lst <- system.file("extdata", "bonn.lst", package = "stratigraphr")
#' read_lst(basp_lst)
#'
#' # Extended LST (Stratify, ArchEd)
#' stratify_lst <- system.file("extdata", "stratify.lst", package = "stratigraphr")
#' read_lst(stratify_lst)
read_lst <- function(file, split = TRUE, sep = ",",
                     locale = readr::default_locale()) {
  # TODO: Add type checking/validation
  lst <- readr::read_lines(file, locale = locale)

  # Extract header
  header <- lst_extract_header(lst)

  # Split by stratum. A line that starts with a non-whitespace character is the
  # start of a new stratum.
  strata <- lst[4:length(lst)]
  strata <- split(strata, cumsum(stringr::str_detect(strata, "^\\s", negate = TRUE)))

  # Extract attributes for each stratum and bind into a data frame
  strata <- purrr::map_dfr(strata, lst_extract_stratum)

  # Split multiple values into vectors if required
  if (any(split != FALSE)) {
    if (any(split == TRUE)) {
      cols_to_split <- names(strata)
      cols_to_split <- cols_to_split[cols_to_split != "name"]
    }
    else {
      cols_to_split <- split
    }

    strata[cols_to_split] <- lapply(strata[cols_to_split], stringr::str_split,
                                    pattern = stringr::coll(sep))

    strata[cols_to_split] <- lapply(strata[cols_to_split], try_to_flatten)
  }

  attr(strata, "dataset_name") <- header$dataset_name
  return(strata)
}

try_to_flatten <- function(x) {
  if (all(lengths(x) == 1)) unlist(x, recursive = FALSE)
  else x
}

#' Extract header from an LST file
#'
#' @param lst Character vector of lines from an LST file (e.g. from [readr::read_lines()])
#'
#' @noRd
#' @keywords internal
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
#' @return
#' A named list of attributes.
#'
#' @noRd
#' @keywords internal
lst_extract_stratum <- function(stratum) {
  # TODO: Add type checking
  # TODO: Extract name, label and site code separately – need some test data
  name <- stratum[1]
  attrs <- purrr::map(stratum[2:length(stratum)], lst_extract_attribute)
  attrs <- purrr::flatten(attrs)

  stratum <- c(name = name, attrs)
  return(stratum)
}

#' Extract a single attribute from an LST file
#'
#' @param attr A single attribute line from an LST file.
#'
#' @return
#' A named list with one element, i.e. `list(attribute = value)`.
#'
#' @noRd
#' @keywords internal
lst_extract_attribute <- function(attr) {
  checkmate::assert_character(attr, pattern = ".*:.*", len = 1)

  # Extract attribute name and value
  attr <- stringr::str_trim(attr)
  attr <- stringr::str_split(attr, stringr::coll(":"), n = 2)
  attr <- unlist(attr)
  name <- attr[1]
  value <- attr[2]

  # Normalise attribute names for R
  name <- stringr::str_replace_all(name, stringr::coll(" "), "_")
  name <- stringr::str_to_lower(name)

  # Remove whitespace from values
  value <- stringr::str_remove_all(value, stringr::coll(" "))

  # Make missing values explicit
  if(value == "") {
    value <- as.character(NA)
  }

  attr <- list(value)
  names(attr) <- name
  return(attr)
}

# OxCal -------------------------------------------------------------------
