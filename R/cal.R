# S3 class representing calibrated dates

# Constructor functions ---------------------------------------------------

#' Calibrated radiocarbon dates
#'
#' `cal()` constructs a generic representation of a calibrated radiocarbon date
#' or other calendar-based probability distribution.
#'
#' @param x                       A `data.frame` with two columns, with calendar years and associated probabilities.
#' @param curve                   (Optional) `character`. Atmospheric curve used for calibration, e.g. "intcal20".
#' @param era                     (Optional) `character`. Calendar system used. Default: `cal BP`.
#' @param lab_id                  (Optional) `character`. Lab code or other label for the calibrated sample.
#' @param cra                     (Optional) `integer`. Uncalibrated conventional radiocarbon age (CRA) of the sample.
#' @param error                   (Optional) `integer`. Error associated with the uncalibrated sample.
#' @param reservoir_offset        (Optional) `integer`. Marine reservoir offset used in the calibration, if any.
#' @param reservoir_offset_error  (Optional) `integer`. Error associated with the marine reservoir offset.
#' @param calibration_range       (Optional) `integer` vector of length 2. The range of years over which the calibration was performed, i.e. `c(start, end)`.
#' @param F14C                    (Optional) `logical`. Whether the calibration was calculated using F14C values instead of the CRA.
#' @param normalised              (Optional) `logical`. Whether the calibrated probability densities were normalised.
#' @param p_cutoff                (Optional) `numeric`. Lower threshold beyond which probability densities were considered zero.
#'
#' @return
#' `cal` object: a data frame with two columns, `year` and `p`, representing
#' the calibrated probability distribution. All other values are stored as
#' attributes and can be accessed with [cal_metadata()].
#'
#' @family tidy radiocarbon functions
#' @family functions for working with `cal` objects
#'
#' @export
cal <- function(x,
                era = c("cal BP"),
                lab_id = NA,
                cra = NA,
                error = NA,
                curve = NA,
                reservoir_offset = NA,
                reservoir_offset_error = NA,
                calibration_range = NA,
                F14C = NA,
                normalised = NA,
                p_cutoff = NA) {
  new_cal(x,
          era = era,
          lab_id = lab_id,
          cra = cra,
          error = error,
          curve = curve,
          reservoir_offset = reservoir_offset,
          reservoir_offset_error = reservoir_offset_error,
          calibration_range = calibration_range,
          F14C = F14C,
          normalised = normalised,
          p_cutoff = p_cutoff)
}

new_cal <- function(x = data.frame(year = integer(0), p = numeric(0)), ...) {
  checkmate::assert_data_frame(x, ncols = 2)

  colnames(x) <- c("year", "p")
  attrs <- list(...)

  class(x) <- c("cal", "data.frame")
  attributes(x) <- c(attributes(x), attrs)

  return(x)
}


# validate_cal <- function() {
#
# }


# S3 Methods ------------------------------------------------------------------

#' @rdname cal
#' @export
print.cal <- function(x, ...) {
  start <- max(x$year)
  end <- min(x$year)
  era <- attr(x, "era")

  metadata <- cal_metadata(x)

  cli::cli_text("# Calibrated probability distribution from {start} to {end} {era}")
  cli::cat_line()
  txtplot::txtplot(x$year, x$p, height = 10)
  cli::cat_line()
  # TODO: Messy – should probably refactor into its own function
  if(!is.null(metadata$lab_id)) {
    cli::cli_dl(list(`Lab ID` = metadata$lab_id))
    metadata$lab_id <- NULL
  }
  if(!is.null(metadata$cra)) {
    cli::cli_dl(list(`Uncalibrated` = glue::glue("{metadata$cra}±{metadata$error} uncal BP")))
    metadata$cra <- NULL
    metadata$error <- NULL
  }
  if(!is.null(metadata$calibration_range) &&
     !all(is.na(metadata$calibration_range))) {
    metadata$calibration_range <- glue::glue("{metadata$calibration_range[1]}–{metadata$calibration_range[2]} BP")
  }
  cli::cli_dl(metadata)

  invisible(x)
}

#' @export
min.cal <- function(...) {
  cals <- rlang::list2(...)
  cals <- dplyr::bind_rows(cals)
  cals[cals$p <= 0] <- NULL
  max(cals$year)
}

#' @export
max.cal <- function(...) {
  cals <- rlang::list2(...)
  cals <- dplyr::bind_rows(cals)
  cals[cals$p <= 0] <- NULL
  min(cals$year)
}

# Conversion functions ------------------------------------------------------

#' Convert an object to a cal object
#'
#' @description
#' `as_cal()` converts objects from other packages that represent calibrated
#' radiocarbon dates to `cal` objects.
#' Methods are currently implemented for:
#'
#' * `CalDates`: from [rcarbon::calibrate()]
#'
#' These functions are intended for complex S3 objects from other packages.
#' See [cal()] for a more generic constructor, e.g. using a data frame.
#'
#' @param x  object to be converted to a `cal` object.
#'
#' @returns
#' `cal` object: a data frame with two columns, `year` and `p`, representing
#' the calibrated probability distribution. All other values are stored as
#' attributes and can be accessed with [cal_metadata()].
#'
#' @family functions for working with `cal` objects
#'
#' @export
as_cal <- function(x) UseMethod("as_cal")

#' @rdname as_cal
#' @export
as_cal.CalDates <- function(x) {
  if(!is.na(x$calmatrix)) {
    warning("calMatrix object in CalDates ignored.")
  }

  caldates <- x$metadata
  caldates$calGrid <- x$grids

  # TODO: automate attribute recoding via cal_recode_metadata()
  purrr::pmap(caldates, ~with(list(...),
                              new_cal(calGrid,
                                      era = "cal BP",
                                      lab_id = DateID,
                                      cra = CRA,
                                      error = Error,
                                      curve = CalCurve,
                                      reservoir_offset = ResOffsets,
                                      reservoir_offset_error = ResErrors,
                                      calibration_range = c(StartBP, EndBP),
                                      normalised = Normalised,
                                      F14C = F14C,
                                      p_cutoff = CalEPS
                              )))
}

#' Convert cal objects to a rcarbon CalDates object
#'
#' @param x  A list of `cal` objects.
#'
#' @return
#' A `CalDates` object. See [rcarbon::calibrate()] for details.
#'
#' @family functions for working with `cal` objects
#'
#' @export
as.CalDates.cal <- function(x) {
  x <- purrr::map(x, cal_repair_calibration_range)

  metadata <- purrr::map(x, cal_metadata)
  metadata <- purrr::map_dfr(metadata, cal_recode_metadata,
                             from = "cal", to = "CalDates")
  metadata <- as.data.frame(metadata)

  grids <- purrr::map(x, data.frame)
  grids <- purrr::map(grids, structure,
                      class = c("calGrid", "data.frame"),
                      names =  c("calBP", "PrDens"))

  calMatrix <- NA

  CalDates <- list(metadata = metadata,
                   grids = grids,
                   calMatrix = calMatrix)
  class(CalDates) <- c("CalDates", "list")

  return(CalDates)
}



# Utility functions -------------------------------------------------------

#' Extract metadata from a calibrated date
#'
#' @param x  A `cal` object. See [cal()].
#'
#' @return
#' A named list of metadata attributes.
#'
#' @family functions for working with `cal` objects
#'
#' @export
cal_metadata <- function(x) {
  attrs <- attributes(x)
  attrs$names <- NULL
  attrs$row.names <- NULL
  attrs$class <- NULL

  return(attrs)
}


# Utility functions (internal) -------------------------------------------

cal_metadata_thesaurus <- function(what = NA) {
  thesaurus <- tibble::tribble(
    ~cal,                      ~CalDates,
    "era",                     NA,
    "lab_id",                  "DateID",
    "cra",                     "CRA",
    "error",                   "Error",
    NA,                        "Details",
    "curve",                   "CalCurve",
    "reservoir_offset",        "ResOffsets",
    "reservoir_offset_error",  "ResErrors",
    "calibration_range",       "StartBP",
    "calibration_range",       "EndBP",
    "normalised",              "Normalised",
    "F14C",                    "F14C",
    "p_cutoff",                "CalEPS"
  )

  if(!is.na(what)) {
    return(thesaurus[[what]])
  }
  else {
    return(thesaurus)
  }
}

cal_recode_metadata <- function(x,
                                from = c("CalDates", "cal"),
                                to = c("cal", "CalDates")) {
  from <- match.arg(from)
  to <- match.arg(to)

  thes <- stats::setNames(cal_metadata_thesaurus(from), cal_metadata_thesaurus(to))
  thes <- thes[!is.na(names(thes))]

  x <- x[thes]
  names(x) <- names(thes)

  # Vectors
  if(to == "CalDates") {
    x$StartBP <- x$StartBP[1]
    x$EndBP <- x$EndBP[2]
  }

  x[sapply(x, is.null)] <- NA

  return(x)
}

# rcarbon functions like spd() expect the StartBP and EndBP metadata to be set,
# but if the dates came from another source, they might be missing. This function
# reconstructs them from the probability distribution.
# x should be a cal object.
cal_repair_calibration_range <- function(x) {
  if(is.null(attr(x, "calibration_range")) ||
     any(is.na(attr(x, "calibration_range")))) {
    attr(x, "calibration_range") <- c(max(x$year), min(x$year))
  }
  return(x)
}