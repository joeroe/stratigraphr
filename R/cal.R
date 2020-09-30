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
#' @param F14C                    (Optional) `logical`. Whether the calibration was calculated using F14C values instead of the CRA.
#' @param normalised              (Optional) `logical`. Whether the calibrated probability densities were normalised.
#' @param p_cutoff                (Optional) `numeric`. Lower threshold beyond which probability densities were considered zero.
#'
#' @return
#' `cal` object: a data frame with two columns, `year` and `p`, representing
#' the calibrated probability distribution. All other values are stored as
#' attributes.
#'
#' @family radiocarbon functions
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
                F14C = NA,
                normalised = NA,
                p_cutoff = NA) {
  new_cal(x, curve = curve, era = era)
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


# Coercion functions ------------------------------------------------------

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
#' attributes.
#'
#' @family radiocarbon functions
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

  purrr::pmap(caldates, ~with(list(...),
                              new_cal(calGrid,
                                      era = "cal BP",
                                      lab_id = DateID,
                                      cra = CRA,
                                      error = Error,
                                      curve = CalCurve,
                                      reservoir_offset = ResOffsets,
                                      reservoir_offset_error = ResErrors,
                                      normalised = Normalised,
                                      p_cutoff = CalEPS
                                      )))
}

# Print and summary functions ---------------------------------------------

#' @rdname cal
#' @export
print.cal <- function(x, ...) {
  start <- max(x$year)
  end <- min(x$year)
  era <- attr(x, "era")

  attrs <- attributes(x)
  attrs$names <- NULL
  attrs$row.names <- NULL
  attrs$class <- NULL
  attrs$era <- NULL

  cli::cli_text("# Calibrated probability distribution from {start} to {end} {era}")
  cli::cat_line()
  txtplot::txtplot(x$year, x$p, height = 10)
  cli::cat_line()
  if(!is.null(attrs$lab_id)) {
    cli::cli_dl(list(`Lab ID` = attrs$lab_id))
    attrs$lab_id <- NULL
  }
  if(!is.null(attrs$cra)) {
    cli::cli_dl(list(`Uncalibrated` = glue::glue("{attrs$cra}±{attrs$error} uncal BP")))
    attrs$cra <- NULL
    attrs$error <- NULL
  }
  cli::cli_dl(attrs)

  invisible(x)
}
