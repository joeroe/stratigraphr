# Functions for tidy radiocarbon data

#' Calibrate radiocarbon dates
#'
#' A thin wrapper of [rcarbon::calibrate()] that returns calibrated dates as a
#' single list rather than a `CalDates`` object. Useful, for example, if you want
#' to add a column of calibrated dates to an existing table with [dplyr::mutate()]
#'
#' @param cra     A vector of uncalibrated radiocarbon ages.
#' @param error   A vector of standard errors associated with `cra`
#' @param ...     Optional arguments passed to calibration function.
#' @param engine  Package to use for calibration, i.e. [rcarbon::calibrate()]
#'                (`"rcarbon"`), [oxcAAR::oxcalCalibrate()] (`"OxCal"`), or
#'                [Bchron::BchronCalibrate()] (`"Bchron"`). Default: `"rcarbon"`.
#'
#' @return A list of `cal` objects.
#' @export
#'
#' @family tidy radiocarbon functions
#'
#' @examples
#' data("shub1_radiocarbon")
#' shub1_radiocarbon %>%
#'   dplyr::mutate(cal = c14_calibrate(cra, error))
c14_calibrate <- function(cra, error, ...,
                          engine = c("rcarbon", "OxCal", "Bchron")) {
  rlang::warn("stratigraphr::c14_calibrate() is deprecated. Please use c14::c14_calibrate() instead.",
              class = "stratigraphr_deprecated_function")

  engine <- rlang::arg_match(engine)

  if (engine == "rcarbon") {
    cals <- rcarbon::calibrate(cra, error, calMatrix = FALSE, ...)
  }

  else if (engine == "OxCal") {
    if(!requireNamespace("oxcAAR")) {
      stop('`engine` = "OxCal" requires package oxcAAR')
    }
    oxcAAR::quickSetupOxcal()
    cals <- oxcAAR::oxcalCalibrate(cra, error, ...)
  }

  else if (engine == "Bchron") {
    if(!requireNamespace("Bchron")) {
      stop('`engine` = "Bchron" requires package Bchron')
    }

    cals <- Bchron::BchronCalibrate(cra, error, ...)
  }

  else {
    stop('`engine` must be one of "rcarbon", "OxCal" or "Bchron"')
  }

  cals <- as_cal(cals)
  return(cals)
}

#' Sum radiocarbon dates with tidy syntax
#'
#' A wrapper for [rcarbon::spd()] that takes a list of calibrated dates rather
#' than a `CalDates` object. This allows you to use the output of [c14_calibrate()] and
#' take advantage of tidy summary syntax ([dplyr::group_by()], etc.)
#'
#' @param cal        A list of `cal` objects.
#' @param time_range Vector of length 2 indicating the range of calendar dates
#'                   over which to sum. If left `NA`, the maximum range of the
#'                   `cal` will be used. See details.
#' @param ...        Optional arguments to be passed to [rcarbon::spd()]
#'
#' @return
#' A `calGrid` object containing the summed probability distribution.
#'
#' @export
#'
#' @details
#' Unlike [rcarbon::spd()], this function will attempt to guess an appropriate
#' time range if it isn't explicitly specified with `time_range`. It's probably
#' a good idea to specify it.
#'
#' @family tidy radiocarbon functions
#'
#' @examples
#' data("shub1_radiocarbon")
#' shub1_radiocarbon %>%
#'   dplyr::mutate(cal = c14_calibrate(cra, error, normalise = FALSE, verbose = FALSE)) %>%
#'   dplyr::group_by(phase) %>%
#'   dplyr::summarise(SPD = c14_sum(cal, spdnormalised = TRUE, verbose = FALSE),
#'                    .groups = "drop_last")
c14_sum <- function(cal, time_range = NA, ...) {
  rlang::warn("stratigraphr::c14_sum() is deprecated. Please use c14::c14_sum() instead.",
              class = "stratigraphr_deprecated_function")

  cal_dates <- as.CalDates.cal(cal)

  if(is.na(time_range)) {
    time_range <- c(min.cal(cal), max.cal(cal))
  }

  summed <- rcarbon::spd(cal_dates, timeRange = time_range, ...)
  return(list(summed$grid))
}
