# Tidy wrappers for the rcarbon package

#' Calibrate radiocarbon dates with tidy syntax
#'
#' A thin wrapper of [rcarbon::calibrate()] that returns calibrated dates as a
#' single list rather than a `CalDates`` object. Useful, for example, if you want
#' to add a column of calibrated dates to an existing table with [dplyr::mutate()]
#'
#' @param cra     A vector of uncalibrated radiocarbon ages.
#' @param error   A vector of standard errors associated with `cra`
#' @param ...     Optional arguments passed to [rcarbon::calibrate()]
#'
#' @return A list of `cal` objects.
#' @export
#'
#' @details
#' See [rcarbon::calibrate()] for details of the calibration options.
#'
#' This function only supports the 'grid' method of representing calibrated
#' dates (i.e. `calMatrix` is always set to `FALSE`) and the metadata usually
#' returned by [rcarbon::calibrate()] is discarded.
#'
#' @family tidy radiocarbon functions
#'
#' @examples
#' data("shub1_radiocarbon")
#' shub1_radiocarbon %>%
#'   dplyr::mutate(cal = c14_calibrate(cra, error, normalise = FALSE, verbose = FALSE))
c14_calibrate <- function(cra, error, ...) {
  CalDates <- rcarbon::calibrate(cra, error, calMatrix = FALSE, ...)
  cals <- as_cal(CalDates)
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
  cal_dates <- as.CalDates.cal(cal)

  if(is.na(time_range)) {
    time_range <- c(min.cal(cal), max.cal(cal))
  }

  summed <- rcarbon::spd(cal_dates, timeRange = time_range, ...)
  return(list(summed$grid))
}
