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
#' @return A list of `calGrid` objects. See [rcarbon::calibrate()] for details.
#' @export
#'
#' @note
#' This function only supports the 'grid' method of representing calibrated
#' dates (i.e. `calMatrix` is always set to `FALSE`) and the metadata usually
#' returned by [rcarbon::calibrate()] is discarded.
#'
#' @family radiocarbon functions
#'
#' @examples
#' data("shub1_radiocarbon")
#' shub1_radiocarbon %>%
#'   dplyr::mutate(CalDate = c14_calibrate(cra, error, normalise = FALSE, verbose = FALSE))
c14_calibrate <- function(cra, error, ...) {
  CalDates <- rcarbon::calibrate(cra, error, calMatrix = FALSE, ...)
  return(CalDates$grids)
}

#' Sum radiocarbon dates with tidy syntax
#'
#' A wrapper for [rcarbon::spd()] that takes a list of calibrated dates rather
#' than a `CalDates` object. This allows you to use the output of [cal()] and
#' take advantage of tidy summary syntax ([dplyr::group_by](), etc.)
#'
#' @param cal_dates  A list of `calGrid` objects.
#' @param time_range Vector of length 2 indicating the range of calendar dates
#'                   over which to sum. If `NULL`, the maximum range of the
#'                   `cal_dates` will be used.
#' @param ...        Optional arguments to be passed to [rcarbon::spd()]
#'
#' @return A `calGrid` object containing the summed probability distribution.
#' @export
#'
#' @note
#' Unlike [rcarbon::spd()], this function will attempt to guess the time range
#' if it isn't specified. It's probably a good idea to specify it.
#'
#' @family radiocarbon functions
#'
#' @examples
#' data("shub1_radiocarbon")
#' shub1_radiocarbon %>%
#'   dplyr::mutate(cal_date = c14_calibrate(cra, error, normalise = FALSE, verbose = FALSE)) %>%
#'   dplyr::group_by(phase) %>%
#'   dplyr::summarise(SPD = c14_sum(cal_date, spdnormalised = TRUE, verbose = FALSE),
#'                    .groups = "drop_last")
c14_sum <- function(cal_dates, time_range = NULL, ...) {
  metadata <- purrr::map_df(cal_dates, ~list(StartBP = max(.$calBP),
                                             EndBP = min(.$calBP)))

  if(is.null(time_range)) {
    time_range <- c(max(metadata$StartBP), min(metadata$EndBP))
  }

  CalDates <- list(metadata = metadata,
                   grids = cal_dates,
                   calMatrix = NA)
  class(CalDates) <- c("CalDates", "list")

  summed <- rcarbon::spd(CalDates, timeRange = time_range, ...)
  return(list(summed$grid))
}
