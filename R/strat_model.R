# Functions for constructing CQL models

#' Generate a CQL phase model
#'
#' @description
#' Constructs a chronological model from a table of radiocarbon dates and
#' phases.
#'
#' Phase models group *events* (in this case radiocarbon dates) into one or more
#' *phases* that form an ordered sequence, but where the order within a phase is
#' unknown. The transition between phases is described by a combination of
#' *boundary* constraints which determine the prior distribution for Bayesian
#' calibration.
#'
#' @param data A data frame of radiocarbon-dated events.
#' @param phase <[tidy_select][dplyr::dplyr_tidy_select]> Variable(s) used to
#'  group events into phases. Ideally these should be factors with levels
#'  ordered chronologically (oldest to youngest). Otherwise, it is assumed that
#'  they have a natural order, e.g. `1..3` or `A..C`.
#' @param label <[`data-masking`][dplyr::dplyr_data_masking]> Variable to use as
#'  labels for the events, e.g. lab numbers.
#' @param c14_age <[`data-masking`][dplyr::dplyr_data_masking]> Variable
#'  describing the uncalibrated radiocarbon ages of events.
#' @param c14_error <[`data-masking`][dplyr::dplyr_data_masking]> Variable
#'  describing the radiocarbon error associated with the events.
#' @param sequence Character. Prior assumption of the ordering of phases. One of:
#'  * `"contiguous"` – phases are assumed to have occurred immediately one after
#'    the other
#'  * `"sequential"` – phases are assumed to have occurred one after the other,
#'    with the possibility of a gap between
#'  * `"overlapping"` – phases are assumed to be independent of each other
#' @param transition Character. Prior assumption of the boundaries of phases.
#'  One of:
#'  * `"gradual"` – assumes a smooth transition at the start and end of each
#'   phase, using a 'trapezium' prior \insertCite{Lee2012}{stratigraphr}.
#'  * `"abrupt"` – assumes an immediate transition at the start and end of each
#'   phase
#'
#' @return
#' A CQL script (see [cql()]).
#'
#' @details
#' This function is based on the phase modelling approach used in OxCal
#' \insertCite{Bronk_Ramsey2013}{stratigraphr}. Currently only uniform prior
#' distributions (using [cql_boundary()]) for phases are supported.
#'
#' @references
#' \insertAllCited{}
#'
#' @family stratigraphic model functions
#'
#' @export
model_phase_cql <- function(data, phase, label, c14_age, c14_error,
                            sequence = c("contiguous", "sequential", "overlapping"),
                            transition = c("gradual", "abrupt")) {
  # TODO: Support multiple phase variables
  # phase <- tidyselect::eval_select(rlang::enquo(phase), data)
  sequence <- rlang::arg_match(sequence)
  transition <- rlang::arg_match(transition)

  data %>%
    dplyr::group_by({{ phase }}) %>%
    dplyr::summarise(
      phase_cql = cql_phase({{ phase }},
                            cql_r_date({{ label }}, {{ c14_age }}, {{ c14_error }}))
    ) ->
    data

  # TODO: Trapezium boundaries

  # TODO: This is disgusting. Rethink.
  if (sequence == "contiguous") {
    data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        phase_cql = cql(
          cql_boundary(""),
          .data$phase_cql
        )
      ) %>%
      purrr::pluck("phase_cql") %>%
      cql() %>%
      {cql(., cql_boundary(""))} %>%
      {cql_sequence("", .)}
  }
  else if (sequence == "sequential") {
    data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        phase_cql = cql(
          cql_boundary(paste({{ phase }}, "start")),
          .data$phase_cql,
          cql_boundary(paste({{ phase }}, "end"))
        )
      ) %>%
      purrr::pluck("phase_cql") %>%
      {cql_sequence("", .)}
  }
  else if (sequence == "overlapping") {
    data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        phase_cql = cql(
          cql_boundary(paste({{ phase }}, "start")),
          .data$phase_cql,
          cql_boundary(paste({{ phase }}, "end"))
        ),
        phase_cql = cql_sequence({{ phase }}, .data$phase_cql)
      ) %>%
      purrr::pluck("phase_cql") %>%
      {cql_phase("", .)}
  }

}