# Functions for working with stratigraphic relations

#' Connect stratigraphic units
#'
#' Constructs a table of directed edges between stratigraphic units based on a
#' vector of relations (e.g. "above" or "below").
#'
#' @param units       Vector of unit labels.
#' @param relations   Vector of relations.
#' @param direction   Are the units in `relations` "above" or "below" the ones
#'   in `units`?
#'
#' @return
#' A data frame of directed edges represented by `to` and `from` columns, which
#' can be used as the `nodes` argument to [tidygraph::tbl_graph()].
#'
#' @export
strat_connect <- function(units, relations, direction = c("above", "below")) {
  direction <- match.arg(direction)

  roots <- rep(units, times = purrr::map_int(relations, length))
  branches <- unlist(relations)

  if (direction == "above") {
    to <- roots
    from <- branches
  } else if (direction == "below") {
    to <- branches
    from <- roots
  }

  df <- data.frame(to, from)
  df <- vctrs::vec_slice(df, vctrs::vec_detect_complete(df))
  df
}

#' Are two relation vectors mirrored?
#'
#' Checks whether one vector of relations is the inverse of another. Typically
#' used to confirm that "above" and "below" columns match and will result in
#' the same stratigraphic graph.
#'
#' @param units      Vector of unit labels.
#' @param relation1  First vector of relations.
#' @param relation2  Second vector of relations.
#'
#' @return
#' `TRUE` or `FALSE`
#'
#' @export
#'
#' @examples
#' data("harris12")
#' strat_is_mirror(harris12$context, harris12$above, harris12$below)
strat_is_mirror <- function(units, relation1, relation2) {
  edges1 <- strat_connect(units, relation1, "above")
  edges2 <- strat_connect(units, relation2, "below")
  edges1 <- vctrs::vec_slice(edges1, vctrs::vec_order(edges1[c("to", "from")]))
  edges2 <- vctrs::vec_slice(edges2, vctrs::vec_order(edges2[c("to", "from")]))
  if (nrow(edges1) != nrow(edges2)) return(FALSE)
  all(edges1 == edges2)
}
