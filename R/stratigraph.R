# Functions for graph-based stratigraphic analysis


# stratigraph class -------------------------------------------------------

#' Construct a stratigraphic graph
#'
#' Takes a data frame describing stratigraphic units and relations and turns
#' it into a graph representation.
#'
#' @param data Data frame of stratigraphic units, containing at least a unique
#'   label column and a column describing stratigraphic relations.
#' @param label Name of the column containing labels of the stratigraphic units.
#' @param relation  Name of the column describing the stratigraphic relations
#'   between units.
#' @param direction Direction described by `relation`, i.e. are the units in
#'   that column "above" or "below" the ones in units. Default: "above".
#'
#' @return
#' A `stratigraph` object.
#'
#' @export
stratigraph <- function(data, label, relation, direction = c("above", "below")) {
  direction <- match.arg(direction)

  edges <- strat_connect(data[[label]], data[[relation]], direction)
  graph <- tidygraph::tbl_graph(nodes = data, edges = edges, node_key = label,
                                directed = TRUE)

  # TODO: turn into an as.stratigraph function?
  class(graph) <- c("stratigraph", class(graph))

  invisible(strat_is_valid(graph))

  return(graph)
}

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

  if(direction == "above") {
    to <- roots
    from <- branches
  }
  else if (direction == "below") {
    to <- branches
    from <- roots
  }

  df <- data.frame(to, from)
  df <- tidyr::drop_na(df)
  return(df)
}

#' Is an object a valid stratigraphic graph?
#'
#' @details
#' Checks whether a stratigraph object is a valid stratigraphic graph. Currently
#' looks for:
#'
#' * Whether the graph contains cycles
#'
#' @param stratigraph   A `stratigraph` object (see [stratigraph()]).
#' @param warn          Display warnings for invalid graphs? Default: `TRUE`.
#'
#' @return
#' `TRUE` or `FALSE`.
#'
#' @export
strat_is_valid <- function(stratigraph, warn = TRUE) {
  if (!tidygraph::with_graph(stratigraph, tidygraph::graph_is_dag())) {
    if(warn) warning("Invalid stratigraphic graph: contains cycles")
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}
