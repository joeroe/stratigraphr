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
#' circle <- stratigraph(data.frame(
#'   label = LETTERS[1:4],
#'   below = c("B", "C", "D", "A")
#' ), "label", "below", "below")
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

strg_locate_cycles <- function(graph) {
  if (tidygraph::with_graph(graph, tidygraph::graph_is_dag())) {
    rlang::warn("`graph` does not contain cycles")
    return(NA)
  }

  bad_edges <- igraph::feedback_arc_set(graph)
}

# Validation functions ----------------------------------------------------

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
#' @importFrom rlang .data
strat_is_mirror <- function(units, relation1, relation2) {
  edges1 <- strat_connect(units, relation1, "above")
  edges2 <- strat_connect(units, relation2, "below")
  edges1 <- dplyr::arrange(edges1, .data$to, .data$from)
  edges2 <- dplyr::arrange(edges2, .data$to, .data$from)
  return(all(edges1 == edges2))
}
