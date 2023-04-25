# strg_prune.R
# strg_prune() and helper functions.

#' Remove redundant relations from a stratigraph
#'
#' strg_prune() removes redundant relations from a stratigraphic graph by
#' computing its [transitive reduction](https://en.wikipedia.org/wiki/Transitive_reduction).
#' The result is a 'pruned' graph that follows Harris' "Law of Stratigraphical
#' Succession": that only the uppermost and undermost relations are significant
#' when placing a unit in a stratigraphic sequence.
#'
#' @param strg A [stratigraph()] object to prune.
#'
#' @return
#' A [stratigraph] with redundant relations removed.
#'
#' @export
#'
#' @examples
#'
#' bushy_stratigraphy <- stratigraph(
#'   tibble::tibble(
#'     id = letters[1:5],
#'     above = list(NA, "a", "a", c("a", "b", "c"), c("a", "c", "d"))
#'   ),
#'   "id", "above"
#' )
#'
#' strg_prune(bushy_stratigraphy)
strg_prune <- function(strg) {
  # TODO: use tidygraph morphers to maintain original data?
  to_transitive_reduction(strg)
}

#' Generate the transitive reduction of a graph
#'
#' Morpher function (see [tidygraph::morphers]) that returns the transitive
#' reduction of a graph.
#'
#' @noRd
#' @keywords {internal}
to_transitive_reduction <- function(graph) {
  relation <- strg_to_relation(graph)
  reduction <- relations::transitive_reduction(relation)
  tidygraph::tbl_graph(
    tidygraph::with_graph(graph, tidygraph::.N()),
    edges_from_relation(reduction),
    directed = TRUE
  )
}

#' Convert tidy graph to endorelation
#'
#' Only the node and edge indices are preserved; no names or attributes.
#'
#' @noRd
#' @keywords {internal}
strg_to_relation <- function(graph) {
  tidygraph::with_graph(graph, {
    relations::endorelation(
      domain = lapply(1:nrow(tidygraph::.N()), sets::as.set),
      graph = tidygraph::.E()
    )
  })
}

#' Extract edge list from an endorelation
#'
#' Via igraph. Returns a two-column data.frame, ignoring the names of the
#' adjacency matrix from the relation (so we assume we're working with
#' bare indices, as returned by [strg_to_relation()]).
#'
#' @noRd
#' @keywords {internal}
edges_from_relation <- function(relation) {
  adjacency <- relations::relation_incidence(relation)
  graph <- igraph::graph_from_adjacency_matrix(adjacency, mode = "directed",
                                               add.colnames = FALSE)
  data.frame(igraph::as_edgelist(graph))
}

