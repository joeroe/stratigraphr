# strg_prune.R
# strg_prune() and helper functions.

#' Remove redundant relations from a stratigraph
#'
#' strg_prune() removes redundant relations from a stratigraphic
#' graph by computing its
#' [transitive reduction](https://en.wikipedia.org/wiki/Transitive_reduction).
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
  keep_ids <- strg_reduction_edge_ids(strg)
  strg |>
    tidygraph::activate("edges") |>
    dplyr::filter(seq_len(dplyr::n()) %in% keep_ids)
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
      domain = lapply(
        seq_len(nrow(tidygraph::.N())),
        sets::as.set
      ),
      graph = tidygraph::.E()
    )
  })
}

#' Get edge IDs to keep in transitive reduction
#'
#' @noRd
#' @keywords {internal}
strg_reduction_edge_ids <- function(strg) {
  relation <- strg_to_relation(strg)
  reduction <- relations::transitive_reduction(relation)
  reduced_edges <- edges_from_relation(reduction)
  edge_vector <- c(rbind(reduced_edges[, 1], reduced_edges[, 2]))
  igraph::get_edge_ids(strg, edge_vector)
}

#' @noRd
#' @keywords {internal}
strg_has_redundant_relations <- function(strg) {
  if (igraph::gsize(strg) == 0) {
    return(FALSE)
  }
  if (!tidygraph::with_graph(strg, tidygraph::graph_is_dag())) {
    return(NA)
  }
  length(strg_reduction_edge_ids(strg)) < igraph::gsize(strg)
}

#' @noRd
#' @keywords {internal}
strg_redundant_edges <- function(strg) {
  if (!tidygraph::with_graph(strg, tidygraph::graph_is_dag())) {
    return(NA)
  }

  original <- as.data.frame(tidygraph::as_tibble(strg, active = "edges"))
  keep_ids <- strg_reduction_edge_ids(strg)
  reduced <- vctrs::vec_slice(original, keep_ids)

  # Edges in original but not in reduction
  vctrs::vec_slice(original, !vctrs::vec_in(original, reduced))
}

#' Extract edge list from an endorelation
#'
#' Via igraph. Returns a two-column data.frame, ignoring the names of the
#' adjacency matrix from the relation (so we assume we're working with
#' bare indices, as returned by strg_to_relation()).
#'
#' @noRd
#' @keywords {internal}
edges_from_relation <- function(relation) {
  adjacency <- relations::relation_incidence(relation)
  graph <- igraph::graph_from_adjacency_matrix(
    adjacency, mode = "directed",
    add.colnames = FALSE
  )
  data.frame(igraph::as_edgelist(graph))
}
