# Validation of stratigraphic graphs

#' Validation of stratigraphic graphs
#'
#' @details
#' Checks whether a stratigraph object is a valid stratigraphic graph. Currently
#' looks for:
#'
#' * Whether the graph contains cycles (violating the law of stratigraphic
#'   superposition)
#' * Whether the graph contains redundant relations (i.e. relations that are
#'   implied by transitivity)
#'
#' `strg_is_valid()` returns `TRUE` or `FALSE` silently. `strg_validate()`
#' signals an error (or warning if `warn = TRUE`) with details of any validity
#' issues found.
#'
#' @param stratigraph   A `stratigraph` object (see [stratigraph()]).
#' @param warn          If `TRUE`, signal warnings instead of errors.
#'                      Default: `FALSE`.
#'
#' @return
#' `strg_is_valid()` returns `TRUE` or `FALSE`. `strg_validate()` returns
#' the object (or signals an error/warning if invalid).
#'
#' @export
#'
#' @examples
#' data("harris12")
#' strg <- stratigraph(harris12, "context", "above")
#'
#' strg_is_valid(strg)
#' strg_validate(strg)
strg_is_valid <- function(stratigraph) {
  length(strg_validity_issues(stratigraph)) == 0
}

#' @rdname strg_is_valid
#' @export
strg_validate <- function(stratigraph, warn = FALSE) {
  issues <- strg_validity_issues(stratigraph)

  if (length(issues) > 0) {
    msg <- "Invalid stratigraphic graph:"
    if (warn) {
      rlang::warn(msg, body = issues, class = "invalid_stratigraph")
    } else {
      rlang::abort(msg, body = issues, class = "invalid_stratigraph")
    }
  }

  stratigraph
}

#' @noRd
#' @keywords internal
strg_validity_issues <- function(stratigraph) {
  issues <- character(0)

  if (!tidygraph::with_graph(stratigraph, tidygraph::graph_is_dag())) {
    issues <- c(issues, "!" = "Contains cycles")
  }

  if (isTRUE(strg_has_redundant_relations(stratigraph))) {
    issues <- c(issues,
      "!" = "Contains redundant relations",
      "i" = "Use `strg_prune()` to remove redundant relations"
    )
  }

  issues
}

#' @noRd
#' @keywords internal
strg_locate_cycles <- function(graph) {
  if (tidygraph::with_graph(graph, tidygraph::graph_is_dag())) {
    rlang::warn("`graph` does not contain cycles")
    return(NA)
  }

  bad_edges <- igraph::feedback_arc_set(graph)
  bad_edges
}
