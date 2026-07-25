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
#'
#' @examples
#' circle <- stratigraph(data.frame(
#'   label = LETTERS[1:4],
#'   below = c("B", "C", "D", "A")
#' ), "label", "below", "below")
stratigraph <- function(data, label, relation,
                        direction = c("above", "below")) {
  direction <- match.arg(direction)

  edges <- strat_connect(data[[label]], data[[relation]], direction)
  graph <- tidygraph::tbl_graph(nodes = data, edges = edges, node_key = label,
                                directed = TRUE)

  # TODO: turn into an as.stratigraph function?
  class(graph) <- c("stratigraph", class(graph))

  strg_validate(graph, warn = TRUE)
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

#' Print a stratigraphic graph
#'
#' Prints a stratigraph object with a summary header and box-drawing
#' visualization using a Sugiyama-style layered layout.
#'
#' @param x A [stratigraph()] object.
#' @param n Number of stratigraphic layers to display. Default: 10.
#' @param max_label_width Maximum width of labels in characters. Default: 8.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @noRd
#' @exportS3Method
print.stratigraph <- function(x, n = 10, max_label_width = 8, ...) {
  n_nodes <- igraph::gorder(x)
  n_edges <- igraph::gsize(x)
  issues <- strg_validity_issues(x)

  cat(pillar::style_subtle(sprintf(
    "# A stratigraph: %d units and %d relations\n",
    n_nodes, n_edges
  )))

  if (length(issues) == 0) {
    cat(
      pillar::style_subtle("# "),
      cli::col_green(cli::symbol$tick, " Valid stratigraphic graph\n"),
      sep = ""
    )

    if (n_nodes == 0) {
      return(invisible(x))
    }

    result <- strg_box_render(
      x, n = n, max_label_width = max_label_width
    )
    cat(result$lines, sep = "\n")

    # Print footer if truncated
    if (result$levels_shown < result$total_levels) {
      remaining_nodes <- result$total_nodes - result$nodes_shown
      remaining_levels <- result$total_levels - result$levels_shown

      cat(pillar::style_subtle(sprintf(
        "# %d more units on %d more layers",
        remaining_nodes, remaining_levels
      )), "\n")
      cat(pillar::style_subtle(
        "# \u2139 Use `print(n = ...)` to see more layers"
      ), "\n")
    }
  } else {
    cat(
      pillar::style_subtle("# "),
      cli::col_red(
        cli::symbol$cross,
        " Invalid stratigraphic graph\n"
      ),
      sep = ""
    )
    cli::cli_bullets(issues)
  }

  invisible(x)
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
strat_is_mirror <- function(units, relation1, relation2) {
  edges1 <- strat_connect(units, relation1, "above")
  edges2 <- strat_connect(units, relation2, "below")
  edges1 <- vctrs::vec_slice(edges1, vctrs::vec_order(edges1[c("to", "from")]))
  edges2 <- vctrs::vec_slice(edges2, vctrs::vec_order(edges2[c("to", "from")]))
  all(edges1 == edges2)
}
