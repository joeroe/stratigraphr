# Functions for graph-based stratigraphic analysis


# stratigraph class -------------------------------------------------------

#' Construct a stratigraphic graph
#'
#' Takes a data frame describing stratigraphic units and relations and turns
#' it into a graph representation.
#'
#' @param data Data frame of stratigraphic units, containing at least a unique
#'   label column and a column describing stratigraphic relations. The relation
#'   column can be either a list column (where each element is a vector of
#'   related units) or a regular column (where each row represents a single
#'   relation). If the relation column is a list, it will be automatically
#'   unnested to long format.
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
#' # List column format (wide format)
#' stratigraph(data.frame(
#'   label = c("A", "B", "C"),
#'   below = list("B", "C", NA)
#' ), "label", "below")
#'
#' # Long format (one relation per row)
#' stratigraph(data.frame(
#'   label = c("A", "B", "C", "C"),
#'   below = c("B", "C", "A", "B")
#' ), "label", "below")
stratigraph <- function(data, label, relation,
                        direction = c("above", "below")) {
  direction <- match.arg(direction)

  # Normalize to long format if relation column is a list
  if (is.list(data[[relation]])) {
    data <- strat_unnest_relations(data, label, relation)
  }

  # Extract edges from long-format data (preserves all relations)
  edges <- strat_connect(data[[label]], data[[relation]], direction)

  # Deduplicate nodes by label (handles long-format input)
  nodes <- data[!duplicated(data[[label]]), , drop = FALSE]

  graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, node_key = label,
                                directed = TRUE)

  # TODO: turn into an as.stratigraph function?
  class(graph) <- c("stratigraph", class(graph))

  strg_validate(graph, warn = TRUE)
}

#' Unnest list-column relations to long format
#'
#' @param data Data frame with a list column for relations
#' @param label Name of the label column
#' @param relation Name of the relation column
#'
#' @return Data frame with unnested relations (one relation per row)
#'
#' @noRd
strat_unnest_relations <- function(data, label, relation) {
  rel_col <- data[[relation]]
  times <- purrr::map_int(rel_col, length)
  indices <- vctrs::vec_rep_each(seq_len(nrow(data)), times = times)
  result <- vctrs::vec_slice(data, indices)
  result[[relation]] <- vctrs::list_unchop(rel_col)
  result
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
