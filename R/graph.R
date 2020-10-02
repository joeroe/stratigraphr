# Functions for constructing graphs from stratigraphic sequences

#' Stratigraphic graphs
#'
#' @param data      `tibble` or `data.frame` of contexts and the stratigraphic
#'                   relationships between them.
#' @param context   `character`. Name of the column containing context labels (nodes)
#' @param relation  `character`. Name of the column describing relations (edges)
#'                               between contexts
#' @param type      Type of graph. Currently only "harris" is supported.
#'
#' @return  A `stratigraph` object.
#' @export
stratigraph <- function(data, context, relation, type = "harris") {
  # Join nodes
  if (type == "harris") {
    edges <- harris(data, context, relation)
  }
  else {
    stop("Stratigraphic graphs other than Harris matrices not yet implemented!")
  }

  # Construct graph
  stratigraph <- tidygraph::tbl_graph(nodes = data, edges = edges,
                                      directed = TRUE)

  # Add stratigraph class
  # TODO: turn into an as.stratigraph function?
  class(stratigraph) %>%
    purrr::prepend("stratigraph") ->
    class(stratigraph)
  attr(stratigraph, "type") <- type

  # Check validity and return
  if (type == "harris") {
    if (is_valid_harris(stratigraph)) {
      return(stratigraph)
    }
    else {
      return(NULL)
    }
  }
}

#' Harris Matrices
#'
#' Join stratigraphic units using the Harris matrix method (Harris 1979).
#'
#' @param data      `tibble` or `data.frame` of contexts and the stratigraphic
#'                   relationships between them.
#' @param context   `character`. Name of the column containing context labels (nodes)
#' @param relation  `character`. Name of the column describing relations (edges)
#'                               between contexts
#'
#' @return
#' A `tibble` of edges with `to` and `from` columns (see [tidygraph::tbl_graph()])
#'
#' @export
#'
#' @references
#' * Harris, E. C. 1979. *Principles of archaeological stratigraphy*. London:
#'   Academic Press.
#'
#' @examples
#' data(harris12)
#' harris(harris12, "context", "above")
harris <- function(data, context, relation) {
  to <- rep(data[[context]], times = purrr::map_int(data[[relation]], length))
  from <- unlist(data[[relation]])
  tibble::tibble(to, from) %>%
    tidyr::drop_na() %>%
    return()
}

#' Validate stratigraphic graphs
#'
#' Checks whether a stratigraphic graph is a valid Harris matrix.
#'
#' @param stratigraph   A `stratigraph` object (see [stratigraph()])
#' @param warn          Display warnings for invalid graphs (Default: `TRUE`).
#'
#' @return `TRUE` or `FALSE`
#' @export
is_valid_harris <- function(stratigraph, warn = TRUE) {
  if (!tidygraph::with_graph(stratigraph, tidygraph::graph_is_dag())) {
    if(warn) warning("Invalid Harris matrix: graph contains cycles")
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}