# Functions for constructing graphs from stratigraphic sequences

#' Construct a Harris matrix from a stratigraphic sequence.
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
#' @examples
#' data(harris12)
#' matrix <- harris(harris12, "context", "above")
#' plot(matrix)
harris <- function(data, context, relation) {
  to <- rep(data[[context]], times = map_int(data[[relation]], length))
  from <- unlist(data[[relation]])
  tibble::tibble(to, from) %>%
    drop_na() %>%
    return()
}