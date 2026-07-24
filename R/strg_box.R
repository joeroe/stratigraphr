# strg_box.R
# Box-drawing rendering of stratigraphic graphs

#' Print a stratigraphic graph
#'
#' Prints a stratigraph object with a summary header and box-drawing
#' visualization using a Sugiyama-style layered layout.
#'
#' @param x A [stratigraph()] object.
#' @param max_lines Maximum number of lines to display. Default: 20.
#' @param max_label_width Maximum width of labels in characters. Default: 8.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @exportS3Method print stratigraph
print.stratigraph <- function(x, max_lines = 20, max_label_width = 8, ...) {
  n_nodes <- igraph::gorder(x)
  n_edges <- igraph::gsize(x)
  valid <- strg_is_valid(x, warn = FALSE)

  cat(sprintf("# A stratigraph: %d units and %d relations\n", n_nodes, n_edges))
  cat(sprintf("# Valid: %s\n", valid))

  if (n_nodes == 0) {
    return(invisible(x))
  }

  tree <- strg_box_render(x, max_lines = max_lines,
                            max_label_width = max_label_width)
  cat(tree, sep = "\n")

  invisible(x)
}

# Render a stratigraph with box-drawing characters using Sugiyama layout
#' @noRd
strg_box_render <- function(strg, max_lines = NULL, max_label_width = 8) {
  # Extract data
  nodes <- tidygraph::as_tibble(strg, active = "nodes")
  labels <- as.character(nodes[[1]])
  edges <- tidygraph::as_tibble(strg, active = "edges")

  n_nodes <- nrow(nodes)

  # Handle edge cases
  if (n_nodes == 1) {
    return(substr(labels[1], 1, max_label_width))
  }

  # Compute Sugiyama layout
  result <- igraph::layout_with_sugiyama(strg, hgap = 2, vgap = 1)
  layout <- result$layout

  # Convert layout to grid positions
  col_pos <- strg_box_compute_col_positions(layout, labels, max_label_width)
  node_to_label_row <- strg_box_compute_label_rows(layout, labels, max_label_width)
  n_rows <- max(node_to_label_row) + 1
  n_cols <- max(col_pos) + max_label_width

  # Initialize canvas
  canvas <- matrix(" ", nrow = n_rows, ncol = n_cols)

  # Draw edges (on even rows)
  if (nrow(edges) > 0) {
    edge_ops <- strg_box_generate_edge_operations(edges, node_to_label_row, col_pos, labels)
    if (nrow(edge_ops) > 0) {
      canvas[cbind(edge_ops$row, edge_ops$col)] <- edge_ops$char
    }
  }

  # Place labels (on odd rows)
  label_ops <- strg_box_generate_label_operations(labels, node_to_label_row, col_pos, max_label_width)
  canvas[cbind(label_ops$row, label_ops$col)] <- label_ops$char

  # Convert to lines
  strg_box_canvas_to_lines(canvas, max_lines)
}

# Compute column positions from layout coordinates
#' @noRd
strg_box_compute_col_positions <- function(layout, labels, max_label_width) {
  all_x <- sort(unique(layout[, 1]))
  
  label_widths <- purrr::map_int(all_x, function(x) {
    node_at_x <- which(layout[, 1] == x)[1]
    min(nchar(labels[node_at_x]), max_label_width)
  })
  
  x_to_col <- 1 + cumsum(c(0, label_widths[-length(label_widths)] + 3))
  names(x_to_col) <- as.character(all_x)
  
  stats::setNames(x_to_col[as.character(layout[, 1])], seq_along(labels))
}

# Compute label row assignments from layout coordinates
#' @noRd
strg_box_compute_label_rows <- function(layout, labels, max_label_width) {
  n_nodes <- length(labels)
  y_max <- max(layout[, 2])
  layers <- round(y_max - layout[, 2]) + 1
  
  layer_groups <- split(seq_len(n_nodes), layers)
  n_layers <- length(layer_groups)
  
  label_rows <- 1 + (seq_len(n_layers) - 1) * 2
  
  node_to_label_row <- rep(label_rows, times = purrr::map_int(layer_groups, length))
  names(node_to_label_row) <- unlist(layer_groups)
  node_to_label_row[order(as.integer(names(node_to_label_row)))]
}

# Generate edge drawing operations
#' @noRd
strg_box_generate_edge_operations <- function(edges, node_to_label_row, col_pos, labels) {
  layers <- sort(unique(node_to_label_row))
  children <- split(edges$to, edges$from)

  all_ops <- purrr::map2(layers[-length(layers)], layers[-1], function(parent_layer, child_layer) {
    parent_nodes <- names(node_to_label_row)[node_to_label_row == parent_layer]
    child_nodes <- names(node_to_label_row)[node_to_label_row == child_layer]
    edge_row <- parent_layer + 1

    strg_box_edge_ops_for_layer_pair(parent_nodes, child_nodes, children, col_pos, edge_row)
  })

  dplyr::bind_rows(all_ops)
}

# Compute edge operations for a single layer pair
#' @noRd
strg_box_edge_ops_for_layer_pair <- function(parent_nodes, child_nodes, children, col_pos, edge_row) {
  connections <- strg_box_build_connections(parent_nodes, child_nodes, children, col_pos)

  if (length(connections) == 0) {
    return(data.frame(row = integer(), col = integer(), char = character(),
                      stringsAsFactors = FALSE))
  }

  has_parent_above <- unique(purrr::map_int(connections, "parent_col"))
  has_child_below <- unique(unlist(purrr::map(connections, "child_cols")))

  spans <- strg_box_edge_spans(connections, has_parent_above, has_child_below)

  all_cols <- unique(c(
    has_parent_above,
    has_child_below,
    unlist(lapply(spans, function(s) s$min_col:s$max_col))
  ))

  purrr::map_dfr(all_cols, function(col) {
    up <- col %in% has_parent_above
    down <- col %in% has_child_below
    span_info <- strg_box_column_span_info(col, spans)

    left <- span_info$in_span && !span_info$at_left_end
    right <- span_info$in_span && !span_info$at_right_end

    char <- strg_box_edge_char_at(up, down, left, right)

    if (!is.null(char)) {
      data.frame(row = edge_row, col = col, char = char, stringsAsFactors = FALSE)
    } else {
      data.frame(row = integer(), col = integer(), char = character(),
                 stringsAsFactors = FALSE)
    }
  })
}

# Build connection list for a layer pair
#' @noRd
strg_box_build_connections <- function(parent_nodes, child_nodes, children, col_pos) {
  connections <- purrr::map(parent_nodes, function(p_node) {
    p_col <- col_pos[p_node]
    p_children <- children[[p_node]]
    p_children <- p_children[p_children %in% child_nodes]

    if (length(p_children) > 0) {
      list(parent_col = p_col, child_cols = col_pos[as.character(p_children)])
    } else {
      NULL
    }
  })
  purrr::compact(connections)
}

# Determine span membership for a column
#' @noRd
strg_box_column_span_info <- function(col, spans) {
  in_span <- any(purrr::map_lgl(spans, ~ col >= .x$min_col && col <= .x$max_col))
  at_left_end <- any(purrr::map_lgl(spans, ~ col == .x$min_col && col <= .x$max_col))
  at_right_end <- any(purrr::map_lgl(spans, ~ col == .x$max_col && col >= .x$min_col))

  list(in_span = in_span, at_left_end = at_left_end, at_right_end = at_right_end)
}

# Compute horizontal spans for edge drawing
#' @noRd
strg_box_edge_spans <- function(connections, has_parent_above, has_child_below) {
  is_split <- length(has_child_below) > 1
  is_merge <- length(has_parent_above) > 1

  if (is_split) {
    all_child_cols <- unlist(purrr::map(connections, "child_cols"))
    parent_col <- connections[[1]]$parent_col
    list(list(min_col = min(all_child_cols), max_col = max(all_child_cols)))
  } else if (is_merge) {
    all_parent_cols <- purrr::map_int(connections, "parent_col")
    child_col <- connections[[1]]$child_cols[1]
    all_cols <- c(all_parent_cols, child_col)
    list(list(min_col = min(all_cols), max_col = max(all_cols)))
  } else {
    list()
  }
}

# Lookup table for box-drawing characters based on connection directions
#' @noRd
strg_box_edge_char_at <- function(up, down, left, right) {
  key <- paste0(as.integer(up), as.integer(down), as.integer(left), as.integer(right))

  chars <- c(
    "0000" = NA,
    "1000" = "\u2502", "0100" = "\u2502",
    "0010" = "\u2500", "0001" = "\u2500",
    "1100" = "\u2502",
    "1010" = "\u2518", "1001" = "\u2514",
    "0110" = "\u2510", "0101" = "\u250C",
    "0011" = "\u2500",
    "1110" = "\u2524", "1101" = "\u251C",
    "1011" = "\u2534", "0111" = "\u252C",
    "1111" = "\u253C"
  )

  result <- chars[[key]]
  if (is.na(result)) NULL else result
}

# Generate label placement operations
#' @noRd
strg_box_generate_label_operations <- function(labels, node_to_label_row, col_pos, max_label_width) {
  purrr::pmap_dfr(
    list(
      label = labels,
      row = node_to_label_row,
      col = col_pos
    ),
    function(label, row, col) {
      label_truncated <- substr(label, 1, max_label_width)
      chars <- strsplit(label_truncated, "")[[1]]

      data.frame(
        row = rep(row, length(chars)),
        col = col + seq_along(chars) - 1,
        char = chars
      )
    }
  )
}

# Convert canvas matrix to character lines
#' @noRd
strg_box_canvas_to_lines <- function(canvas, max_lines) {
  lines <- apply(canvas, 1, paste0, collapse = "")

  # Trim trailing spaces
  lines <- sub("\\s+$", "", lines)

  # Remove empty lines at start and end
  lines <- purrr::keep(lines, ~ .x != "")

  # Truncate if max_lines specified
  if (!is.null(max_lines) && length(lines) > max_lines) {
    lines <- c(lines[1:(max_lines - 1)], "...")
  }

  lines
}
