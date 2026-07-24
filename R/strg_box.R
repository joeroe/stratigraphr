# strg_box.R
# Box-drawing rendering of stratigraphic graphs

# Render a stratigraph with box-drawing characters using Sugiyama layout
#' @noRd
strg_box_render <- function(strg, max_lines = NULL, max_label_width = 8) {
  # Extract data
  nodes <- tidygraph::as_tibble(strg, active = "nodes")
  labels <- as.character(nodes[[1]])

  n_nodes <- nrow(nodes)

  # Handle edge cases
  if (n_nodes == 1) {
    return(substr(labels[1], 1, max_label_width))
  }

  # Compute Sugiyama layout
  result <- igraph::layout_with_sugiyama(strg, hgap = 2, vgap = 1)
  
  # Combine layout for original nodes and dummy nodes
  if (!is.null(result$layout.dummy) && nrow(result$layout.dummy) > 0) {
    full_layout <- rbind(result$layout, result$layout.dummy)
  } else {
    full_layout <- result$layout
  }
  
  # Get extended graph for edge routing (includes dummy nodes)
  extd_graph <- result$extd_graph
  extd_edges <- as.data.frame(igraph::as_edgelist(extd_graph, names = FALSE))
  names(extd_edges) <- c("from", "to")

  # Convert layout to grid positions
  col_pos <- strg_box_compute_col_positions(full_layout, labels, max_label_width, n_nodes)
  node_to_row <- strg_box_compute_label_rows(full_layout, nrow(full_layout))
  
  # Center labels on their column positions
  label_widths <- pmin(nchar(labels), max_label_width)
  label_offsets <- floor((label_widths - 1) / 2)
  
  # Calculate adjusted column positions to ensure no label extends to column < 1
  min_col_after_centering <- min(col_pos[1:n_nodes] - label_offsets)
  shift <- max(0, 1 - min_col_after_centering)
  col_pos <- col_pos + shift
  
  n_rows <- max(node_to_row) + 1
  max_right_extent <- max(col_pos[1:n_nodes] + floor(label_widths / 2))
  n_cols <- max_right_extent

  # Initialize canvas
  canvas <- matrix(" ", nrow = n_rows, ncol = n_cols)

  # Draw edges (on even rows) using extended graph
  if (nrow(extd_edges) > 0) {
    edge_ops <- strg_box_generate_edge_operations(extd_edges, node_to_row, col_pos)
    if (nrow(edge_ops) > 0) {
      canvas[cbind(edge_ops$row, edge_ops$col)] <- edge_ops$char
    }
  }

  # Place labels (on odd rows) - only for original nodes
  label_ops <- strg_box_generate_label_operations(labels, node_to_row[1:n_nodes], col_pos[1:n_nodes], max_label_width)
  canvas[cbind(label_ops$row, label_ops$col)] <- label_ops$char
  
  # Place vertical connectors at dummy node positions (on odd rows)
  if (nrow(full_layout) > n_nodes) {
    dummy_ops <- strg_box_generate_dummy_operations(node_to_row, col_pos, n_nodes)
    if (nrow(dummy_ops) > 0) {
      canvas[cbind(dummy_ops$row, dummy_ops$col)] <- dummy_ops$char
    }
  }

  # Convert to lines
  strg_box_canvas_to_lines(canvas, max_lines)
}

# Compute column positions from layout coordinates
#' @noRd
strg_box_compute_col_positions <- function(layout, labels, max_label_width, n_original) {
  n_total <- nrow(layout)
  
  # Calculate uniform column width based on actual max label width
  actual_max_width <- min(max(nchar(labels)), max_label_width)
  column_width <- actual_max_width + 1  # label width + 1 char gap for edges
  
  all_x <- sort(unique(layout[, 1]))
  n_unique_x <- length(all_x)
  
  # All columns have uniform width
  x_to_col <- 1 + cumsum(c(0, rep(column_width, n_unique_x - 1)))
  names(x_to_col) <- as.character(all_x)
  
  stats::setNames(x_to_col[as.character(layout[, 1])], seq_len(n_total))
}

# Compute label row assignments from layout coordinates
#' @noRd
strg_box_compute_label_rows <- function(layout, n_nodes) {
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
strg_box_generate_edge_operations <- function(edges, node_to_row, col_pos) {
  layers <- sort(unique(node_to_row))
  children <- split(edges$to, edges$from)

  all_ops <- purrr::map2(layers[-length(layers)], layers[-1], function(parent_layer, child_layer) {
    parent_nodes <- names(node_to_row)[node_to_row == parent_layer]
    child_nodes <- names(node_to_row)[node_to_row == child_layer]
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
  # Create spans for each connection
  raw_spans <- list()
  
  for (conn in connections) {
    parent_col <- conn$parent_col
    child_cols <- conn$child_cols
    
    if (length(child_cols) > 1) {
      # Split: create span from parent to each child
      for (child_col in child_cols) {
        if (parent_col != child_col) {
          raw_spans <- c(raw_spans, list(list(
            min_col = min(parent_col, child_col),
            max_col = max(parent_col, child_col)
          )))
        }
      }
    } else if (length(child_cols) == 1) {
      child_col <- child_cols[1]
      if (parent_col != child_col) {
        # Diagonal: create span from parent to child
        raw_spans <- c(raw_spans, list(list(
          min_col = min(parent_col, child_col),
          max_col = max(parent_col, child_col)
        )))
      }
    }
  }
  
  # Merge overlapping spans
  if (length(raw_spans) == 0) {
    return(list())
  }
  
  # Sort spans by min_col
  raw_spans <- raw_spans[order(purrr::map_int(raw_spans, "min_col"))]
  
  merged_spans <- list(raw_spans[[1]])
  if (length(raw_spans) > 1) {
    for (i in 2:length(raw_spans)) {
      current <- raw_spans[[i]]
      last <- merged_spans[[length(merged_spans)]]
      
      # If current span overlaps or is adjacent to last span, merge them
      if (current$min_col <= last$max_col + 1) {
        merged_spans[[length(merged_spans)]] <- list(
          min_col = last$min_col,
          max_col = max(last$max_col, current$max_col)
        )
      } else {
        merged_spans <- c(merged_spans, list(current))
      }
    }
  }
  
  merged_spans
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
  if (is.na(result)) NULL else pillar::style_subtle(result)
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
      
      # Center label on column position
      offset <- floor((length(chars) - 1) / 2)

      data.frame(
        row = rep(row, length(chars)),
        col = col - offset + seq_along(chars) - 1,
        char = chars
      )
    }
  )
}

# Generate dummy node vertical connector operations
#' @noRd
strg_box_generate_dummy_operations <- function(node_to_row, col_pos, n_original) {
  n_total <- length(node_to_row)
  if (n_total <= n_original) {
    return(data.frame(row = integer(), col = integer(), char = character(),
                      stringsAsFactors = FALSE))
  }
  
  # Dummy nodes are indices (n_original + 1) to n_total
  dummy_indices <- (n_original + 1):n_total
  
  purrr::map_dfr(dummy_indices, function(idx) {
    data.frame(
      row = node_to_row[idx],
      col = col_pos[idx],
      char = pillar::style_subtle("\u2502"),
      stringsAsFactors = FALSE
    )
  })
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
