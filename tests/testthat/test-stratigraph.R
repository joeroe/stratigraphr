test_that("stratigraphs constructed from above and below are isomorphic", {
  data("harris12")
  h12_above <- stratigraph(harris12, "context", "above", "above")
  h12_below <- stratigraph(harris12, "context", "below", "below")

  expect_true(
    tidygraph::with_graph(
      h12_above,
      tidygraph::graph_is_isomorphic_to(h12_below)
    )
  )
})

test_that("strg_is_valid() returns TRUE for the harris12 dataset", {
  data("harris12")
  h12_graph <- stratigraph(harris12, "context", "above")
  expect_true(strg_is_valid(h12_graph))
})

test_that("strg_validate() detects cycles", {
  data("harris12")
  harris12$above[1] <- list(c("natural"))
  expect_warning(h12_graph <- stratigraph(harris12, "context", "above"))
  expect_error(strg_validate(h12_graph), class = "invalid_stratigraph")
  expect_false(strg_is_valid(h12_graph))
})

test_that("strg_validate() detects redundant relations", {
  bushy <- suppressWarnings(stratigraph(
    tibble::tibble(
      id = letters[1:5],
      above = list(NA, "a", "a", c("a", "b", "c"), c("a", "c", "d"))
    ),
    "id", "above"
  ))
  expect_error(strg_validate(bushy), class = "invalid_stratigraph")
  expect_false(strg_is_valid(bushy))
})

test_that("print.stratigraph() produces output", {
  data("harris12")
  h12_graph <- stratigraph(harris12, "context", "above")

  output <- cli::ansi_strip(capture.output(print(h12_graph)))
  expect_true(length(output) > 0)
  expect_true(any(grepl("A stratigraph:", output)))
  expect_true(any(grepl("10 units", output)))
  expect_true(any(grepl("12 relations", output)))
  expect_true(any(grepl("Valid stratigraphic graph", output)))
})

test_that("print.stratigraph() handles empty graphs", {
  empty_graph <- stratigraph(data.frame(label = character(0), above = list()),
                             "label", "above")

  output <- cli::ansi_strip(capture.output(print(empty_graph)))
  expect_true(length(output) > 0)
  expect_true(any(grepl("0 units", output)))
  expect_true(any(grepl("0 relations", output)))
})

test_that("print.stratigraph() handles invalid graphs", {
  data("harris12")
  harris12$above[1] <- list(c("natural"))
  suppressWarnings(invalid_graph <- stratigraph(harris12, "context", "above"))

  expect_no_error(capture.output(print(invalid_graph)))
})

test_that("strg_box_render() produces correct structure for harris12", {
  data("harris12")
  h12_graph <- stratigraph(harris12, "context", "above")

  result <- stratigraphr:::strg_box_render(h12_graph)
  tree <- result$lines

  # Check that the output contains the expected nodes
  expect_true(any(grepl("1", tree)))
  expect_true(any(grepl("2", tree)))
  expect_true(any(grepl("natural", tree)))

  # Check that the output contains box-drawing characters
  expect_true(any(grepl("┌", tree)))
  expect_true(any(grepl("┐", tree)))
  expect_true(any(grepl("└", tree)))
  expect_true(any(grepl("┘", tree)))
  expect_true(any(grepl("│", tree)))
})

test_that("strg_box_render() produces exact output for harris12", {
  data("harris12")
  h12_graph <- stratigraph(harris12, "context", "above")

  result <- stratigraphr:::strg_box_render(h12_graph)
  tree <- result$lines

  # Expected output structure
  # Row 1: "1" centered
  # Row 2: "┌───────┼───────┐" (split from 1)
  # Row 3: "2       3       4" (children of 1)
  # Row 4: "└───────┼───────┘" (merge into 5)
  # Row 5: "5" centered
  # Row 6: "│" (pass-through)
  # Row 7: "6" centered
  # Row 8: "┌───┴───┐" (split from 6)
  # Row 9: "7       8" (children of 6)
  # Row 10: "└───┬───┘" (merge into 9)
  # Row 11: "9" centered
  # Row 12: "│" (pass-through)
  # Row 13: "natural" centered

  # Check specific rows
  # Split from 1
  expect_true(any(grepl(
    "┌───────────────┼───────────────┐",
    tree
  )))
  # Merge into 5
  expect_true(any(grepl(
    "└───────────────┼───────────────┘",
    tree
  )))
  expect_true(any(grepl("┌───────┴───────┐", tree)))          # Split from 6
  expect_true(any(grepl("└───────┬───────┘", tree)))          # Merge into 9

  # Check that 2, 3, 4 are on the same row
  row_with_2 <- which(grepl("2", tree))
  row_with_3 <- which(grepl("3", tree))
  row_with_4 <- which(grepl("4", tree))
  expect_equal(length(unique(c(row_with_2, row_with_3, row_with_4))), 1)

  # Check that 7, 8 are on the same row
  row_with_7 <- which(grepl("7", tree))
  row_with_8 <- which(grepl("8", tree))
  expect_equal(length(unique(c(row_with_7, row_with_8))), 1)
})

test_that("strg_box_render() handles long edges with dummy nodes", {
  data("shub1")
  shub1_graph <- suppressWarnings(stratigraph(shub1, "context", "above"))

  result <- stratigraphr:::strg_box_render(shub1_graph, n = 100)
  tree <- result$lines

  # Check that all nodes are present
  for (i in 1:30) {
    expect_true(any(grepl(paste0("\\b", i, "\\b"), tree)),
                info = paste("Node", i, "not found"))
  }

  # Check that node 30 is connected (has edges above it)
  # Node 30 should have a "│" or "└" or "┘" or "┴" or "┬"
  # above it on the previous line
  row_with_30 <- which(grepl("30", tree))
  expect_true(length(row_with_30) > 0)

  # The line before node 30 should have some edge character
  if (row_with_30 > 1) {
    prev_line <- tree[row_with_30 - 1]
    # Should have some box-drawing character indicating connection
    expect_true(grepl("[│┌┐└┘┼┬┴├┤]", prev_line),
                info = "Node 30 should be connected to the graph")
  }
})

test_that("strg_box_render() truncates labels wider than max_label_width", {
  # Create a graph with long labels
  nodes <- data.frame(
    context = c("verylonglabel1", "verylonglabel2", "verylonglabel3"),
    above = I(list(NA_character_, "verylonglabel1", "verylonglabel2")),
    stringsAsFactors = FALSE
  )

  g <- stratigraph(nodes, "context", "above")

  # Test with default max_label_width (8)
  result_default <- stratigraphr:::strg_box_render(g)
  tree_default <- result_default$lines
  expect_true(any(grepl("verylong", tree_default)))
  expect_false(any(grepl("verylonglabel", tree_default)))

  # Test with custom max_label_width (5)
  result_custom <- stratigraphr:::strg_box_render(g, max_label_width = 5)
  tree_custom <- result_custom$lines
  expect_true(any(grepl("veryl", tree_custom)))
  expect_false(any(grepl("verylo", tree_custom)))

  # Verify column width adapts to truncated width
  # With max_label_width = 5, column width should be 6 (5 + 1 gap)
  line_lengths <- nchar(tree_custom)
  expect_true(max(line_lengths) <= 20)  # 3 nodes * 6 chars + some spacing
})

test_that("strg_box_render() centers labels on edge connections", {
  # Create a simple graph with odd-width labels
  nodes <- data.frame(
    context = c("abcde", "fghij", "klmno"),
    above = I(list(NA_character_, "abcde", "fghij")),
    stringsAsFactors = FALSE
  )

  g <- stratigraph(nodes, "context", "above")
  result <- stratigraphr:::strg_box_render(g)
  tree <- result$lines

  # For odd-width labels (5 chars), the vertical edge should be centered
  # Find the line with the label and the line below it with the edge
  label_line <- which(grepl("abcde", tree))[1]
  edge_line <- label_line + 1

  # The edge "│" should be at the center of the label
  label_pos <- as.integer(regexpr("abcde", tree[label_line]))
  edge_pos <- as.integer(regexpr("│", tree[edge_line]))

  # For a 5-char label starting at position label_pos,
  # center is at label_pos + 2
  expect_equal(edge_pos, label_pos + 2)
})

test_that("print.stratigraph() truncates by levels and shows footer", {
  data("harris12")
  h12_graph <- stratigraph(harris12, "context", "above")

  # Test truncation with n = 3 (show first 3 levels: 1, 2-3-4, 5)
  output <- cli::ansi_strip(capture.output(print(h12_graph, n = 3)))

  # Should show nodes from first 3 levels (1, 2, 3, 4, 5)
  expect_true(any(grepl("1", output)))
  expect_true(any(grepl("5", output)))

  # Should NOT show nodes from later levels (6, 7, 8, 9, natural)
  expect_false(any(grepl("\\b6\\b", output)))
  expect_false(any(grepl("natural", output)))

  # Should show footer
  expect_true(any(grepl("more units on.*more layers", output)))
  expect_true(any(grepl("Use.*print.*n.*to see more", output)))

  # Test with n = NULL (show all)
  output_all <- cli::ansi_strip(capture.output(print(h12_graph, n = NULL)))
  expect_true(any(grepl("natural", output_all)))
  expect_false(any(grepl("more units", output_all)))

  # Test with n >= total levels (show all)
  output_big <- cli::ansi_strip(capture.output(print(h12_graph, n = 100)))
  expect_true(any(grepl("natural", output_big)))
  expect_false(any(grepl("more units", output_big)))
})
