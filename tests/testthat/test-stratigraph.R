test_that("stratigraphs constructed from above and below are isomorphic", {
  data("harris12")
  h12_above <- stratigraph(harris12, "context", "above", "above")
  h12_below <- stratigraph(harris12, "context", "below", "below")

  expect_true(
    tidygraph::with_graph(h12_above, tidygraph::graph_is_isomorphic_to(h12_below))
  )
})

test_that("strg_is_valid() returns TRUE for the harris12 dataset", {
  data("harris12")
  h12_graph <- stratigraph(harris12, "context", "above")
  expect_warning(strg_is_valid(h12_graph), NA)
  expect_true(strg_is_valid(h12_graph))
})

test_that("strg_is_valid() detects cycles", {
  data("harris12")
  harris12$above[1] <- list(c("natural"))
  expect_warning(h12_graph <- stratigraph(harris12, "context", "above"))
  expect_warning(strg_is_valid(h12_graph), "cycle")
  expect_false(strg_is_valid(h12_graph, warn = FALSE))
})

test_that("print.stratigraph() produces output", {
  data("harris12")
  h12_graph <- stratigraph(harris12, "context", "above")
  
  output <- capture.output(print(h12_graph))
  expect_true(length(output) > 0)
  expect_true(any(grepl("A stratigraph:", output)))
  expect_true(any(grepl("10 units", output)))
  expect_true(any(grepl("12 relations", output)))
  expect_true(any(grepl("Valid: TRUE", output)))
})

test_that("print.stratigraph() handles empty graphs", {
  empty_graph <- stratigraph(data.frame(label = character(0), above = list()), 
                             "label", "above")
  
  output <- capture.output(print(empty_graph))
  expect_true(length(output) > 0)
  expect_true(any(grepl("0 units", output)))
  expect_true(any(grepl("0 relations", output)))
})

test_that("strg_box_render() produces correct structure for harris12", {
  data("harris12")
  h12_graph <- stratigraph(harris12, "context", "above")
  
  tree <- stratigraphr:::strg_box_render(h12_graph)
  
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
  
  tree <- stratigraphr:::strg_box_render(h12_graph)
  
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
  expect_true(any(grepl("┌───────┼───────┐", tree)))  # Split from 1
  expect_true(any(grepl("└───────┼───────┘", tree)))  # Merge into 5
  expect_true(any(grepl("┌───┴───┐", tree)))          # Split from 6
  expect_true(any(grepl("└───┬───┘", tree)))          # Merge into 9
  
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
