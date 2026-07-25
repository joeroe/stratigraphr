# Test strg_prune() function

test_that("strg_prune() removes redundant relations", {
  # Create a graph with redundant relations
  bushy <- suppressWarnings(stratigraph(
    tibble::tibble(
      id = letters[1:5],
      above = list(NA, "a", "a", c("a", "b", "c"), c("a", "c", "d"))
    ),
    "id", "above"
  ))

  # Should have redundant relations
  expect_false(strg_is_valid(bushy))

  # Prune should reduce edge count
  pruned <- strg_prune(bushy)
  expect_lt(igraph::gsize(pruned), igraph::gsize(bushy))

  # Pruned graph should be valid
  expect_true(strg_is_valid(pruned))
})

test_that("strg_prune() preserves stratigraph class", {
  # Regression test for class preservation
  bushy <- suppressWarnings(stratigraph(
    tibble::tibble(
      id = letters[1:5],
      above = list(NA, "a", "a", c("a", "b", "c"), c("a", "c", "d"))
    ),
    "id", "above"
  ))

  pruned <- strg_prune(bushy)

  # Should be a stratigraph
  expect_s3_class(pruned, "stratigraph")
  expect_s3_class(pruned, "tbl_graph")
})

test_that("strg_prune() preserves node attributes", {
  # Create graph with node attributes
  graph <- suppressWarnings(stratigraph(
    tibble::tibble(
      id = letters[1:5],
      custom_attr = 1:5,
      above = list(NA, "a", "a", c("a", "b", "c"), c("a", "c", "d"))
    ),
    "id", "above"
  ))

  pruned <- strg_prune(graph)

  # Node attributes should be preserved
  pruned_nodes <- tidygraph::as_tibble(pruned, active = "nodes")
  expect_equal(pruned_nodes$custom_attr, 1:5)
  expect_equal(pruned_nodes$id, letters[1:5])
})

test_that("strg_prune() on valid graph returns same edge count", {
  # Create a graph with no redundant relations
  data("harris12")
  graph <- stratigraph(harris12, "context", "above")

  # Should be valid (no redundant relations)
  expect_true(strg_is_valid(graph))

  # Pruning should not change edge count
  pruned <- strg_prune(graph)
  expect_equal(igraph::gsize(pruned), igraph::gsize(graph))
})
