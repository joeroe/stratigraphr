test_that("stratigraphs constructed from above and below are isomorphic", {
  data("harris12")
  h12_above <- stratigraph(harris12, "context", "above", "above")
  h12_below <- stratigraph(harris12, "context", "below", "below")

  expect_true(
    tidygraph::with_graph(h12_above, tidygraph::graph_is_isomorphic_to(h12_below))
  )
})

test_that("strat_is_valid() returns TRUE for the harris12 dataset", {
  data("harris12")
  h12_graph <- stratigraph(harris12, "context", "above")
  expect_warning(strat_is_valid(h12_graph), NA)
  expect_true(strat_is_valid(h12_graph))
})

test_that("strat_is_valid() detects cycles", {
  data("harris12")
  harris12$above[1] <- list(c("natural"))
  expect_warning(h12_graph <- stratigraph(harris12, "context", "above"))
  expect_warning(strat_is_valid(h12_graph), "cycle")
  expect_false(strat_is_valid(h12_graph, warn = FALSE))
})
