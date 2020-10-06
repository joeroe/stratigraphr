test_that("strat_is_valid() returns TRUE for the harris12 dataset", {
  data("harris12")
  h12_graph <- stratigraph(harris12, "context", "above")
  expect_warning(strat_is_valid(h12_graph), NA)
  expect_true(strat_is_valid(h12_graph))
})

test_that("strat_is_valid() detects cycles", {
  data("harris12")
  harris12$above[1] <- list(c("natural"))
  h12_graph <- stratigraph(harris12, "context", "above")
  expect_warning(strat_is_valid(h12_graph), "cycle")
  expect_false(strat_is_valid(h12_graph))
})
