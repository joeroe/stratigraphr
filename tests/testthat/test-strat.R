# strat_connect() ---------------------------------------------------------

test_that("strat_connect() returns correct edges for direction = 'above'", {
  units <- c("a", "b", "c")
  relations <- list("b", "c", NA)

  edges <- strat_connect(units, relations, "above")

  expect_s3_class(edges, "data.frame")
  expect_named(edges, c("to", "from"))
  expect_equal(edges$to, c("a", "b"))
  expect_equal(edges$from, c("b", "c"))
})

test_that("strat_connect() returns correct edges for direction = 'below'", {
  units <- c("a", "b", "c")
  relations <- list("b", "c", NA)

  edges <- strat_connect(units, relations, "below")

  expect_equal(edges$to, c("b", "c"))
  expect_equal(edges$from, c("a", "b"))
})

test_that("strat_connect() drops NA relations", {
  units <- c("a", "b")
  relations <- list(NA, NA)

  edges <- strat_connect(units, relations, "above")

  expect_equal(nrow(edges), 0)
})

test_that("strat_connect() expands list-valued relations", {
  units <- c("a", "b")
  relations <- list(c("b", "c"), NA)

  edges <- strat_connect(units, relations, "above")

  expect_equal(nrow(edges), 2)
  expect_equal(edges$to, c("a", "a"))
  expect_equal(edges$from, c("b", "c"))
})

# strat_is_mirror() -------------------------------------------------------

test_that("strat_is_mirror() returns TRUE for mirrored relations", {
  units <- c("a", "b", "c")
  above <- list("b", "c", NA)
  below <- list(NA, "a", "b")

  expect_true(strat_is_mirror(units, above, below))
})

test_that("strat_is_mirror() returns FALSE for non-mirrored relations", {
  units <- c("a", "b", "c")
  above <- list("b", "c", NA)
  below <- list(NA, NA, "b")

  expect_false(strat_is_mirror(units, above, below))
})

test_that("strat_is_mirror() handles NA values", {
  units <- c("a", "b")
  above <- list(NA, NA)
  below <- list(NA, NA)

  expect_true(strat_is_mirror(units, above, below))
})

test_that("strat_is_mirror() works with list-valued relations", {
  units <- c("a", "b", "c")
  above <- list(c("b", "c"), "c", NA)
  below <- list(NA, "a", c("a", "b"))

  expect_true(strat_is_mirror(units, above, below))
})
