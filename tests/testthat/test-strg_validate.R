# strg_is_valid() ---------------------------------------------------------

test_that("strg_is_valid() returns TRUE for valid DAG", {
  df <- data.frame(
    id = c("a", "b", "c"),
    above = I(list("b", "c", NA))
  )
  g <- stratigraph(df, "id", "above")

  expect_true(strg_is_valid(g))
})

test_that("strg_is_valid() returns FALSE for cyclic graph", {
  df <- data.frame(
    id = c("a", "b"),
    above = I(list("b", "a"))
  )
  g <- suppressWarnings(stratigraph(df, "id", "above"))

  expect_false(strg_is_valid(g))
})

test_that("strg_is_valid() returns FALSE for redundant relations", {
  df <- data.frame(
    id = c("a", "b", "c"),
    above = I(list(c("b", "c"), "c", NA))
  )
  g <- suppressWarnings(stratigraph(df, "id", "above"))

  expect_false(strg_is_valid(g))
})

test_that("strg_is_valid() returns TRUE for single-node graph", {
  df <- data.frame(
    id = "a",
    above = I(list(NA))
  )
  g <- stratigraph(df, "id", "above")

  expect_true(strg_is_valid(g))
})

# strg_validate() ---------------------------------------------------------

test_that("strg_validate() returns object when valid", {
  df <- data.frame(
    id = c("a", "b", "c"),
    above = I(list("b", "c", NA))
  )
  g <- stratigraph(df, "id", "above")

  expect_identical(strg_validate(g), g)
})

test_that("strg_validate() errors on cycles", {
  df <- data.frame(
    id = c("a", "b"),
    above = I(list("b", "a"))
  )
  g <- suppressWarnings(stratigraph(df, "id", "above"))

  expect_error(strg_validate(g), class = "invalid_stratigraph")
})

test_that("strg_validate() errors on redundant relations", {
  df <- data.frame(
    id = c("a", "b", "c"),
    above = I(list(c("b", "c"), "c", NA))
  )
  g <- suppressWarnings(stratigraph(df, "id", "above"))

  expect_error(strg_validate(g), class = "invalid_stratigraph")
})

test_that("strg_validate() warns instead of errors when warn = TRUE", {
  df <- data.frame(
    id = c("a", "b"),
    above = I(list("b", "a"))
  )
  g <- suppressWarnings(stratigraph(df, "id", "above"))

  expect_warning(strg_validate(g, warn = TRUE), class = "invalid_stratigraph")
  expect_silent(result <- suppressWarnings(strg_validate(g, warn = TRUE)))
  expect_identical(result, g)
})
