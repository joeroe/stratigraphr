# stratigraph() -----------------------------------------------------------

test_that("stratigraph() returns correct class", {
  df <- data.frame(
    id = c("a", "b", "c"),
    above = I(list("b", "c", NA))
  )

  g <- stratigraph(df, "id", "above")

  expect_s3_class(g, "stratigraph")
  expect_s3_class(g, "tbl_graph")
})

test_that("stratigraph() handles NA relations", {
  df <- data.frame(
    id = c("a", "b"),
    above = I(list(NA, NA))
  )

  g <- stratigraph(df, "id", "above")

  expect_s3_class(g, "stratigraph")
  expect_equal(igraph::gorder(g), 2)
  expect_equal(igraph::gsize(g), 0)
})

test_that("stratigraph() warns on invalid graphs", {
  df <- data.frame(
    id = c("a", "b"),
    above = I(list("b", "a"))
  )

  expect_warning(stratigraph(df, "id", "above"), class = "invalid_stratigraph")
})

test_that("stratigraph() handles long-format input", {
  # Long format (one relation per row)
  df_long <- data.frame(
    id = c("a", "b", "c", "c"),
    above = c("b", "c", "a", "b")
  )

  # Equivalent list-column format
  df_list <- data.frame(
    id = c("a", "b", "c"),
    above = I(list("b", "c", c("a", "b")))
  )

  g_long <- stratigraph(df_long, "id", "above")
  g_list <- stratigraph(df_list, "id", "above")

  expect_equal(igraph::gorder(g_long), igraph::gorder(g_list))
  expect_equal(igraph::gsize(g_long), igraph::gsize(g_list))
  expect_equal(
    igraph::as_edgelist(g_long, names = TRUE),
    igraph::as_edgelist(g_list, names = TRUE)
  )
})

test_that("stratigraph() handles long-format input with NA relations", {
  df <- data.frame(
    id = c("a", "b", "c"),
    above = c(NA, NA, NA)
  )

  g <- stratigraph(df, "id", "above")

  expect_s3_class(g, "stratigraph")
  expect_equal(igraph::gorder(g), 3)
  expect_equal(igraph::gsize(g), 0)
})

test_that("stratigraph() handles long-format input with multiple relations per unit", {
  df <- data.frame(
    id = c("a", "a", "b", "b", "c"),
    above = c("b", "c", "c", "c", NA)
  )

  g <- stratigraph(df, "id", "above")

  expect_s3_class(g, "stratigraph")
  expect_equal(igraph::gorder(g), 3)  # a, b, c
  expect_equal(igraph::gsize(g), 4)   # a->b, a->c, b->c, b->c (duplicate edge)
})

# print.stratigraph() -----------------------------------------------------

test_that("print.stratigraph() produces output", {
  df <- data.frame(
    id = c("a", "b", "c"),
    above = I(list("b", "c", NA))
  )
  g <- stratigraph(df, "id", "above")

  output <- cli::ansi_strip(capture.output(print(g)))
  expect_true(length(output) > 0)
  expect_true(any(grepl("A stratigraph:", output)))
  expect_true(any(grepl("3 units", output)))
  expect_true(any(grepl("2 relations", output)))
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
  df <- data.frame(
    id = c("a", "b"),
    above = I(list("b", "a"))
  )
  invalid_graph <- suppressWarnings(stratigraph(df, "id", "above"))

  expect_no_error(capture.output(print(invalid_graph)))
})

test_that("print.stratigraph() truncates by levels and shows footer", {
  df <- data.frame(
    id = c("a", "b", "c", "d", "e"),
    above = I(list(c("b", "c"), c("d"), c("d"), "e", NA))
  )
  g <- stratigraph(df, "id", "above")

  output <- cli::ansi_strip(capture.output(print(g, n = 2)))

  expect_true(any(grepl("e", output)))
  expect_true(any(grepl("d", output)))

  expect_false(any(grepl("\\bb\\b", output)))
  expect_false(any(grepl("\\bc\\b", output)))
  expect_false(any(grepl("\\ba\\b", output)))

  expect_true(any(grepl("more units on.*more layers", output)))
  expect_true(any(grepl("Use.*print.*n.*to see more", output)))

  output_all <- cli::ansi_strip(capture.output(print(g, n = NULL)))
  expect_true(any(grepl("\\ba\\b", output_all)))
  expect_false(any(grepl("more units", output_all)))
})

# strg_box_render() -------------------------------------------------------

test_that("strg_box_render() produces correct structure", {
  df <- data.frame(
    id = c("a", "b", "c"),
    above = I(list("b", "c", NA))
  )
  g <- stratigraph(df, "id", "above")

  result <- stratigraphr:::strg_box_render(g)
  tree <- result$lines

  expect_true(any(grepl("a", tree)))
  expect_true(any(grepl("b", tree)))
  expect_true(any(grepl("c", tree)))

  expect_true(any(grepl("│", tree)))
})

test_that("strg_box_render() truncates labels wider than max_label_width", {
  df <- data.frame(
    id = c("verylonglabel1", "verylonglabel2", "verylonglabel3"),
    above = I(list(c("verylonglabel2", "verylonglabel3"), NA, NA))
  )
  g <- stratigraph(df, "id", "above")

  result_default <- stratigraphr:::strg_box_render(g)
  tree_default <- result_default$lines
  expect_true(any(grepl("verylong", tree_default)))
  expect_false(any(grepl("verylonglabel", tree_default)))

  result_custom <- stratigraphr:::strg_box_render(g, max_label_width = 5)
  tree_custom <- result_custom$lines
  expect_true(any(grepl("veryl", tree_custom)))
  expect_false(any(grepl("verylo", tree_custom)))
})
