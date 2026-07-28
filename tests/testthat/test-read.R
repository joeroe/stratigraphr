# lst_extract_attribute() -------------------------------------------------

test_that("lst_extract_attribute() parses name: value format", {
  result <- stratigraphr:::lst_extract_attribute("above: 1, 2, 3")
  expect_named(result, "above")
  expect_equal(result$above, "1,2,3")
})

test_that("lst_extract_attribute() normalizes attribute names", {
  result <- stratigraphr:::lst_extract_attribute("Contemporary With: 5")
  expect_named(result, "contemporary_with")
})

test_that("lst_extract_attribute() converts empty values to NA", {
  result <- stratigraphr:::lst_extract_attribute("below: ")
  expect_named(result, "below")
  expect_true(is.na(result$below))
})

test_that("lst_extract_attribute() removes whitespace from values", {
  result <- stratigraphr:::lst_extract_attribute("above: 1 , 2 , 3")
  expect_equal(result$above, "1,2,3")
})

# lst_extract_stratum() ---------------------------------------------------

test_that("lst_extract_stratum() parses stratum block", {
  stratum_lines <- c(
    "1",
    "            above: 2",
    "            below: 3"
  )
  result <- stratigraphr:::lst_extract_stratum(stratum_lines)
  expect_equal(result$name, "1")
  expect_equal(result$above, "2")
  expect_equal(result$below, "3")
})

test_that("lst_extract_stratum() handles missing attributes", {
  stratum_lines <- c(
    "A",
    "            above: B"
  )
  result <- stratigraphr:::lst_extract_stratum(stratum_lines)
  expect_equal(result$name, "A")
  expect_equal(result$above, "B")
  expect_null(result$below)
})

# lst_extract_header() ----------------------------------------------------

test_that("lst_extract_header() extracts dataset name", {
  lst_lines <- c(
    "                Stratigraphic Dataset bonntest",
    "",
    "  Name"
  )
  result <- stratigraphr:::lst_extract_header(lst_lines)
  expect_equal(result$dataset_name, "Stratigraphic Dataset bonntest")
})

# try_to_flatten() --------------------------------------------------------

test_that("try_to_flatten() flattens list when all elements have length 1", {
  x <- list("a", "b", "c")
  result <- stratigraphr:::try_to_flatten(x)
  expect_type(result, "character")
  expect_equal(result, c("a", "b", "c"))
})

test_that("try_to_flatten() preserves list when elements have varying lengths", {
  x <- list("a", c("b", "c"), "d")
  result <- stratigraphr:::try_to_flatten(x)
  expect_type(result, "list")
  expect_length(result, 3)
})

# read_lst() --------------------------------------------------------------

test_that("read_lst() reads LST file with split = TRUE (default)", {
  lst_content <- c(
    "Test Dataset",
    "",
    "  Name",
    "A",
    "            above: B, C",
    "            below: ",
    "B",
    "            above: C",
    "            below: A"
  )
  tmp <- tempfile(fileext = ".lst")
  writeLines(lst_content, tmp)
  on.exit(unlink(tmp))

  result <- read_lst(tmp)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_named(result, c("name", "above", "below"))
  expect_type(result$above, "list")
  expect_equal(result$above[[1]], c("B", "C"))
  expect_equal(result$above[[2]], "C")
  expect_true(is.na(result$below[[1]]))
  expect_equal(result$below[[2]], "A")
})

test_that("read_lst() reads LST file with split = FALSE", {
  lst_content <- c(
    "Test Dataset",
    "",
    "  Name",
    "A",
    "            above: B, C",
    "B",
    "            above: C"
  )
  tmp <- tempfile(fileext = ".lst")
  writeLines(lst_content, tmp)
  on.exit(unlink(tmp))

  result <- read_lst(tmp, split = FALSE)

  expect_type(result$above, "character")
  expect_equal(result$above[[1]], "B,C")
  expect_equal(result$above[[2]], "C")
})

test_that("read_lst() selectively splits specified columns", {
  lst_content <- c(
    "Test Dataset",
    "",
    "  Name",
    "A",
    "            above: B, C",
    "            below: D, E",
    "B",
    "            above: C",
    "            below: F"
  )
  tmp <- tempfile(fileext = ".lst")
  writeLines(lst_content, tmp)
  on.exit(unlink(tmp))

  result <- read_lst(tmp, split = "above")

  expect_type(result$above, "list")
  expect_type(result$below, "character")
  expect_equal(result$above[[1]], c("B", "C"))
  expect_equal(result$below[[1]], "D,E")
})

test_that("read_lst() uses custom separator", {
  lst_content <- c(
    "Test Dataset",
    "",
    "  Name",
    "A",
    "            above: B; C"
  )
  tmp <- tempfile(fileext = ".lst")
  writeLines(lst_content, tmp)
  on.exit(unlink(tmp))

  result <- read_lst(tmp, sep = ";")

  expect_equal(result$above[[1]], c("B", "C"))
})

test_that("read_lst() extracts dataset_name attribute", {
  lst_content <- c(
    "My Archaeological Site",
    "",
    "  Name",
    "A",
    "            above: B"
  )
  tmp <- tempfile(fileext = ".lst")
  writeLines(lst_content, tmp)
  on.exit(unlink(tmp))

  result <- read_lst(tmp)

  expect_equal(attr(result, "dataset_name"), "My Archaeological Site")
})
