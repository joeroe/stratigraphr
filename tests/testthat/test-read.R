test_that("read_lst() reads correct number of strata and attributes", {
  bonn <- read_lst(system.file("extdata", "bonn.lst", package = "stratigraphr"))
  stratify <- read_lst(system.file("extdata", "stratify.lst", package = "stratigraphr"))

  expect_equal(nrow(bonn), 19)
  expect_equal(nrow(stratify), 17)
  expect_equal(ncol(bonn), 5)
  expect_equal(ncol(stratify), 4)
})
