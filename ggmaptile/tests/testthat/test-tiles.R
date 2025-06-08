context("tiles")

test_that("Tile helper functions return correct results", {

  expect_equal(lon_to_xtile(20.5, 10), 570)
  expect_equal(lat_to_ytile(20.5, 10), 452)
  expect_equal(round(xtile_to_lon(570, 10), 2), 20.39)
  expect_equal(round(ytile_to_lon(452, 10), 2), 20.63)

})

test_that("Download file from URL", {

  testthat::skip_on_cran()

  tf <- file.path(tempfile(), "newfolder", "2001_shrek.jpg")
  url <- paste(c(
    "https://raw.githubusercontent.com/statsmaths/",
    "ggimg/master/inst/extdata/2001_shrek.jpg"
  ), collapse = "")

  download_paths(url, tf)
  expect_true(file.exists(tf))

})

test_that("Check that mt_map_extent_data returns correct result", {

  testthat::skip_on_cran()

  # grab default bounding box
  df <- mt_map_extent_data(-5, 10, 30, 35)
  expect_equal(ncol(df), 6)
  expect_equal(nrow(df), 6)

  # extend in y-direction to have a square box
  df <- mt_map_extent_data(-5, 10, 30, 35, aspect_ratio = 1)
  expect_equal(ncol(df), 6)
  expect_equal(nrow(df), 9)

  # extend in x-direction to have a region three times as wide as it is long
  df <- mt_map_extent_data(-5, 10, 30, 35, aspect_ratio = 3)
  expect_equal(ncol(df), 6)
  expect_equal(nrow(df), 8)

})
