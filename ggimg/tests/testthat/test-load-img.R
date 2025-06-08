context("load-img")

test_that("File extensions resolve correctly", {

  expect_equal(file_ext("somefile.png"), "png")
  expect_equal(file_ext("some.file.JPEG"), "JPEG")
  expect_equal(file_ext("somelonger/file.R"), "R")

})

test_that("Loading PNG from disk", {

  # PNG is a lossy compression algorithm, so need to compare to a
  # the output of load_img with png::readPNG, not fake_img directly
  fake_img <- array(runif(50 * 50 * 3), dim = c(50, 50, 3))
  filepath_png <- tempfile(fileext = ".png")
  png::writePNG(fake_img, filepath_png)
  fake_img_png <- png::readPNG(filepath_png)
  expect_equal(load_img(filepath_png), fake_img_png)

})

test_that("Loading JPEG from disk", {

  # JPEG is a lossy compression algorithm, so need to compare to a
  # the output of load_img with png::readPNG, not fake_img directly
  fake_img <- array(runif(50 * 50 * 3), dim = c(50, 50, 3))
  filepath_jpg <- tempfile(fileext = ".jpg")
  jpeg::writeJPEG(fake_img, filepath_jpg)
  fake_img_jpg <- jpeg::readJPEG(filepath_jpg)
  expect_equal(load_img(filepath_jpg), fake_img_jpg)

})

test_that("Loading JPEG from web", {

  testthat::skip_on_cran()

  # Test that the poster thumbnail in the package is the same as the one in the
  # GitHub repository
  local_path <- file.path(
    system.file("extdata", package="ggimg"),
    "2019_the_lion_king.jpg"
  )
  github_path <- paste(c(
    "https://github.com/statsmaths/ggimg/raw/master/inst/extdata/",
    "2019_the_lion_king.jpg"
  ), collapse = '')

  expect_equal(load_img(local_path), load_img(github_path))

})

test_that("Return raw image data when given list", {

  fake_img_list <- list(array(runif(50 * 50 * 3), dim = c(50, 50, 3)))
  expect_equal(load_img(fake_img_list), fake_img_list[[1]])

})

test_that("Error when given filenames with other extensions", {

  expect_error(load_img("file.bmp"), "Cannot open file file.bmp")

})

test_that("Error when given date data type", {

  expect_error(load_img(as.Date("2020-02-02")), "Cannot read image file.")

})
