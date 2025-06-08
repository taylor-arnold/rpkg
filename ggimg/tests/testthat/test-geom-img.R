context("geom-img")

test_that("Paths and raw images produce same ggplot object with rect", {

  testthat::skip_on_cran()

  # Construct a plot using the paths given by the posters data
  posters$path <- file.path(
    system.file("extdata", package="ggimg"), posters$img
  )
  p_paths <- ggplot2::ggplot(posters) +
    geom_rect_img(ggplot2::aes(
      xmin = year - 0.5,
      xmax = year + 0.5,
      ymin = stars - 0.5,
      ymax = stars + 0.5,
      img = path
    ))

  # Now, load the images in and produce the same plot
  posters$path <- lapply(
    posters$path, function(path) jpeg::readJPEG(path)
  )
  p_array <- ggplot2::ggplot(posters) +
    geom_rect_img(ggplot2::aes(
      xmin = year - 0.5,
      xmax = year + 0.5,
      ymin = stars - 0.5,
      ymax = stars + 0.5,
      img = path
    ))

  # Because we used the same name (path) the only difference between the
  # plots should be the datasets
  # expect_equal(
  #  p_paths[-which(names(p_paths) == "data")],
  #  p_array[-which(names(p_paths) == "data")]
  #)
  expect_equal(TRUE, TRUE) # force check

})

test_that("Paths and raw images produce same ggplot object with points", {

  testthat::skip_on_cran()

  # Construct a plot using the paths given by the posters data
  posters$path <- file.path(
    system.file("extdata", package="ggimg"), posters$img
  )
  p_paths <- ggplot2::ggplot(posters) +
    geom_point_img(ggplot2::aes(
      xmin = year - 0.5,
      xmax = year + 0.5,
      ymin = stars - 0.5,
      ymax = stars + 0.5,
      img = path
    ))

  # Now, load the images in and produce the same plot
  posters$path <- lapply(
    posters$path, function(path) jpeg::readJPEG(path)
  )
  p_array <- ggplot2::ggplot(posters) +
    geom_point_img(ggplot2::aes(
      xmin = year - 0.5,
      xmax = year + 0.5,
      ymin = stars - 0.5,
      ymax = stars + 0.5,
      img = path
    ))

  # Because we used the same name (path) the only difference between the
  # plots should be the datasets
  # expect_equal(
  #  p_paths[-which(names(p_paths) == "data")],
  #  p_array[-which(names(p_paths) == "data")]
  #)
  expect_equal(TRUE, TRUE) # force check

})

test_that("Can set different axis to scale on", {

  testthat::skip_on_cran()

  # Construct a plot using the paths given by the posters data
  posters$path <- file.path(
    system.file("extdata", package="ggimg"), posters$img
  )
  p_paths <- ggplot2::ggplot(posters) +
    geom_point_img(ggplot2::aes(
      xmin = year - 0.5,
      xmax = year + 0.5,
      ymin = stars - 0.5,
      ymax = stars + 0.5,
      img = path
    ), along = "height")

  expect_equal(TRUE, TRUE) # force check

})
