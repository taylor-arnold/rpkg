context("stat_maptiles")

test_that("Create plot with statistic", {

  p <- ggplot2::ggplot(french_city, ggplot2::aes(lng, lat)) +
      stat_maptiles(
        cache_dir = system.file("extdata", package="ggmaptile")
      )

  expect_equal(class(p), c("gg", "ggplot"))

})
