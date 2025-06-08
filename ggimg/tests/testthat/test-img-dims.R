context("fix-img-dims")

test_that("Returns correct image from RGB or RBGA", {

  expected_img <- array(runif(50 * 100 * 4), dim = c(50, 100, 4))
  expected_img[,,4] <- 0.5

  expect_equal(
    fix_img_dims(expected_img[,,seq(1, 3)], alpha = 0.5),
    expected_img
  )
  expect_equal(
    fix_img_dims(expected_img[,,], alpha = 0.5),
    expected_img
  )
  expect_equal(
    fix_img_dims(expected_img[,,], alpha = -1),
    expected_img
  )

})

test_that("Returns correct image from matrix or 1 channel", {

  img_bw <- matrix(runif(50 * 100), nrow = 50, ncol = 100)
  expected_img <- array(runif(50 * 100 * 4), dim = c(50, 100, 4))
  expected_img[,,1] <- img_bw
  expected_img[,,2] <- img_bw
  expected_img[,,3] <- img_bw
  expected_img[,,4] <- 1

  expect_equal(
    fix_img_dims(img_bw, alpha = 1),
    expected_img
  )
  expect_equal(
    fix_img_dims(array(img_bw, dim = c(50, 100, 1)), alpha = 1),
    expected_img
  )

})

test_that("Returns correct image from 2 channel BA", {

  img_ba <- array(runif(50 * 100 * 2), dim = c(50, 100, 2))
  img_ba[,,2] <- 0.5
  expected_img <- array(runif(50 * 100 * 2), dim = c(50, 100, 4))
  expected_img[,,1] <- img_ba[,,1]
  expected_img[,,2] <- img_ba[,,1]
  expected_img[,,3] <- img_ba[,,1]
  expected_img[,,4] <- img_ba[,,2]

  expect_equal(
    fix_img_dims(img_ba, alpha = 0.5),
    expected_img
  )
  expect_equal(
    fix_img_dims(img_ba, alpha = -1),
    expected_img
  )

})


test_that("Error when given more than 4 channels", {

  fake_img <- array(runif(50 * 50 * 5), dim = c(50, 50, 5))

  expect_error(
    fix_img_dims(fake_img),
    "We do not know how to display an image with 5 channels"
  )

})
