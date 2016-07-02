context("Beta")

test_that("Beta mean is correct", {
  expect_equal(mean(beta(0, 1)), 0)
  expect_equal(mean(beta(1, 0)), 1)
  expect_equal(mean(beta(5, 5)), .5)
})
