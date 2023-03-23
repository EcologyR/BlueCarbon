test_that("multiplication works", {
  expect_equal(multiply(2, 2), 4)
  expect_error(multiply(2,"a"))
})
