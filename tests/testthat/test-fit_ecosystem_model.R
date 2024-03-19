

test_that("fit_ecosystem_model function works as expected", {
  # Create a sample data frame for testing
  df <- data.frame(
    oc_r = c(1, NA, 3, 4, 5),
    om_r = c(0.1, 0.2, 0.3, 0.4, 0.5)
  )

  # Test if the function runs without error
  expect_no_error(fit_ecosystem_model(df))

  # Test if the output is a linear model object
  expect_s3_class(fit_ecosystem_model(df), "lm")

  # Test if the linear model is fitted correctly
  expected_coefficients <- c(2, 1)
  actual_coefficients <- coef(fit_ecosystem_model(df))
  expect_equal(actual_coefficients[[1]], expected_coefficients[[1]], tolerance = 1)
  expect_equal(actual_coefficients[[2]], expected_coefficients[[2]], tolerance = 1)

})
