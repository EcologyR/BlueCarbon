library(testthat)

# Test 1: Valid input
test_that("Valid input returns expected result", {
  # Create a sample data frame
  df <- data.frame(
    core = c("A", "A", "B", "B"),
    mind = c(1, 3, 6, 9),
    maxd = c(2, 4, 7, 10)
  )

  # Call the estimate_h function
  result <- estimate_h(df = df)

  # Define the expected result
  expected <- data.frame(
    core_r = c("A","A", "B","B"),
    mind_r = c(0, 2.5, 5, 8),
    maxd_r = c(2.5,5, 8, 10),
    h = c(2.5, 2.5, 3, 2)
  )

  # Compare the result with the expected value
  expect_equal(result, expected)
})

# Test 2: Non-numeric 'mind' data
test_that("Non-numeric 'mind' data throws an error", {
  # Create a sample data frame with non-numeric 'mind' column
  df <- data.frame(
    core = c("A", "B"),
    mind = c("1", "2"),  # 'mind' column with non-numeric values
    maxd = c(5, 6)
  )

  # Call the estimate_h function and expect an error to be thrown
  expect_error(estimate_h(df = df), "'mind' data must be class numeric")
})

# Test 3: NAs in 'maxd' column
test_that("NAs in 'maxd' column throw an error", {
  # Create a sample data frame with NAs in 'maxd' column
  df <- data.frame(
    core = c("A", "B"),
    mind = c(1, 2),
    maxd = c(5, NA)  # 'maxd' column with NA
  )

  # Call the estimate_h function and expect an error to be thrown
  expect_error(estimate_h(df = df), "Samples maximun depth column has NAs, please check")
})



