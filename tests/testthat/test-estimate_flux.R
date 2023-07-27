

# Test 1: Valid input
test_that("Valid input returns expected result", {
  # Create a sample data frame
  df <- data.frame(
    core = c("Core1", "Core1", "Core1", "Core1", "Core2", "Core2", "Core2", "Core2"),
    mind = c(1, 20, 45, 102, 0, 15, 20, 53),
    maxd = c(5, 26, 47, 108, 7, 20, 23, 56),
    dbd = c(1.2, 1.3, 1.4, 1.5, 1.2, 1.3, 1.4, 1.5),
    oc = c(2, 4, 6, 8, 2, 4, 6, 8),
    age = c(10, 20, 30, 40, 10, 20, 50, 120)
  )

  # Call the estimate_flux function
  result <- estimate_flux(df = df, oc="oc", timeframe = 75)

  # Define the expected result
  expected <- data.frame(
    core = c("Core1", "Core2"),
    fluxwc = c(0.2198, 0.0367),
    maxage = c(40, 120),
    flux = c(NA, 0.0402057142857143)
  )

  # Compare the result with the expected value
  expect_equal(result, expected)
})

# Test 2: Non-numeric 'depth'
test_that("Non-numeric 'timeframe' throws an error", {
  # Create a sample data frame
  df <- data.frame(
    core = c("Core1"),
    mind = c(1),
    maxd = c(5),
    dbd = c(1.2),
    eoc = c(2),
    age= c(100)
  )

  # Call the estimate_flux function with non-numeric 'depth'
  expect_error(estimate_flux(df = df, oc="oc", timeframe = "10"), "The time frame must be class numeric")
})

# Test 3: Non-numeric 'mind' data
test_that("Non-numeric 'mind' data throws an error", {
  # Create a sample data frame with non-numeric 'mind' column
  df <- data.frame(
    core = c("Core1"),
    mind = c("1"),  # 'mind' column with non-numeric value
    maxd = c(5),
    dbd = c(1.2),
    oc = c(2),
    age= c(100)
  )

  # Call the estimate_oc_stock function and expect an error to be thrown
  expect_error(estimate_flux(df = df, oc="oc", timeframe = 10), "Minimum depth data is not class numeric, please check")
})

# Test 4: Non-numeric 'maxd' data
test_that("Non-numeric 'maxd' data throws an error", {
  # Create a sample data frame with non-numeric 'maxd' column
  df <- data.frame(
    core = c("Core1"),
    mind = c(1),
    maxd = c("5"),  # 'maxd' column with non-numeric value
    dbd = c(1.2),
    oc = c(2),
    age= c(100)
  )

  # Call the estimate_oc_stock function and expect an error to be thrown
  expect_error(estimate_flux(df = df, oc="oc", timeframe = 10), "Maximum depth data is not class numeric, please check")
})



