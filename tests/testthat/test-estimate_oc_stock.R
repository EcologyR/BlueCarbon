

# Test 1: Valid input
test_that("Valid input returns expected result", {
  # Create a sample data frame
  df <- data.frame(
    core = c("Core1", "Core1", "Core1", "Core1", "Core2", "Core2", "Core2", "Core2"),
    mind = c(1, 20, 45, 102, 0, 15, 20, 53),
    maxd = c(5, 26, 47, 108, 7, 20, 23, 56),
    dbd = c(1.2, 1.3, 1.4, 1.5, 1.2, 1.3, 1.4, 1.5),
    oc = c(2, 4, 6, 8, 2, 4, 6, 8)
  )

  # Call the estimate_oc_stock function
  result <- estimate_oc_stock(df = df, oc="oc", depth = 75)

  # Define the expected result
  expected <- data.frame(
    core = c("Core1", "Core2"),
    stockwc = c(8.792, 4.404),
    maxd = c(108, 56),
    stock = c(4.832000, 5.9597288),
    stock_se = c(NA, 0.42672608)
  )

  # Compare the result with the expected value
  expect_equal(result, expected)
})

# Test 2: Non-numeric 'depth'
test_that("Non-numeric 'depth' throws an error", {
  # Create a sample data frame
  df <- data.frame(
    core = c("Core1"),
    mind = c(1),
    maxd = c(5),
    dbd = c(1.2),
    eoc = c(2)
  )

  # Call the estimate_oc_stock function with non-numeric 'depth'
  expect_error(estimate_oc_stock(df = df, depth = "10"), "'depth' must be class numeric")
})

# Test 3: Non-numeric 'mind' data
test_that("Non-numeric 'mind' data throws an error", {
  # Create a sample data frame with non-numeric 'mind' column
  df <- data.frame(
    core = c("Core1"),
    mind = c("1"),  # 'mind' column with non-numeric value
    maxd = c(5),
    dbd = c(1.2),
    oc = c(2)
  )

  # Call the estimate_oc_stock function and expect an error to be thrown
  expect_error(estimate_oc_stock(df = df, oc="oc", depth = 10), "'mind' must be class numeric")
})

# Test 4: Non-numeric 'maxd' data
test_that("Non-numeric 'maxd' data throws an error", {
  # Create a sample data frame with non-numeric 'maxd' column
  df <- data.frame(
    core = c("Core1"),
    mind = c(1),
    maxd = c("5"),  # 'maxd' column with non-numeric value
    dbd = c(1.2),
    oc = c(2)
  )

  # Call the estimate_oc_stock function and expect an error to be thrown
  expect_error(estimate_oc_stock(df = df,oc="oc", depth = 10), "'maxd' must be class numeric")
})

# Test 5: Non-numeric 'dbd' data
test_that("Non-numeric 'dbd' data throws an error", {
  # Create a sample data frame with non-numeric 'dbd' column
  df <- data.frame(
    core = c("Core1"),
    mind = c(1),
    maxd = c(5),
    dbd = c("1.2"),  # 'dbd' column with non-numeric value
    oc = c(2)
  )

  # Call the estimate_oc_stock function and expect an error to be thrown
  expect_error(estimate_oc_stock(df = df, oc= "oc", depth = 10), "'dbd' must be class numeric")
})

# Test 6: Non-numeric 'oc' data
test_that("Non-numeric 'oc' data throws an error", {
  # Create a sample data frame with non-numeric 'oc' column
  df <- data.frame(
    core = c("Core1"),
    mind = c(1),
    maxd = c(5),
    dbd = c(1.2),
    oc = c("2")  # 'oc' column with non-numeric value
  )

  # Call the estimate_oc_stock function and expect an error to be thrown
  expect_error(estimate_oc_stock(df = df,oc="oc", depth = 10), "'oc' must be class numeric")
})

# Add more test cases if needed

