

# Test 1: Valid input
test_that("Valid input returns expected result", {
  # Create a sample data frame
  df <- data.frame(
    core = c("core1", "core1", "core2", "core2"),
    site = c("Site1", "Site1", "Site2", "Site2"),
    ecosystem = c("Salt Marsh", "Salt Marsh", "Seagrass", "Seagrass"),
    species = c("Sp1", "Sp1", "Sp2", "Sp2"),
    om = c(10, 20, 30, 40),
    oc = c(5, 10, 15, NA)
  )

  # Call the estimate_oc function
  result <- estimate_oc(df = df)[[1]]

  # Define the expected result
  expected <- data.frame(
    core = c("core1", "core1", "core2", "core2"),
    site = c("Site1", "Site1", "Site2", "Site2"),
    ecosystem = c("Salt Marsh", "Salt Marsh", "Seagrass", "Seagrass"),
    species = c("Sp1", "Sp1", "Sp2", "Sp2"),
    om = c(10, 20, 30, 40),
    oc = c(5, 10, 15, NA),
    eoc = c(5, 10, 15, 17),
    eoc_se = NA,
    origin = c("Measured","Measured","Measured","Fourqurean et al. 2012")
  )

  # Compare the result with the expected value
  expect_equal(result, expected, tolerance = 1)
})

# Test 2: Non-numeric 'om' data
test_that("Non-numeric 'om' data throws an error", {
  # Create a sample data frame with non-numeric 'om' column
  df <- data.frame(
    core = c("Core1", "Core2"),
    site = c("Site1", "Site2"),
    ecosystem = c("Eco1", "Eco2"),
    species = c("Sp1", "Sp2"),
    om = c("10", "20"),  # 'om' column with non-numeric values
    oc = c(5, 10)
  )

  # Call the estimate_oc function and expect an error to be thrown
  expect_error(estimate_oc(df = df), "Organic matter data must be class numeric")
})

# Test 3: Non-numeric 'oc' data
test_that("Non-numeric 'oc' data throws an error", {
  # Create a sample data frame with non-numeric 'oc' column
  df <- data.frame(
    core = c("Core1", "Core2"),
    site = c("Site1", "Site2"),
    ecosystem = c("Eco1", "Eco2"),
    species = c("Sp1", "Sp2"),
    om = c(10, 20),
    oc = c("5", "10")  # 'oc' column with non-numeric values
  )

  # Call the estimate_oc function and expect an error to be thrown
  expect_error(estimate_oc(df = df), "Organic carbon data must be class numeric")
})


# Test 4: 'om' value greater than 'oc' value
test_that("'om' value greater than 'oc' value throws an error", {
  # Create a sample data frame with 'om' value greater than 'oc' value
  df <- data.frame(
    core = c("Core1"),
    site = c("Site1"),
    ecosystem = c("Eco1"),
    species = c("Sp1"),
    om = c(20),  # 'om' value greater than 'oc' value
    oc = c(30)
  )

  # Call the estimate_oc function and expect an error to be thrown
  expect_error(estimate_oc(df = df), "Some organic carbon values are higher than organic matter values. Please check your data.")
})


