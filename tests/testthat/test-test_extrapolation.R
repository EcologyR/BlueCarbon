




# Define the test
test_that("test_extrapolation function produces expected output", {

  # Create a sample dataframe
  df <- data.frame(
    core = c("Core1", "Core2", "Core3", "Core4"),
    mind = c(0, 10, 20, 30),
    maxd = c(10, 20, 30, 40),
    dbd = c(1.2, 1.4, 1.3, 1.1),
    oc = c(0.5, 0.8, 1.0, 0.7)
  )

  # Call the function
  result <- test_extrapolation(df = df, depth = 40, core = "core", mind = "mind", maxd = "maxd", dbd = "dbd", oc = "oc")

  # Perform assertions to check the output

  # Check the number of columns in the output dataframe
  expect_equal(ncol(result), 15)

  # Check if the necessary columns exist in the output dataframe
  expect_true("core" %in% colnames(result))
  expect_true("stock" %in% colnames(result))
  expect_true("stock_90" %in% colnames(result))
  expect_true("stock_90_se" %in% colnames(result))

  # Check if the expected number of rows is present in the output dataframe
  expect_equal(nrow(result), 4)

  # You can add more specific assertions based on your expected output

})

# Run the tests
test_results <- testthat::test_file("path_to_your_test_file.R")
