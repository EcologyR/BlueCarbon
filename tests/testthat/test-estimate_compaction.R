
test_that("estimate_compaction function works as expected", {
  # Create a sample data frame for testing
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    sampler_length = c(10, 15, 20),
    internal_distance = c(5, 10, 15),
    external_distance = c(2, 5, 10)
  )

  # Test if the function runs without error
  expect_no_error(estimate_compaction(df))

  # Test if the output data frame contains the compaction column
  expect_true("compaction" %in% names(estimate_compaction(df)))

  # Test if the compaction values are calculated correctly
  expected_compaction <- c(38, 50, 50)
  actual_compaction <- estimate_compaction(df)$compaction
  expect_equal(actual_compaction, expected_compaction, tolerance = 1)

  # Test if the function throws an error when non-numeric values are provided
  df$sampler_length <- as.character(df$sampler_length)
  expect_error(estimate_compaction(df))

  # Test if the function throws an error when internal_distance is smaller than external_distance
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    sampler_length = c(10, 15, 20),
    internal_distance = c(5, 10, 5),  # internal_distance < external_distance for the third row
    external_distance = c(2, 5, 10)
  )
  expect_error(estimate_compaction(df))

})
