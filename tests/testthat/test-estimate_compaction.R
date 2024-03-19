
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

  # Test if the output data frame has the correct number of rows
  expect_equal(nrow(estimate_compaction(df)), nrow(df))

  # Test if the output data frame contains the compression column
  expect_true("compression" %in% names(estimate_compaction(df)))

  # Test if the compression values are calculated correctly
  expected_compression <- c(38, 50, 50)
  actual_compression <- estimate_compaction(df)$compression
  expect_equal(actual_compression, expected_compression, tolerance = 1)

  # Test if the function throws an error when non-numeric values are provided
  df$sampler_length <- as.character(df$sampler_length)
  expect_error(estimate_compaction(df), "'sampler_length' data must be class numeric")

  # Test if the function throws an error when internal_distance is smaller than external_distance
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    sampler_length = c(10, 15, 20),
    internal_distance = c(5, 10, 5),  # internal_distance < external_distance for the third row
    external_distance = c(2, 5, 10)
  )
  expect_error(estimate_compaction(df), "internal_distance is smaller than the external_distance")
})
