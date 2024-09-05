
#Test 1: the function estimates final depths
test_that("decompact works correctly with valid inputs", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compaction = c(10, 20, 30),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6)
  )

  result <- decompact(df)
  expect_true("mind_corrected" %in% names(result))
  expect_true("maxd_corrected" %in% names(result))
  expect_equal(result$mind_corrected, c(1.11, 2.50, 4.29), tolerance = 0.01)
  expect_equal(result$maxd_corrected, c(4.44, 6.25, 8.57), tolerance = 0.01)
})

#Test 2: the function only work with dataframes
test_that("decompact stops with invalid dataframe input", {
  expect_error(decompact(list(a = 1, b = 2)))
})

#Test 3: the function check for numeric data
test_that("decompact stops if columns are not numeric", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compaction = c("10", "20", "30"),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6)
  )

  expect_error(decompact(df))
})


#Test 4: the function detect missing compaction values
test_that("decompact handles missing compaction values", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compaction = c(10, NA, 30),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6)
  )

  expect_warning(decompact(df))
})


#Test 5: the function estimates corrected dbd if a dbd column is provided
test_that("decompact works correctly with dry bulk density", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compaction = c(10, 20, 30),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6),
    dbd = c(1.2, 1.5, 1.8)
  )

  result <- decompact(df, dbd = "dbd")
  expect_true("dbd_corrected" %in% names(result))
  expect_equal(result$dbd_corrected, c(1.08, 1.20, 1.26), tolerance = 0.01)
})

#Test 6: the function stops if compaction is not provided in %
test_that("decompact check if compaction is provided in %", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compaction = c(0.2, 0.1, 0.3),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6)
  )

  expect_warning(decompact(df))
})


#Test 7: the function estimates decompaction properlly

test_that("decompact provide expected results", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compaction = c(12, 23, 0),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6)
  )

  out<- data.frame(
    core = c("core1", "core2", "core3"),
    compaction = c(12, 23, 0),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6),
    mind_corrected = c(1,2.5,3),
    maxd_corrected = c(4.5,6.5,6))

  expect_equal(decompact(df), out, tolerance = 1)})
