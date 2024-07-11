

test_that("decompact_linear works correctly with valid inputs", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compression = c(10, 20, 30),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6)
  )

  result <- decompact_linear(df)
  expect_true("mind_corrected" %in% names(result))
  expect_true("maxd_corrected" %in% names(result))
  expect_equal(result$mind_corrected, c(1.11, 2.50, 4.29), tolerance = 0.01)
  expect_equal(result$maxd_corrected, c(4.44, 6.25, 8.57), tolerance = 0.01)
})

test_that("decompact_linear stops with invalid dataframe input", {
  expect_error(decompact_linear(list(a = 1, b = 2)), "The data provided must be a tibble or data.frame")
})


test_that("decompact_linear stops if columns are not numeric", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compression = c("10", "20", "30"),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6)
  )

  expect_error(decompact_linear(df), "Compression data is not class numeric, please check")
})



test_that("decompact_linear handles missing compression values", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compression = c(10, NA, 30),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6)
  )

  expect_error(decompact_linear(df), "There are cores without estimated compresion: core2
Please, provide compression data of field measurements for all cores. If the core is not compressed compression should be 0")
})



test_that("decompact_linear works correctly with dry bulk density", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compression = c(10, 20, 30),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6),
    dbd = c(1.2, 1.5, 1.8)
  )

  result <- decompact_linear(df, dbd = "dbd")
  expect_true("dbd_corrected" %in% names(result))
  expect_equal(result$dbd_corrected, c(1.08, 1.20, 1.26), tolerance = 0.01)
})


test_that("decompact_linear check if compresion is provided in %", {
  df <- data.frame(
    core = c("core1", "core2", "core3"),
    compression = c(0.2, 0.1, 0.3),
    mind = c(1, 2, 3),
    maxd = c(4, 5, 6)
  )

  expect_warning(decompact_linear(df), "Compresion values should be provided in %")
})


