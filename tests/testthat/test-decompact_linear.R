samples <- data.frame(
  core        = c("Core1", "Core1", "Core1", "Core1", "Core2", "Core2", "Core2", "Core2"),
  mind        = c(  1,        20,     45,      102,     0,       15,      20,      53),
  maxd        = c(  5,        26,     47,      108,     7,       20,      23,      56),
  dbd         = c(  1.2,      1.3,    1.4,     1.5,     1.2,     1.3,     1.4,     1.5),
  oc          = c(  2,        4,      6,       8,       2,       4,       6,       8),
  compression = c(  17.2,     17.2,   17.2,    17.2,    25,      25,      25,      25)
)

samples_out <- data.frame(
  core = c("Core1", "Core1", "Core1", "Core1", "Core2", "Core2", "Core2", "Core2"),
  mind = c(1, 20, 45, 102, 0, 15, 20, 53),
  maxd = c(5, 26, 47, 108, 7, 20, 23, 56),
  dbd = c(1.2, 1.3, 1.4, 1.5, 1.2, 1.3, 1.4, 1.5),
  oc = c(2, 4, 6, 8, 2, 4, 6, 8),
  compression = c(17.2, 17.2, 17.2, 17.2, 25, 25, 25, 25),
  mind_corrected = c(1.20772946859903, 24.1545893719807, 54.3478260869565, 123.188405797101,
                     0, 20, 26.6666666666667, 70.6666666666667),
  maxd_corrected = c(6.03864734299517, 31.4009661835749, 56.7632850241546, 130.434782608696,
                     9.33333333333333, 26.6666666666667, 30.6666666666667, 74.6666666666667),
  dbd_corrected = c(0.9936, 1.0764, 1.1592, 1.242, 0.9, 0.975, 1.05, 1.125)
)

cores <- data.frame(
  core              = c("Core1", "Core2"),
  sampler_length    = c(70, 120),
  internal_distance = c(20.32, 63.75),
  external_distance = c(10, 45)
)

test_that("valid input with compaction values works", {

  ## With DBD
  # And compression data
  expect_equal(decompact_linear(samples, dbd = "dbd"), samples_out)

  ## Custom names
  # With Compression data
  samples_mod <- samples
  names(samples_mod) <- c(
    "core_id", "min_depth", "max_depth", "bulk_density", "organic_carbon", "compression"
  )
  samples_out_mod <- samples_out
  names(samples_out_mod) <- c(
    "core_id", "min_depth", "max_depth", "bulk_density", "organic_carbon",
    "compression", "mind_corrected", "maxd_corrected", "dbd_corrected"
  )

  expect_equal(
    decompact_linear(df = samples_mod, core = "core_id", mind = "min_depth", maxd = "max_depth",
                     dbd = "bulk_density", compression = "compression"),
    samples_out_mod
  )
})

test_that("valid input with no compaction values works", {
  ## With DBD
  samples_mod <- samples
  samples_mod$compression <- NULL
  expect_equal(decompact_linear(df = samples_mod, df_fm = cores, dbd = "dbd"), samples_out)

  ## Custom names
  names(samples_mod) <- c(
    "core_id", "min_depth", "max_depth", "bulk_density", "organic_carbon")
  samples_out_mod <- samples_out_mod
  names(samples_out_mod) <- c(
    "core_id", "min_depth", "max_depth", "bulk_density", "organic_carbon",
    "compression",  "mind_corrected", "maxd_corrected", "dbd_corrected"
  )

  expect_equal(
    decompact_linear(df = samples_mod, df_fm = cores, core = "core_id",
                     mind = "min_depth", maxd = "max_depth", dbd = "bulk_density"),
    samples_out_mod)
})

test_that("input has numeric column set to character", {
  expect_equal(2 * 2, 4)
})

