# declarations ------------------------------------------------------------
library(posterior, quietly = TRUE)


# the inla objects used
m04m07ctr <- readRDS(test_path("testdata", "fits", "m04m07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify the inla model object", {
  expect_s4_class(m04m07ctr, "map")
})


test_that("verify the inla model object", {
  # str(m04m07ctr)
  expect_s4_class(m04m07ctr, "map")
})

test_that("draw_posterior_map", {
  nsamples <- 3L

  samples <- draw_posterior_map(m04m07ctr, n = nsamples)
  # str(samples)
  # expect_type(samples, "list")
  expect_s3_class(samples, "draws_rvars")
  expect_identical(variables(samples), names(m04m07ctr@coef))
  expect_equal(nvariables(samples), length(m04m07ctr@coef))
  expect_equal(niterations(samples), nsamples)
})

