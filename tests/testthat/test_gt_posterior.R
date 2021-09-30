
# declarations ------------------------------------------------------------

# the inla objects used
b05M04 <- readRDS(test_path("testdata", "fits", "b05M04.rds"))
i05M04 <- readRDS(test_path("testdata", "fits", "i05M04.rds"))


# tests -------------------------------------------------------------------

test_that("verify the inla model object", {
  expect_s3_class(b05M04, "brmsfit")
  expect_s3_class(i05M04, "inla")
})

test_that("gt_posterior", {
  summ <- brms::posterior_summary(b05M04) %>%
    as.data.frame() %>%
    filter(row.names(.) != "lp__") %>%
    gt_posterior(labs = list(title = "Posterior Summary BRMS",
                             subtitle = "Practice 5M4"),
                 qtl = list(Q2.5 = "2.5%", Q97.5 = "97.5%"),
                 .tab_options = list(heading.background.color = "lightgreen"))
  # cat("\n", str(summ), "\n")

  # skip("manual")
  expect_s3_class(summ, "gt_tbl")
  expect_length(summ, 15)
})

test_that("gt_posterior_compare", {
  summ <- list()
  summ$brm <- brms::posterior_summary(b05M04) %>%
    as.data.frame() %>%
    select(mean = Estimate, sd = Est.Error) %>%
    tibble::rownames_to_column(var = "var") %>%
    filter(var != "lp__")
  summ$inla <- eflINLA::posterior_summary(i05M04) %>%
    select(mean, sd) %>%
    tibble::rownames_to_column(var = "var")
  summ$gt <- gt_posterior_compare(
    summ, var_df = "brm",
    labs = list(title = "Posterior Summary Comparison",
                subtitle = "Practice 5M4"))
  # cat("\n", str(summ$gt), "\n")

  # skip("manual")
  expect_s3_class(summ$gt, "gt_tbl")
  expect_length(summ$gt, 15)
})
