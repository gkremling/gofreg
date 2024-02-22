test_that("get_stat_orig works", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  x <- dummy$x
  y <- dummy$y
  model <- dummy$model

  test_stat <- CondKolmY$new()
  goftest <- GOFTest$new(x, y, model, test_stat, boot_type="keep", nboot=10)

  ts1 <- goftest$get_stat_orig()
  expect_error(test_stat$get_value())
  ts2 <- test_stat$calc_stat(x, y, model)
  expect_equal(ts1, ts2)
})

test_that("get_stat_boot and get_pvalue work", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  x <- dummy$x
  y <- dummy$y
  model <- dummy$model

  test_stat <- CondKolmY$new()
  goftest <- GOFTest$new(x, y, model, test_stat, boot_type="keep", nboot=10)
  ts_boot <- goftest$get_stats_boot()

  get_val <- function(ts) {
    ts$get_value()
  }

  ts_boot_vals <- sapply(ts_boot, get_val)
  expect_equal(ts_boot_vals, c(0.52479397, 0.87484393, 0.63576482, 0.64580577, 0.69234386, 0.48805946, 0.48614489, 0.52749552, 0.69325262, 0.29166960))

  pval <- goftest$get_pvalue()
  expect_equal(pval, mean(goftest$get_stat_orig()$get_value() < ts_boot_vals))
  expect_equal(pval, 0.9)
})


test_that("get_pvalue works alone", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  x <- dummy$x
  y <- dummy$y
  model <- dummy$model

  test_stat <- CondKolmY$new()
  goftest <- GOFTest$new(x, y, model, test_stat, boot_type="keep", nboot=10)
  pval <- goftest$get_pvalue()
  expect_equal(pval, 0.9)
})
