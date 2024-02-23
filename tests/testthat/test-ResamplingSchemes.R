test_that("resample_param works without resampling x", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- list(x = dummy$x, y = dummy$y)
  model <- dummy$model

  set.seed(123)
  data.rs <- resample_param(data, model)
  expect_equal(data.rs$x, data$x)
  set.seed(123)
  expect_equal(data.rs$y, model$sample_yx(data$x))
})

test_that("resample works with resampling x", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- list(x = dummy$x, y = dummy$y)
  model <- dummy$model

  set.seed(123)
  data.rs <- resample_param_rsmplx(data, model)
  set.seed(123)
  expect_equal(data.rs$x, data$x[, sample(ncol(data$x), size=ncol(data$x), replace=T)])
  expect_equal(data.rs$y, model$sample_yx(data.rs$x))
})
