test_that("resample works without resampling x", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- list(x = dummy$x, y = dummy$y)
  model <- dummy$model

  rs <- ParamResamplingScheme$new()
  set.seed(123)
  data.rs <- rs$resample(data, model, rsmpl_x = FALSE)
  expect_equal(data.rs$x, data$x)
  set.seed(123)
  expect_equal(data.rs$y, model$sample_yx(data$x))

  set.seed(123)
  data.rs2 <- rs$resample(data, model)
  expect_equal(data.rs, data.rs2)
})

test_that("resample works with resampling x", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- list(x = dummy$x, y = dummy$y)
  model <- dummy$model

  rs <- ParamResamplingScheme$new()
  set.seed(123)
  data.rs <- rs$resample(data, model, rsmpl_x = TRUE)
  set.seed(123)
  expect_equal(data.rs$x, data$x[, sample(ncol(data$x), size=ncol(data$x), replace=T)])
  expect_equal(data.rs$y, model$sample_yx(data.rs$x))
})
