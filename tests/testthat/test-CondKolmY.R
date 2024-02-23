test_that("calc_stat works", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- list(x = dummy$x, y = dummy$y)
  model <- dummy$model

  test_stat <- CondKolmY$new()
  test_stat$calc_stat(data, model)
  expect_equal(test_stat$get_value(), 0.46643783)
})

test_that("calc_stat does not work for unfitted model", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- list(x = dummy$x, y = dummy$y)
  model <- dummy$model
  model$set_params(NA)

  test_stat <- CondKolmY$new()
  expect_error(test_stat$calc_stat(data, model))
})

test_that("calc_stat does not work for wrong type of data", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- list(x = dummy$x)
  model <- dummy$model

  test_stat <- CondKolmY$new()
  expect_error(test_stat$calc_stat(data, model))
})
