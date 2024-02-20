test_that("calc_stat works", {
  set.seed(123)

  dummy <- dummy_xymodel()
  x <- dummy$x
  y <- dummy$y
  model <- dummy$model

  test_stat <- MEP$new()
  test_stat$calc_stat(x, y, model)
  expect_equal(test_stat$get_value(), 2.5130817)
})
