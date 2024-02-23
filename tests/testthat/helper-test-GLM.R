expect_params_range <- function(params_est, params_true, tol) {
  params_est_unlist <- unlist(params_est, use.names = FALSE)
  params_true_unlist <- unlist(params_true, use.names=FALSE)
  q <- length(params_est_unlist)
  for (i in 1:q) {
    expect_lt(params_est_unlist[i], params_true_unlist[i] + tol)
    expect_gt(params_est_unlist[i], params_true_unlist[i] - tol)
  }
}

test_glm_fF_yx <- function(distr, params, new.params, t, true_vals) {
  g1 <- function(u) {1/u}
  x <- cbind(c(1,2,3), c(4,5,6))

  # create model
  model <- GLM.new(distr = distr, linkinv = g1)

  # no parameters specified (nor saved yet)
  expect_error(model$f_yx(t, x))
  expect_error(model$F_yx(t, x))

  # wrong shape of parameters
  expect_error(model$f_yx(t, x, params=list(useless_param = 3)))
  expect_error(model$F_yx(t, x, params=list(useless_param = 3)))

  # use saved parameters
  model$set_params(params)

  # print(model$f_yx(t, x))

  expect_equal(model$f_yx(t, x), true_vals(t, x, g1, params)$dens)
  expect_equal(model$F_yx(t, x), true_vals(t, x, g1, params)$dist)

  # use new parameters if specified
  expect_equal(model$f_yx(t, x, new.params), true_vals(t, x, g1, new.params)$dens)
  expect_equal(model$F_yx(t, x, new.params), true_vals(t, x, g1, new.params)$dist)
}

test_glm_sample_yx <- function(distr, params, new.params, expected_sample) {
  g1 <- function(u) {1/u}
  x <- cbind(c(1,2,3), c(4,5,6))

  # create model
  model <- GLM.new(distr, g1)

  # no parameters specified (nor saved yet)
  expect_error(model$sample_yx(x))

  # wrong shape of parameters
  expect_error(model$sample_yx(x, params=list(useless_param = 3)))

  # use saved parameters
  model$set_params(params)
  set.seed(123)
  s1 <- model$sample_yx(x)
  # print(s1)
  set.seed(123)
  s2 <- expected_sample(x, g1, params)
  expect_equal(s1, s2)

  # use new parameters if specified
  set.seed(123)
  s1 <- model$sample_yx(x, new.params)
  set.seed(123)
  s2 <- expected_sample(x, g1, new.params)
  expect_equal(s1, s2)
}

test_glm_fit <- function(distr, params_true, params_error, tol, multi) {
  set.seed(123)

  # univariate oder multidimensional covariates x
  if (multi) {
    dummy_xymodel <- "dummy_xymodel_x3"
  } else {
    dummy_xymodel <- "dummy_xymodel_x1"
  }

  # create data and model
  dummy <- do.call(dummy_xymodel, args=list(params_true=params_true, distr=distr))
  data <- list(x = dummy$x, y = dummy$y)
  model <- dummy$model

  # no initial parameter values
  expect_error(model$fit(data))

  # wrong shape of initial parameters
  expect_error(model$fit(data, params_init=list(useless_param = 3)))

  # non-feasible initial parameter values
  expect_error(model$fit(data, params_init = params_error))

  # estimated parameters are close to true values
  params_est <- model$fit(data, params_init = params_true)

  expect_params_range(params_est, params_true, tol)

  # by default, fit does not set model parameters
  expect(is.na(model$get_params()), "Model parameters should not be defined yet.")

  # model parameters are set if inplace=TRUE
  model$fit(data, params_init = params_true, inplace = TRUE)
  expect_equal(model$get_params(), params_est)
}
