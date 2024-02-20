test_that("f_yx and F_yx work", {
  g1 <- function(u) {return(1/u)}
  beta <- c(1,2,3)
  shape <- 2

  model <- WeibullGLM$new(g1)

  expect_error(model$f_yx(t, x))

  model$set_params(list(beta=beta, shape=shape))
  x <- cbind(c(1,2,3), c(4,5,6))
  t <- c(0,4)

  expect_equal(model$f_yx(t, x), dweibull(t, scale=g1(beta %*% x)/gamma(1+1/shape), shape=shape))
  expect_equal(model$F_yx(t, x), pweibull(t, scale=g1(beta %*% x)/gamma(1+1/shape), shape=shape))

  new.params <- list(beta=c(2,3,4), shape=5)

  expect_equal(model$f_yx(t, x, new.params), dweibull(t, scale=g1(new.params$beta %*% x)/gamma(1+1/new.params$shape), shape=new.params$shape))
  expect_equal(model$F_yx(t, x, new.params), pweibull(t, scale=g1(new.params$beta %*% x)/gamma(1+1/new.params$shape), shape=new.params$shape))
})

test_that("sample_yx works", {
  g1 <- function(u) {return(1/u)}
  beta <- c(1,2,3)
  shape <- 2
  x <- cbind(c(1,2,3), c(4,5,6))

  model <- WeibullGLM$new(g1)

  expect_error(model$sample_yx(x))

  model$set_params(list(beta=beta, shape=shape))
  set.seed(123)
  s1 <- model$sample_yx(x)
  set.seed(123)
  s2 <- rweibull(dim(x)[2], scale=g1(beta %*% x)/gamma(1+1/shape), shape=shape)

  expect_equal(s1, s2)

  new.params <- list(beta=c(2,3,4), shape=5)
  set.seed(123)
  s1 <- model$sample_yx(x, new.params)
  set.seed(123)
  s2 <- rweibull(dim(x)[2], scale=g1(new.params$beta %*% x)/gamma(1+1/new.params$shape), shape=new.params$shape)

  expect_equal(s1, s2)
})

test_that("fit works", {
  set.seed(123)
  g1 <- function(u) {return(exp(u))}
  beta <- c(1,2,3)
  shape <- 3
  n <- 100
  x <- rbind(runif(n), runif(n), rnorm(n))
  y <- rweibull(n, scale=g1(beta %*% x)/gamma(1+1/shape), shape=shape)

  model <- WeibullGLM$new(g1)

  # no initial parameter values
  expect_error(model$fit(x, y))

  # non-feasible initial parameter values
  expect_error(model$fit(x, y, params_init = list(beta=c(0,0,0), shape=0)))

  params_mle <- model$fit(x, y, params_init = list(beta=c(1,2,3), shape=3))
  expect(all(beta-rep(0.1,3) <= params_mle$beta), failure_message="some component of beta was estimated too low")
  expect(all(params_mle$beta <= beta+rep(0.1,3)), failure_message="some component of beta was estimated too high")
  expect(shape-0.3 <= params_mle$shape, failure_message="shape was estimated too low")
  expect(params_mle$shape <= shape+0.3, failure_message="shape was estimated too high")

  expect(is.na(model$get_params()), "Model parameters should not be defined yet.")
  model$fit(x, y, params_init = list(beta=c(1,2,3), shape=3), inplace = TRUE)
  expect_equal(model$get_params(), params_mle)

  # test <- glm(y ~ x.1+x.2+x.3-1, family=gaussian(link="log"), data=data.frame(x=t(x), y=y), start=c(1,2,3))
})

test_that("default linkinv in constructor works", {
  set.seed(123)
  beta <- c(1,2,3)
  shape <- 3
  n <- 1000
  x <- rbind(runif(n), runif(n), rbinom(n, 1, 0.5))
  y <- rweibull(n, scale=(beta %*% x)/gamma(1+1/shape), shape=shape)

  model <- WeibullGLM$new()

  params_mle <- model$fit(x, y, params_init = list(beta=c(1,2,3), shape=3))
  expect(all(beta-rep(0.1,3) <= params_mle$beta), failure_message="some component of beta was estimated too low")
  expect(all(params_mle$beta <= beta+rep(0.1,3)), failure_message="some component of beta was estimated too high")
  expect(shape-0.3 <= params_mle$shape, failure_message="shape was estimated too low")
  expect(params_mle$shape <= shape+0.3, failure_message="shape was estimated too high")
})
