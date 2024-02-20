test_that("f_yx and F_yx work", {
  g1 <- function(u) {return(1/u)}
  beta <- c(1,2,3)
  sd <- 2

  model <- NormalGLM$new(g1)

  expect_error(model$f_yx(t, x))

  model$set_params(list(beta=beta, sd=sd))
  x <- cbind(c(1,2,3), c(4,5,6))
  t <- c(0,4)

  expect_equal(model$f_yx(t, x), dnorm(t, mean=g1(beta %*% x), sd=sd))
  expect_equal(model$F_yx(t, x), pnorm(t, mean=g1(beta %*% x), sd=sd))

  new.params <- list(beta=c(2,3,4), sd=5)

  expect_equal(model$f_yx(t, x, new.params), dnorm(t, mean=g1(new.params$beta %*% x), sd=new.params$sd))
  expect_equal(model$F_yx(t, x, new.params), pnorm(t, mean=g1(new.params$beta %*% x), sd=new.params$sd))
})

test_that("sample_yx works", {
  g1 <- function(u) {return(1/u)}
  beta <- c(1,2,3)
  sd <- 2
  x <- cbind(c(1,2,3), c(4,5,6))

  model <- NormalGLM$new(g1)

  expect_error(model$sample_yx(x))

  model$set_params(list(beta=beta, sd=sd))
  set.seed(123)
  s1 <- model$sample_yx(x)
  set.seed(123)
  s2 <- rnorm(dim(x)[2], mean=g1(beta %*% x), sd=sd)

  expect_equal(s1, s2)

  new.params <- list(beta=c(2,3,4), sd=5)
  set.seed(123)
  s1 <- model$sample_yx(x, new.params)
  set.seed(123)
  s2 <- rnorm(dim(x)[2], mean=g1(new.params$beta %*% x), sd=new.params$sd)

  expect_equal(s1, s2)
})

test_that("fit works", {
  set.seed(123)
  g1 <- function(u) {return(exp(u))}
  beta <- c(1,2,3)
  sd <- 2
  n <- 100
  x <- rbind(runif(n), runif(n), rnorm(n))
  y <- rnorm(n, mean=g1(beta %*% x), sd=sd)

  model <- NormalGLM$new(g1)

  # no initial parameter values
  expect_error(model$fit(x, y))

  # non-feasible initial parameter values
  expect_error(model$fit(x, y, params_init = list(beta=c(0,0,0), sd=0)))

  params_mle <- model$fit(x, y, params_init = list(beta=c(1,2,3), sd=2))
  expect(all(beta-rep(0.1,3) <= params_mle$beta), failure_message="some component of beta was estimated too low")
  expect(all(params_mle$beta <= beta+rep(0.1,3)), failure_message="some component of beta was estimated too high")
  expect(sd-0.2 <= params_mle$sd, failure_message="sd was estimated too low")
  expect(params_mle$sd <= sd+0.2, failure_message="sd was estimated too high")

  expect(is.na(model$get_params()), "Model parameters should not be defined yet.")
  model$fit(x, y, params_init = list(beta=c(1,2,3), sd=2), inplace = TRUE)
  expect_equal(model$get_params(), params_mle)

  # test <- glm(y ~ x.1+x.2+x.3-1, family=gaussian(link="log"), data=data.frame(x=t(x), y=y), start=c(1,2,3))
})

test_that("default linkinv in constructor works", {
  set.seed(123)
  beta <- c(1,2,3)
  sd <- 0.5
  n <- 1000
  x <- rbind(runif(n), runif(n), rnorm(n))
  y <- rnorm(n, mean=beta %*% x, sd=sd)

  model <- NormalGLM$new()

  params_mle <- model$fit(x, y, params_init = list(beta=c(1,2,3), sd=2))
  expect(all(beta-rep(0.1,3) <= params_mle$beta), failure_message="some component of beta was estimated too low")
  expect(all(params_mle$beta <= beta+rep(0.1,3)), failure_message="some component of beta was estimated too high")
  expect(sd-0.2 <= params_mle$sd, failure_message="sd was estimated too low")
  expect(params_mle$sd <= sd+0.2, failure_message="sd was estimated too high")
})
