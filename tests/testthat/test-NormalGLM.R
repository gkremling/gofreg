test_that("f_yx and F_yx work", {
  g1 <- function(u) {return(1/u)}
  beta <- c(1,2,3)
  sd <- 2
  model <- NormalGLM$new(g1)
  model$params <- list(beta=beta, sd=sd)
  x <- cbind(c(1,2,3), c(4,5,6))
  t <- c(0,4)
  expect_equal(model$f_yx(t, x), dnorm(t, mean=g1(beta %*% x), sd=sd))
  expect_equal(model$F_yx(t, x), pnorm(t, mean=g1(beta %*% x), sd=sd))
})

test_that("sample_yx works", {
  g1 <- function(u) {return(1/u)}
  beta <- c(1,2,3)
  sd <- 2
  model <- NormalGLM$new(g1)
  model$params <- list(beta=beta, sd=sd)
  x <- cbind(c(1,2,3), c(4,5,6))
  t <- c(0,4)
  set.seed(123)
  s1 <- model$sample_yx(x)
  set.seed(123)
  s2 <- rnorm(dim(x)[2], mean=g1(beta %*% x), sd=sd)
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
  model$fit(x,y, params_init=list(beta=c(1,2,3), sd=2))
  expect(all(beta-rep(0.1,3) <= model$params$beta), failure_message="some component of beta was estimated too low")
  expect(all(model$params$beta <= beta+rep(0.1,3)), failure_message="some component of beta was estimated too high")
  expect(sd-0.2 <= model$params$sd, failure_message="sd was estimated too low")
  expect(model$params$sd <= sd+0.2, failure_message="sd was estimated too high")
  # test <- glm(y ~ x.1+x.2+x.3-1, family=gaussian(link="log"), data=data.frame(x=t(x), y=y), start=c(1,2,3))
})
