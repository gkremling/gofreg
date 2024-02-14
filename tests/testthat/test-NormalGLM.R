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
