test_that("get_stat_orig works", {
  set.seed(123)
  g1 <- function(u) {return(exp(u))}
  beta <- c(1,2,3)
  sd <- 2
  n <- 100
  x <- rbind(runif(n), runif(n), rnorm(n))
  y <- rnorm(n, mean=g1(beta %*% x), sd=sd)

  model <- NormalGLM$new(g1)
  model$fit(x,y, params_init = list(beta=c(1,2,3), sd=2), inplace = TRUE)

  test_stat <- CondKolmY$new()
  goftest <- GOFTest$new(x, y, model, test_stat, boot_type="keep", nboot=10)
  ts1 <- goftest$get_stat_orig()
  expect_error(test_stat$get_value())
  ts2 <- test_stat$calc_stat(x, y, model)
  expect_equal(ts1, ts2)
})

test_that("get_stat_boot and get_pvalue work", {
  set.seed(123)
  g1 <- function(u) {return(exp(u))}
  beta <- c(1,2,3)
  sd <- 2
  n <- 100
  x <- rbind(runif(n), runif(n), rnorm(n))
  y <- rnorm(n, mean=g1(beta %*% x), sd=sd)

  model <- NormalGLM$new(g1)
  model$fit(x,y, params_init = list(beta=c(1,2,3), sd=2), inplace = TRUE)

  test_stat <- CondKolmY$new()
  goftest <- GOFTest$new(x, y, model, test_stat, boot_type="keep", nboot=10)
  ts_boot <- goftest$get_stats_boot()

  get_val <- function(ts) {
    return(ts$get_value())
  }

  ts_boot_vals <- sapply(ts_boot, get_val)
  expect_equal(ts_boot_vals, c(0.52479397, 0.87484393, 0.63576482, 0.64580577, 0.69234386, 0.48805946, 0.48614489, 0.52749552, 0.69325262, 0.29166960))

  pval <- goftest$get_pvalue()
  expect_equal(pval, mean(goftest$get_stat_orig()$get_value() < ts_boot_vals))
  expect_equal(pval, 0.9)
})


test_that("get_pvalue works alone", {
  set.seed(123)
  g1 <- function(u) {return(exp(u))}
  beta <- c(1,2,3)
  sd <- 2
  n <- 100
  x <- rbind(runif(n), runif(n), rnorm(n))
  y <- rnorm(n, mean=g1(beta %*% x), sd=sd)

  model <- NormalGLM$new(g1)
  model$fit(x,y, params_init = list(beta=c(1,2,3), sd=2), inplace = TRUE)

  test_stat <- CondKolmY$new()
  goftest <- GOFTest$new(x, y, model, test_stat, boot_type="keep", nboot=10)
  pval <- goftest$get_pvalue()
  expect_equal(pval, 0.9)
})
