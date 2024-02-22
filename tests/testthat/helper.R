#' Create dummy GLM data and model (with 1-dim x)
#'
#' @param params_true true model parameters (list)
#' @param distr distribution family used for the model
#'
#' @return `list()` with tags `x`, `y` `model`
dummy_xymodel_x1 <- function(params_true, distr, n = 1000) {
  g1 <- function(u) {exp(u)}

  x <- runif(n)

  model <- GLM.new(distr, g1)
  y <- model$sample_yx(x, params_true)
  list(x=x, y=y, model=model)
}

#' Create dummy GLM data and model (with 3-dim x)
#'
#' @param params_true true model parameters (list)
#' @param distr distribution family used for the model
#'
#' @return `list()` with tags `x`, `y` `model`
dummy_xymodel_x3 <- function(params_true, distr, n = 1000) {
  g1 <- function(u) {exp(u)}

  x <- rbind(runif(n), runif(n), rnorm(n))

  model <- GLM.new(distr, g1)
  y <- model$sample_yx(x, params_true)
  list(x=x, y=y, model=model)
}

dummy_xymodel_fitted <- function() {
  params_true <- list(beta=c(1,2,3), sd=2)
  distr = "normal"

  dummy <- dummy_xymodel_x3(params_true, distr, n=100)
  dummy$model$fit(dummy$x, dummy$y, params_init=params_true, inplace=TRUE)
  dummy
}
