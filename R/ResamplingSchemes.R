#' Parametric resampling scheme
#'
#' @description Generate a new, resampled dataset of the same shape as data
#'   following the given model. The covariates are kept the same and the
#'   response variables are drawn according to `model$sample_yx()`.
#'
#' @param data `list()` with tags x and y containing the original data
#' @param model [ParamRegrModel] to use for the resampling
#'
#' @returns `list()` with tags x and y containing the resampled data
#'
#' @export
#' @examples
#' # Create an example dataset
#' n <- 10
#' x <- rbind(runif(n), rbinom(n, 1, 0.5))
#' model <- NormalGLM$new()
#' params <- list(beta=c(2,3), sd=1)
#' y <- model$sample_yx(x, params=params)
#' data <- list(x=x, y=y)
#'
#' # Fit the model to the data
#' model$fit(data, params_init=params, inplace=TRUE)
#'
#' # Resample from the model given data
#' resample_param(data, model)
resample_param = function(data, model) {
  checkmate::assert_names(names(data), must.include = c("x", "y"))
  checkmate::assert_class(model, "ParamRegrModel")

  n <- length(data$y)

  # sample new survival times (according to parametric model)
  y.b <- model$sample_yx(data$x)

  list(x = data$x, y = y.b)
}

#' Parametric resampling scheme with resampling of covariates
#'
#' @description Generate a new, resampled dataset of the same shape as data
#'   following the given model. The covariates are resampled from `data$x` and
#'   the response variables are drawn according to `model$sample_yx()`.
#'
#' @param data `list()` with tags x and y containing the original data
#' @param model [ParamRegrModel] to use for the resampling
#'
#' @returns `list()` with tags x and y containing the resampled data
#'
#' @export
#' @examples
#' # Create an example dataset
#' n <- 10
#' x <- rbind(runif(n), rbinom(n, 1, 0.5))
#' model <- NormalGLM$new()
#' params <- list(beta=c(2,3), sd=1)
#' y <- model$sample_yx(x, params=params)
#' data <- list(x=x, y=y)
#'
#' # Fit the model to the data
#' model$fit(data, params_init=params, inplace=TRUE)
#'
#' # Resample from the model given data
#' resample_param(data, model)
resample_param_rsmplx = function(data, model) {
  checkmate::assert_names(names(data), must.include = c("x", "y"))
  checkmate::assert_class(model, "ParamRegrModel")

  n <- length(data$y)

  # resample covariates
  if(is.matrix(data$x)) {
    x.b <- data$x[, sample(ncol(data$x), size=n, replace=T)]
  } else {
    x.b <- sample(data$x, n, replace=T)
  }

  # sample new survival times (according to parametric model)
  y.b <- model$sample_yx(x.b)

  list(x = x.b, y = y.b)
}
