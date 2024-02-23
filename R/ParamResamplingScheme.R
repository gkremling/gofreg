##' @title Parametric Resampling Scheme for (X,Y) data
##' @description This class inherits from [ResamplingScheme] and implements a
##'   function to resample new data based on the given data and assumed model.
##'
##'   The covariates are kept the same (default) or resampled from `data$x` and
##'   the response variables are drawn according to `model$sample_yx()`.
##' @export
##'
##' @examples
##' # Create an example dataset
##' set.seed(123)
##' n <- 10
##' x <- rbind(runif(n), rbinom(n, 1, 0.5))
##' model <- NormalGLM$new()
##' params <- list(beta=c(2,3), sd=1)
##' y <- model$sample_yx(x, params=params)
##' data <- list(x = x, y = y)
##'
##' # Fit the model to the data
##' model$fit(x, y, params_init=params, inplace=TRUE)
##'
##' # Resample from the model given data with or without resampling x
##' rs <- ParamResamplingScheme$new()
##' rs$resample(data, model)
##' rs$resample(data, model, rsmpl_x=TRUE)
ParamResamplingScheme <- R6::R6Class(
  classname = "ParamResamplingScheme",
  inherit = ResamplingScheme,
  public = list(
    #' @description Generate a new, resampled dataset of the same shape as data
    #'   following the given model.
    #'
    #' @param data `list()` with tags x and y containing the original data
    #' @param model [ParamRegrModel] to use for the resampling
    #' @param rsmpl_x if TRUE covariates are resampled, if FALSE (default)
    #'   covariates are kept the same
    #'
    #' @returns `list()` with tags x and y containing the resampled data
    #'
    #' @export
    resample = function(data, model, rsmpl_x = FALSE) {
      checkmate::assert_names(names(data), must.include = c("x", "y"))
      checkmate::assert_class(model, "ParamRegrModel")
      checkmate::assert_logical(rsmpl_x, len=1)

      n <- length(data$y)

      # resample covariates (or not)
      if (rsmpl_x) {
        x.b <- data$x[, sample(ncol(data$x), size=n, replace=T)]
      } else {
        x.b <- data$x
      }

      # sample new survival times (according to parametric model)
      y.b <- model$sample_yx(x.b)

      list(x = x.b, y = y.b)
    }
  )
)
