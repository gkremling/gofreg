##' @title Generalized Linear Model with Normal Distribution
##' @description This class represents a generalized linear model with normal
##'   distribution. It inherits from [GLM] and implements its functions that,
##'   for example, evaluate the conditional density and distribution functions.
##' @param x vector of covariates
##' @param params model parameters to use (`list()` with tags beta and sd),
##'   defaults to the fitted parameter values
##' @export
##'
##' @examples
##' # Use the built-in cars dataset
##' x <- datasets::cars$speed
##' y <- datasets::cars$dist
##'
##' # Create an instance of a NormalGLM
##' model <- NormalGLM$new()
##'
##' # Fit a Normal GLM to the cars dataset
##' model$fit(x, y, params_init = list(beta=3, sd=2), inplace=TRUE)
##' params_opt <- model$get_params()
##'
##' # Plot the resulting regression function
##' plot(datasets::cars)
##' abline(a = 0, b = params_opt$beta)
##'
##' # Generate a sample for y for given x following the same distribution
##' x.new <- seq(min(x), max(x), by=2)
##' y.smpl <- model$sample_yx(x.new)
##' points(x.new, y.smpl, col="red")
##'
##' # Evaluate the conditional density, distribution and regression function at given values
##' model$f_yx(y.smpl, x.new)
##' model$F_yx(y.smpl, x.new)
##' y.pred <- model$mean_yx(x.new)
##' points(x.new, y.pred, col="blue")
NormalGLM <- R6::R6Class(
  classname = "NormalGLM",
  inherit = GLM,
  public = list(
    #' @description Calculates the maximum likelihood estimator for the model
    #'   parameters based on given data.
    #'
    #' @param y response variable
    #' @param params_init initial value of the model parameters to use for the
    #'   optimization (defaults to the fitted parameter values)
    #' @param inplace `logical`; if `TRUE`, default model parameters are set
    #'   accordingly and parameter estimator is not returned
    #'
    #' @return MLE of the model parameters for the given data, same shape as
    #'   `params_init`
    #' @export
    fit = function(x, y, params_init = private$params, inplace = FALSE) {
      private$check_params(params_init, x)
      params_opt <- super$fit(x, y, unlist(params_init, use.names=FALSE))
      params_opt <- list(beta = params_opt[-length(params_opt)], sd = params_opt[length(params_opt)])
      if (inplace) {
        private$params <- params_opt
        invisible(self)
      } else {
        params_opt
      }
    },

    #' @description Evaluates the conditional density function.
    #'
    #' @param t value(s) at which the conditional density shall be evaluated
    #'
    #' @return value(s) of the conditional density function, same shape as `t`
    #' @export
    f_yx = function(t, x, params=private$params) {
      super$check_params(params)
      # when computing the MLE, params is a plain vector and needs to be reshaped
      if(checkmate::test_atomic_vector(params, len=1+ifelse(is.matrix(x), nrow(x), 1))) {
        params <- list(beta = params[-length(params)], sd = params[length(params)])
      }
      mean <- self$mean_yx(x, params)
      sd <- params$sd
      dnorm(t, mean=mean, sd=sd)
    },

    #' @description Evaluates the conditional distribution function.
    #'
    #' @param t value(s) at which the conditional distribution shall be
    #'   evaluated
    #'
    #' @return value(s) of the conditional distribution function,  same shape as
    #'   `t`
    #' @export
    F_yx = function(t, x, params=private$params) {
      private$check_params(params, x)
      mean <- self$mean_yx(x, params)
      sd <- params$sd
      pnorm(t, mean=mean, sd=sd)
    },

    #' @description Generates a new sample of response variables with the same
    #'   conditional distribution.
    #'
    #' @return vector of sampled response variables, same length as `x`
    #' @export
    sample_yx = function(x, params=private$params) {
      private$check_params(params, x)
      mean <- self$mean_yx(x, params)
      sd <- params$sd
      rnorm(length(mean), mean=mean, sd=sd)
    }
  ),
  private = list(
    # @description Check that `params` have the correct form
    check_params = function(params, x) {
      super$check_params(params)
      checkmate::assert_list(params, len=2)
      checkmate::assert_names(names(params), identical.to = c("beta", "sd"))
      checkmate::assert_vector(params$beta, len=ifelse(is.matrix(x), nrow(x), 1))
    }
  )
)
