##' @title R6 Class representing a parametric regression model
##' @description This class implements the maximum likelihood estimator for the model parameters
##'   as well as abstract methods to, for example, evaluate the conditional density and distribution functions.
##' @export
ParamRegrModel <- R6::R6Class("ParamRegrModel", public = list(
  #' @description Set the value of the model parameters used as default for the class functions
  #'
  #' @param params model parameters to use as default
  #'
  #' @export
  set_params = function(params) {
    private$params <- params
    invisible(self)
  },

  #' @description Returns the value of the model parameters used as default for the class functions
  #'
  #' @return model parameters used as default
  #'
  #' @export
  get_params = function() {
    return(private$params)
  },

  #' @description Calculates the maximum likelihood estimator for the model parameters based on given data
  #'
  #' @param x vector of covariates
  #' @param y response variable
  #' @param params_init initial value of the model parameters to use for the optimization
  #' @param inplace logical; if TRUE, default model parameters are set accordingly
  #'
  #' @return MLE of the model parameters for the given data
  #' @export
  fit = function(x, y, params_init = private$params) {
    if(anyNA(params_init)) {
      stop("Starting value of model parameters needs to be defined for the optimization.")
    }
    if(length(params_init)==1) {
      params_opt <- optim(par=params_init,
                          fn=private$loglik, x=x, y=y,
                          lower=0, upper=20, method="Brent")
    } else {
      params_opt <- optim(par=params_init,
                          fn=private$loglik, x=x, y=y,
                          method="Nelder-Mead")
    }
    return(params_opt$par)
  },

  #' @description Evaluates the conditional density function
  #'
  #' @param t value(s) at which the conditional density shall be evaluated
  #' @param x vector of covariates
  #' @param params use different model parameters
  #'
  #' @return value(s) of the conditional density function
  #' @export
  f_yx = function(t, x, params = private$params) {
    stop("Abstract method. Needs to be implemented.")
  },

  #' @description Evaluates the conditional distribution function
  #'
  #' @param t value(s) at which the conditional distribution shall be evaluated
  #' @param x vector of covariates
  #' @param params use different model parameters
  #'
  #' @return value(s) of the conditional distribution function
  #' @export
  F_yx = function(t, x, params = private$params) {
    stop("Abstract method. Needs to be implemented.")
  },

  #' @description Generates a new sample of response variables with the same conditional distribution
  #'
  #' @param x vector of covariates
  #' @param params use different model parameters
  #'
  #' @return vector of sampled response variables
  #' @export
  sample_yx = function(x, params = private$params) {
    stop("Abstract method. Needs to be implemented.")
  },

  #' @description Evaluates the regression function or in other terms the expected value of Y given X=x
  #'
  #' @param x vector of covariates
  #' @param params use different model parameters
  #'
  #' @return value of the regression function
  #' @export
  mean_yx = function(x, params = private$params) {
    stop("Abstract method. Needs to be implemented.")
  }), private = list(
    params = NA,

    # negative log-likelihood function that is minimized to determine the MLE
    loglik = function(x, y, params) {
      lik <- self$f_yx(y, x, params)
      if(any(is.nan(lik))) return(1e100)
      if(any(lik==0)) return(1e100)
      return(-sum(log(lik)))
  })
)
