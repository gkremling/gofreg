##' @title R6 Class representing a parametric regression model
##' @description This class implements the maximum likelihood estimator for the model parameters
##'   as well as abstract methods to, for example, evaluate the conditional density and distribution functions.
##' @export
ParamRegrModel <- R6::R6Class("ParamRegrModel", public = list(
  #' @field params (`list()`)
  #' Model parameters.
  params = NA,

  #' @description Calculates the maximum likelihood estimator for the model parameters based on given data
  #'
  #' @param x vector of covariates
  #' @param y response variable
  #' @param params_init initial value of the model parameters to use for the optimization
  #'
  #' @export
  fit = function(x, y, params_init) {
    # Log likelihood function
    loglik <- function(params) {
      lik <- model$fyx(y, x, params)
      if(any(is.nan(lik))) return(1e100)
      if(any(lik==0)) return(1e100)
      return(-sum(log(lik)))
    }
    # maximize log likelihood function
    if(length(params_init)==1) {
      params_opt <- optim( par=params_init,
                    fn=log.like,
                    lower=0, upper=20, method="Brent")
    } else {
      params_opt <- optim( par=params_init,
                    fn=log.like,
                    method="Nelder-Mead")
    }
    self$params <- params_opt
    invisible(self)
  },

  #' @description Evaluates the conditional density function
  #'
  #' @param t value(s) at which the conditional density shall be evaluated
  #' @param x vector of covariates
  #' @param params use different model parameters
  #'
  #' @return value(s) of the conditional density function
  #' @export
  f_yx = function(t, x, params=NA) {
    stop("Abstract method. Needs to be implemented.")
  },

  #' @description Evaluates the conditional distribution function
  #'
  #' @param t value(s) at which the conditional distribution shall be evaluated
  #' @param x vector of covariates
  #'
  #' @return value(s) of the conditional distribution function
  #' @export
  F_yx = function(t,x) {
    stop("Abstract method. Needs to be implemented.")
  },

  #' @description Generates a new sample of response variables with the same conditional distribution
  #'
  #' @param x vector of covariates
  #'
  #' @return vector of sampled response variables
  #' @export
  sample_yx = function(x) {
    stop("Abstract method. Needs to be implemented.")
  },

  #' @description Evaluates the regression function or in other terms the expected value of Y given X=x
  #'
  #' @param x vector of covariates
  #'
  #' @return value of the regression function
  #' @export
  mean_yx = function(x) {
    stop("Abstract method. Needs to be implemented.")
  }), private = list(
))
