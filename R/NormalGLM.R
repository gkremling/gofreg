##' @title Generalized linear model with normal distribution
##' @description This class implements functions of \code{ParamRegrModel} that, for example, evaluate the conditional density and distribution functions.
##' @export
NormalGLM <- R6::R6Class(
  classname = "NormalGLM",
  inherit = ParamRegrModel,
  public = list(
    #' @field linkinv (`function`)
    #' Inverse link function.
    linkinv = NA,

    #' @description Initialize an object of class NormalGLM
    #'
    #' @param linkinv inverse link function
    #'
    #' @export
    initialize = function(linkinv) {
      self$linkinv <- linkinv
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
      if(anyNA(params)) {
        params <- self$params
      }
      mean = self$mean_yx(x, params)
      sd = self$params$sd
      return(dnorm(t, mean=mean, sd=sd))
    },

    #' @description Evaluates the conditional distribution function
    #'
    #' @param t value(s) at which the conditional distribution shall be evaluated
    #' @param x vector of covariates
    #'
    #' @return value(s) of the conditional distribution function
    #' @export
    F_yx = function(t,x) {
      mean = self$mean_yx(x)
      sd = self$params$sd
      return(pnorm(t, mean=mean, sd=sd))
    },

    #' @description Generates a new sample of response variables with the same conditional distribution
    #'
    #' @param x vector of covariates
    #'
    #' @return vector of sampled response variables
    #' @export
    sample_yx = function(x) {
      mean = self$mean_yx(x)
      sd = self$params$sd
      return(rnorm(dim(x)[2], mean=mean, sd=sd))
    },

    #' @description Evaluates the regression function or in other terms the expected value of Y given X=x
    #'
    #' @param x vector of covariates
    #' @param params use different model parameters
    #'
    #' @return value of the regression function
    #' @export
    mean_yx = function(x, params=NA) {
      if(anyNA(params)) {
        params <- self$params
      }
      mean = self$linkinv(params$beta %*% x)
      return(mean)
    })
)
