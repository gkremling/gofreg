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

    #' @description Calculates the maximum likelihood estimator for the model parameters based on given data
    #'
    #' @param x vector of covariates
    #' @param y response variable
    #' @param params_init initial value of the model parameters to use for the optimization
    #'
    #' @export
    fit = function(x, y, params_init) {
      checkmate::check_list(params_init, len=2)
      checkmate::check_names(names(params_init), identical.to = c("beta", "sd"))
      params_opt <- super$fit(x, y, unlist(params_init, use.names=FALSE))
      self$params <- list(beta = params_opt[-length(params_opt)], sd = params_opt[length(params_opt)])
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
      else if(checkmate::test_vector(params, len=1+dim(x)[1])) {
        params <- list(beta = params[-length(params)], sd = params[length(params)])
      }
      mean = self$mean_yx(x, params)
      sd = params$sd
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
