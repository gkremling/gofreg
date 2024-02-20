##' @title Generalized Linear Model
##' @description This class specializes [ParamRegrModel] and is the abstract
##'   base class for generalized linear model objects like [NormalGLM]. It
##'   handles the (inverse) link function.
##'
##' @param x vector of covariates
##' @param params model parameters to use (`list()` with tags beta and sd),
##'   defaults to the fitted parameter values
##' @export
GLM <- R6::R6Class(
  classname = "GLM",
  inherit = ParamRegrModel,
  public = list(
    #' @description Initialize an object of class GLM.
    #'
    #' @param linkinv inverse link function, defaults to identity function
    #'
    #' @export
    initialize = function(linkinv = function(u) {return(u)}) {
      checkmate::assert_function(linkinv, nargs=1)
      private$linkinv <- linkinv
    }),
  private = list(
    linkinv = NA
  )
)
