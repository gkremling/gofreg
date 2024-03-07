##' @title Generalized Linear Model
##' @description This class specializes [ParamRegrModel]. It is the abstract
##'   base class for parametric generalized linear model objects with specific
##'   distribution family such as [NormalGLM] and handles the (inverse) link
##'   function.
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
    initialize = function(linkinv = function(u) {u}) {
      checkmate::assert_function(linkinv, nargs=1)
      private$linkinv <- linkinv
    },

    #' @description Evaluates the regression function or in other terms the
    #'   expected value of Y given X=x.
    #'
    #' @param x vector of covariates
    #' @param params model parameters to use, defaults to the fitted parameter
    #'   values
    #'
    #' @return value of the regression function
    #' @export
    mean_yx = function(x, params=private$params) {
      private$check_params(params, x)
      private$linkinv(as.matrix(x) %*% params$beta)
    }
  ),
  private = list(
    linkinv = NA
  )
)

#' Create GLM object with specific distribution family
#'
#' @description This constructor function can be used to create an instance of a
#' parametric GLM with specific distribution family, returning a new object of
#' [NormalGLM], [ExpGLM], [WeibullGLM] or [GammaGLM], depending on the value of
#' `distr`.
#'
#' @param distr distribution family
#' @param linkinv inverse link function
#'
#' @return new instance of a GLM-subclass
#' @export
#'
#' @examples
#' model <- GLM.new(distr="normal")
#' # see examples of GLM-subclasses (e.g. NormalGLM) for how to use such models
GLM.new <- function(distr, linkinv = function(u) {u}) {
  distr_poss <- c("normal", "exp", "weibull", "gamma")
  checkmate::assert_function(linkinv, nargs=1)
  checkmate::check_choice(distr, distr_poss)
  switch(distr,
         normal = NormalGLM$new(linkinv),
         exp = ExpGLM$new(linkinv),
         weibull = WeibullGLM$new(linkinv),
         gamma = GammaGLM$new(linkinv),
         stop(paste0("Bug in the code: There is a distribution family listed in distr_poss",
                                "which is not taken account of in the switch-statement.")))
}
