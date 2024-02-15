##' @title Conditional Kolmogorov test statistic for the marginal distribution of Y
##' @description This class implements a function of \code{TestStatistic} to calculate the test statistic (and x-y-values that can be used to plot it).
##' @export
CondKolmY <- R6::R6Class(
  classname = "CondKolmY",
  inherit = TestStatistic,
  public = list(
    #' @description Calculate the value of the test statistic for given data (x,y) and a model to test for
    #'
    #' @param x vector of covariates
    #' @param y response variable
    #' @param model model of type ParamRegrModel to test for
    #'
    #' @export
    calc_stat = function(x, y, model) {
      n <- length(y)
      # compute ECDF of Y (non-parametric estimator for distribution of Y)
      Fyn <- ecdf(y)

      # determine jump points and value of the ECDF at these points
      t.vals <- knots(Fyn)
      Fyn.vals <- Fyn(t.vals)

      # determine semi-parametric estimator for distribution of Y evaluated at the same (jump) points
      Fypar.vals <- sapply(t.vals, function(t) { sum(model$F_yx(t, x))/n  })

      # determine KS statistic by computing the difference at the jump points
      proc <- sqrt(n) * (Fyn.vals-Fypar.vals)

      # set private fields accordingly
      private$value <- max(abs(proc))
      private$plot.x <- t.vals
      private$plot.y <- proc
      invisible(self)
    }
  )
)
