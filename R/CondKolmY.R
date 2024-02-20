##' @title Conditional Kolmogorov test statistic for the marginal distribution
##'   of Y
##' @description This class inherits from [TestStatistic] and implements a
##'   function to calculate the test statistic (and x-y-values that can be used
##'   to plot the underlying process).
##'
##'   The process underlying the test statistic is given in Kremling & Dikta (2024) and defined by
##'   \deqn{\tilde{\alpha}_n(t) = \frac{1}{\sqrt{n}} \sum_{i=1}^n \left( I_{\{Y_i \le t\}} -
##'   F(t|\hat{\vartheta}_n, X_i) \right), \quad -\infty \le t \le \infty.}{(see formula given in paper).}
##' @export
##'
##' @examples
##' # Create an example dataset
##' set.seed(123)
##' n <- 100
##' x <- rbind(runif(n), rbinom(n, 1, 0.5))
##' model <- NormalGLM$new()
##' y <- model$sample_yx(x, params=list(beta=c(2,3), sd=1))
##'
##' # Fit the correct model
##' model$fit(x, y, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Print value of test statistic and plot corresponding process
##' ts <- CondKolmY$new()
##' ts$calc_stat(x, y, model)
##' print(ts)
##' ggplot2::ggplot() + ts$geom_ts_proc()
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {return(u+10)})
##' model2$fit(x, y, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Print value of test statistic and plot corresponding process
##' ts2 <- CondKolmY$new()
##' ts2$calc_stat(x, y, model2)
##' print(ts2)
##' ggplot2::ggplot() + ts2$geom_ts_proc()
CondKolmY <- R6::R6Class(
  classname = "CondKolmY",
  inherit = TestStatistic,
  public = list(
    #' @description Calculate the value of the test statistic for given data
    #'   (x,y) and a model to test for.
    #'
    #' @param x vector of covariates
    #' @param y response variable
    #' @param model [ParamRegrModel] to test for, already fitted to the data
    #'
    #' @export
    calc_stat = function(x, y, model) {
      if(anyNA(model$get_params())) {
        stop("Model first needs to be fitted to the data.")
      }
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
