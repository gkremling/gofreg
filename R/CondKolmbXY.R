##' @title Conditional Kolmogorov test statistic for the joint distribution of
##'   (beta^T X,Y)
##' @description This class inherits from [TestStatistic] and implements a
##'   function to calculate the test statistic (and x-y-values that can be used
##'   to plot the underlying process).
##'
##'   The process underlying the test statistic is given in Kremling & Dikta
##'   (2024) and defined by \deqn{\bar{\alpha}_n(s,t) = \frac{1}{\sqrt{n}}
##'   \sum_{i=1}^n \left( I_{\{Y_i \le t\}} - F(t|\hat{\vartheta}_n, X_i)
##'   \right) I_{\{\hat{\beta}_n^T X_i \le s\}}, \quad (s,t) \in R^{2}.}
##'   {(see formula given in paper).}
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
##' ts <- CondKolmbXY$new()
##' ts$calc_stat(x, y, model)
##' print(ts)
##' ggplot2::ggplot() + ts$geom_ts_proc()
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {return(u+10)})
##' model2$fit(x, y, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Print value of test statistic and plot corresponding process
##' ts2 <- CondKolmbXY$new()
##' ts2$calc_stat(x, y, model2)
##' print(ts2)
##' ggplot2::ggplot() + ts2$geom_ts_proc()
CondKolmbXY <- R6::R6Class(
  classname = "CondKolmbXY",
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

      # compute sum_{i=1}^n (1{Yi<=Yj} - F(Yj|theta,Xi)) 1{beta^T Xi<=beta^T Xj} for each j
      # and take the maximum
      beta.x <- model$get_params()$beta %*% x
      proc <- 1/sqrt(n) * sapply(seq(1,n), function(j) { sum(((y <= y[j]) - model$F_yx(y[j], x)) * (beta.x <= beta.x[j])) })

      # set private fields accordingly
      private$value <- max(abs(proc))
      private$plot.x <- seq(1,n)
      private$plot.y <- proc
      invisible(self)
    }
  )
)
