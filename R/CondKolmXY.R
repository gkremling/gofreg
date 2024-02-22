##' @title Conditional Kolmogorov test statistic for the joint distribution of
##'   (X,Y)
##' @description This class inherits from [TestStatistic] and implements a
##'   function to calculate the test statistic (and x-y-values that can be used
##'   to plot the underlying process).
##'
##'   The process underlying the test statistic is given in Andrews (1997) and
##'   defined by \deqn{\nu_n(x,y) = \frac{1}{\sqrt{n}} \sum_{i=1}^n \left(
##'   I_{\{Y_i \le y\}} - F(y|\hat{\vartheta}_n, X_i) \right) I_{\{X_i \le x\}},
##'   \quad (x,y) \in R^{p+1}.}{(see formula given in paper).}
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
##' ts <- CondKolmXY$new()
##' ts$calc_stat(x, y, model)
##' print(ts)
##' ggplot2::ggplot() + ts$geom_ts_proc()
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {u+10})
##' model2$fit(x, y, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Print value of test statistic and plot corresponding process
##' ts2 <- CondKolmXY$new()
##' ts2$calc_stat(x, y, model2)
##' print(ts2)
##' ggplot2::ggplot() + ts2$geom_ts_proc()
CondKolmXY <- R6::R6Class(
  classname = "CondKolmXY",
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

      # compute sum_{i=1}^n (1{Yi<=Yj} - F(Yj|theta,Xi)) 1{Xi<=Xj} for each j
      # and take the maximum
      proc <- 1/sqrt(n) * sapply(seq(1,n), function(j) { sum(((y <= y[j]) - model$F_yx(y[j], x)) * (colSums(x <= x[,j]) == ifelse(is.matrix(x), nrow(x), 1))) })

      # set private fields accordingly
      private$value <- max(abs(proc))
      private$plot.x <- seq(1,n)
      private$plot.y <- proc
      invisible(self)
    }
  )
)
