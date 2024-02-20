##' @title Marked empirical process test statistic for a given GLM
##' @description This class implements a function of \code{TestStatistic} to
##'   calculate the test statistic (and x-y-values that can be used to plot it).
##'
##'   The process underlying the test statistic is given in Dikta & Scheer
##'   (2021) and defined by \deqn{\bar{R}^1_n(u) = \frac{1}{\sqrt{n}}
##'   \sum_{i=1}^n \left( Y_i - m(X_i, \hat{\beta}_n) \right) I_{\{\hat{\beta}_n
##'   X_i \le u\}}, \quad -\infty \le u \le \infty.}{(see formula given in paper).}
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
##' ts <- MEP$new()
##' ts$calc_stat(x, y, model)
##' print(ts)
##' ggplot2::ggplot() + ts$geom_ts_proc()
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {return(u+10)})
##' model2$fit(x, y, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Print value of test statistic and plot corresponding process
##' ts2 <- MEP$new()
##' ts2$calc_stat(x, y, model2)
##' print(ts2)
##' ggplot2::ggplot() + ts2$geom_ts_proc()
MEP <- R6::R6Class(
  classname = "MEP",
  inherit = TestStatistic,
  public = list(
    #' @description Calculate the value of the test statistic for given data
    #'   (x,y) and a model to test for.
    #'
    #' @param x vector of covariates
    #' @param y response variable
    #' @param model [ParamRegrModel] to test for
    #'
    #' @export
    calc_stat = function(x, y, model) {
      n <- length(y)
      params <- model$get_params()
      checkmate::assert_names(names(params), must.include = c("beta"))
      beta <- params$beta
      checkmate::assert_vector(beta, len=dim(x)[1])

      # compute linear combination beta^T*X and residuals
      beta.x <- beta %*% x
      res <- y - model$mean_yx(x)

      # order residuals by beta^T*X, compute scaled cumsum (Rn1)
      ord.id <- order(beta.x)
      res.ord <- res[ord.id]
      Rn1 <- cumsum(res.ord)/sqrt(n)

      # determine KS statistic by computing the supremum of |Rn1|
      private$value <- max(abs(Rn1))
      private$plot.x <- beta.x[ord.id]
      private$plot.y <- Rn1
      invisible(self)
    }
  )
)
