##' @title Goodness-of-fit test for parametric regressin
##' @description This class implements functions to calculate the test statistic for the original data
##'   as well as the statistics for bootstrap samples. It also offers the possibilitz to computate the
##'   corresponding bootstrap p-value.
##' @export
GOFTest <- R6::R6Class(
  classname = "GOFTest",
  public = list(
    #' @description Initialize an object of class GOFTest
    #'
    #' @param x vector of covariates
    #' @param y response variables
    #' @param model_fitted object of class ParamRegrModel with defined default parameters
    #' @param test_stat object of class TestStatistic
    #' @param boot_type a specification for the type of resampling scheme: "rsmpl" if covariates shall resampled or "keep" if they shall be kept the same
    #' @param nboot number of bootstrap iterations
    #'
    #' @export
    initialize = function(x, y, model_fitted, test_stat, boot_type, nboot) {
       checkmate::check_class(model_fitted, "ParamRegrModel")
       checkmate::check_class(test_stat, "TestStatistic")
       checkmate::check_choice(boot_type, c("rsmpl", "keep"))
       private$x <- x
       private$y <- y
       private$model <- model_fitted
       private$test_stat <- test_stat
       private$boot_type <- boot_type
       private$nboot <- nboot
    },

    #' @description Calculates the test statistic for the original data and model
    #'
    #' @return object of class \code{TestStatistic}
    #' @export
    get_stat_orig = function() {
      if(!checkmate::test_class(private$stat_orig, "TestStatistic") && anyNA(private$stat_orig)) {
        private$stat_orig <- private$test_stat$clone(deep = TRUE)
        private$stat_orig$calc_stat(private$x, private$y, private$model)
      }
      return(private$stat_orig)
    },

    #' @description Calculates the test statistics for the resampled data and corresponding models
    #'
    #' @return vector of objects of class \code{TestStatistic}
    #' @export
    get_stats_boot = function() {
      if(!checkmate::test_class(private$stats_boot, "TestStatistic") && anyNA(private$stats_boot)) {
        private$stats_boot <- replicate(private$nboot, private$bootstrap())
      }
      return(private$stats_boot)
    },

    #' @description Calculates the bootstrap p-value for the given model
    #'
    #' @return p-value for the null hypothesis that \code{y} was generated according to \code{model}
    #' @export
    get_pvalue = function() {
      stat_orig <- self$get_stat_orig()
      stats_boot <- self$get_stats_boot()

      get_val <- function(stat) {
        return(stat$get_value())
      }

      val_orig <- stat_orig$get_value()
      val_boot <- sapply(stats_boot, get_val)
      return(mean(val_orig < val_boot))
    }
  ),
  private = list(
    x = NA,
    y = NA,
    model = NA,
    test_stat = NA,
    boot_type = NA,
    nboot = NA,
    stat_orig = NA,
    stats_boot = NA,
    bootstrap = function() {
      n <- length(private$y)

      # resample covariates (or not)
      if (private$boot_type == "rsmpl") {
        x.b <- private$x[, sample(ncol(private$x), size=n, replace=T)]
      } else {
        x.b <- private$x
      }

      # sample new survival times (according to parametric model)
      y.b <- private$model$sample_yx(x.b)

      # find MLE and test statistic for bootstrap data
      model.b <- private$model$clone(deep = TRUE)
      model.b$fit(x.b, y.b, inplace=TRUE)

      stat.b <- private$test_stat$clone(deep = TRUE)
      stat.b$calc_stat(x.b, y.b, model.b)

      # return requested statistic
      return(stat.b)
    }
  )
)
