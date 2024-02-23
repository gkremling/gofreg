##' @title Goodness-of-fit test for parametric regression
##' @description This class implements functions to calculate the test statistic
##'   for the original data as well as the statistics for bootstrap samples. It
##'   also offers the possibility to compute the corresponding bootstrap
##'   p-value.
##' @export
##'
##' @examples
##' # Create an example dataset
##' set.seed(123)
##' n <- 100
##' x <- rbind(runif(n), rbinom(n, 1, 0.5))
##' model <- NormalGLM$new()
##' y <- model$sample_yx(x, params=list(beta=c(2,3), sd=1))
##' data <- list(x = x, y = y)
##'
##' # Fit the correct model
##' model$fit(x, y, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Calculate the bootstrap p-value and plot the corresponding processes
##' goftest <- GOFTest$new(data, model, test_stat = CondKolmY$new(), nboot = 100)
##' goftest$get_pvalue()
##' goftest$plot_procs()
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {u+10})
##' model2$fit(x, y, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Calculate the bootstrap p-value and plot the corresponding processes
##' goftest2 <- GOFTest$new(data, model2, test_stat = CondKolmY$new(), nboot = 100)
##' goftest2$get_pvalue()
##' goftest2$plot_procs()
GOFTest <- R6::R6Class(
  classname = "GOFTest",
  public = list(
    #' @description Initialize an instance of class [GOFTest].
    #'
    #' @param data `list()` containing the data
    #' @param model_fitted object of class [ParamRegrModel] with fitted
    #'   parameters
    #' @param test_stat object of class [TestStatistic]
    #' @param nboot number of bootstrap iterations
    #' @param resampling_scheme object of class [ResamplingScheme]
    #'
    #' @export
    initialize = function(data, model_fitted, test_stat, nboot, resampling_scheme = ParamResamplingScheme$new()) {
       checkmate::assert_class(model_fitted, "ParamRegrModel")
       checkmate::assert_class(test_stat, "TestStatistic")
       checkmate::assert_class(resampling_scheme, "ResamplingScheme")
       private$data <- data
       private$model <- model_fitted
       private$test_stat <- test_stat
       private$resampling_scheme <- resampling_scheme
       private$nboot <- nboot
    },

    #' @description Calculates the test statistic for the original data and
    #'   model.
    #'
    #' @return object of class [TestStatistic]
    #' @export
    get_stat_orig = function() {
      if(!checkmate::test_class(private$stat_orig, "TestStatistic") && anyNA(private$stat_orig)) {
        private$stat_orig <- private$test_stat$clone(deep = TRUE)
        private$stat_orig$calc_stat(private$data, private$model)
      }
      private$stat_orig
    },

    #' @description Calculates the test statistics for the resampled data and
    #'   corresponding models.
    #'
    #' @return vector of length `nboot` containing objects of class
    #'   [TestStatistic]
    #' @export
    get_stats_boot = function() {
      if(!checkmate::test_class(private$stats_boot, "TestStatistic") && anyNA(private$stats_boot)) {
        private$stats_boot <- replicate(private$nboot, private$bootstrap())
      }
      private$stats_boot
    },

    #' @description Calculates the bootstrap p-value for the given model.
    #'
    #' @return p-value for the null hypothesis that `y` was generated according
    #'   to `model`
    #' @export
    get_pvalue = function() {
      stat_orig <- self$get_stat_orig()
      stats_boot <- self$get_stats_boot()

      get_val <- function(stat) {
        stat$get_value()
      }

      val_orig <- stat_orig$get_value()
      val_boot <- sapply(stats_boot, get_val)
      mean(val_orig < val_boot)
    },

    #' @description Plots the processes underlying the bootstrap test statistics
    #'   (gray) and the original test statistic (red)
    #'
    #' @export
    plot_procs = function() {
      gpl <- ggplot2::ggplot() + ggplot2::ggtitle(sprintf("Test statistic, p-value=%s", self$get_pvalue()))
      for(ts in private$stats_boot) {
        gpl <- gpl + ts$geom_ts_proc(col = "gray40")
      }
      gpl <- gpl + private$stat_orig$geom_ts_proc(col="red")
      print(gpl)
      invisible(self)
    }
  ),
  private = list(
    data = NA,
    model = NA,
    test_stat = NA,
    resampling_scheme = NA,
    nboot = NA,
    stat_orig = NA,
    stats_boot = NA,

    # Implements a single bootstrap iteration
    #
    # @description Generates a bootstrap sample according to the resampling
    #   scheme, fits the model and computes the corresponding test statistic.
    #
    # @returns object of class [TestStatistic]
    bootstrap = function() {
      # create bootstrap data
      data.b <- private$resampling_scheme$resample(private$data, private$model)

      # find MLE and test statistic for bootstrap data
      model.b <- private$model$clone(deep = TRUE)
      model.b$fit(data.b$x, data.b$y, inplace=TRUE)

      stat.b <- private$test_stat$clone(deep = TRUE)
      stat.b$calc_stat(data.b, model.b)

      # return requested statistic
      stat.b
    }
  )
)
