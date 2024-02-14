##' @title R6 Class representing a test statistic
##' @description This class contains an abstract methods to calculate the test statistic (and x-y-values that can be used to plot it).
##' @export
TestStatistic <- R6::R6Class(
  classname = "TestStatistic",
  public = list(
    #' @description Returns the value of the test statistic
    #'
    #' @return value of the test statistic
    #' @export
    get_value = function() {
      if(!is.na(private$value)) {
        return(private$value)
      }
      stop("Value of the test statistic is not assigned yet. Call calc_stat(x, y, model) first!")
    },
    #' @description Calculate the value of the test statistic for given data (x,y) and a model to test for
    #'
    #' @param x vector of covariates
    #' @param y response variable
    #' @param model model of type ParamRegrModel to test for
    #'
    #' @export
    calc_stat = function(x, y, model) {
      stop("Abstract method. Needs to be implemented.")
    },
    #' @description Returns vectors of x and y that can be used to plot the process corresponding to the test statistic
    #'
    #' @return list with plot.x and plot.y
    #' @export
    get_plot_xy = function() {
      if(!anyNA(c(private$plot.x, private$plot.y))) {
        return(list(plot.x=plot.x, plot.y=plot.y))
      }
      stop("The values of plot.x and plot.y are not assigned yet. Call calc_stat(x, y, model) first!")
    }
  ),
  private = list(
    value = NA,
    plot.x = NA,
    plot.y = NA
  )
)
