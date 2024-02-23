##' @title ResamplingScheme Class
##' @description This is the abstract base class for resampling scheme objects
##'   like [ParamResamplingScheme].
##'
##'   Resampling schemes are built around the key method `resample()` which
##'   generates new data based on the given data and assumed model.
##' @export
ResamplingScheme <- R6::R6Class(
  classname = "ResamplingScheme",
  public = list(
    #' @description Generate a new, resampled dataset of the same shape as data
    #'   following the given model.
    #'
    #' @param data `list()` containing the original data
    #' @param model [ParamRegrModel] to use for the resampling
    #'
    #' @returns `list()` with the same tags as data containing the resampled
    #'   data
    #'
    #' @export
    resample = function(data, model) {
      stop("Abstract method. Needs to be implemented.")
    }
  )
)
