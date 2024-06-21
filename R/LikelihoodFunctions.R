#' Negative log-likelihood function for a parametric regression model
#'
#' @description The log-likelihood function for a parametric regression model
#'   with data (x,y) is given by the sum of the logarithm of the conditional
#'   density of Y given X=x evaluated at y.
#'
#'   This function is one option that can be used to fit a [ParamRegrModel]. It
#'   returns the negative log-likelihood value in order for `optim()` to
#'   maximize (instead of minimize).
#'
#' @param data `list()` with tags x and y containing the data
#' @param model [ParamRegrModel] to use for the likelihood function
#' @param params vector with model parameters to compute likelihood function for
#'
#' @return Value of the negative log-likelihood function
#' @export
loglik_xy <- function(data, model, params) {
  checkmate::assert_data_frame(data)
  checkmate::assert_names(names(data), must.include = c("x", "y"))
  checkmate::assert_class(model, "ParamRegrModel")
  suppressWarnings(lik <- model$f_yx(data$y, as.matrix(data$x), params))
  if (any(lik == 0) || checkmate::anyNaN(lik)) {
    return(1e100)
  }
  -sum(log(lik))
}

#' Negative log-likelihood function for a parametric regression model under
#' random censorship
#'
#' @description The log-likelihood function for a parametric regression model
#'   under random censorship with data (x,z,delta) is given by the sum of the
#'   logarithm of the conditional density of Y given X=x evaluated at z if z was
#'   uncensored or the logarithm of the conditional survival of Y given X=x
#'   evaluated at z if z was censored.
#'
#'   This function is one option that can be used to fit a [ParamRegrModel]. It
#'   returns the negative log-likelihood value in order for `optim()` to
#'   maximize (instead of minimize).
#'
#' @param data `list()` with tags x, z and delta containing the data
#' @param model [ParamRegrModel] to use for the likelihood function
#' @param params vector with model parameters to compute likelihood function for
#'
#' @return Value of the negative log-likelihood function
#' @export
loglik_xzd <- function(data, model, params) {
  checkmate::assert_data_frame(data)
  checkmate::assert_names(names(data), must.include = c("x", "z", "delta"))
  checkmate::assert_class(model, "ParamRegrModel")
  cens_index <- data$delta == 0
  cens <- c()
  unens <- c()

  # if there is uncensored data
  if (any(!cens_index)) {
    suppressWarnings(uncens <- model$f_yx(data$z[!cens_index], as.matrix(data[!cens_index, "x"]), params))
  }

  # if there is censored data
  if (any(cens_index)) {
    suppressWarnings(cens <- 1 - model$F_yx(data$z[cens_index], as.matrix(data[cens_index, "x"]), params))
  }

  lik <- c(uncens, cens)
  if (checkmate::anyNaN(lik) || any(lik == 0)) {
    return(1e100)
  }
  -sum(log(lik))
}
