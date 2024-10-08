% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LikelihoodFunctions.R
\name{loglik_xzd}
\alias{loglik_xzd}
\title{Negative log-likelihood function for a parametric regression model under
random censorship}
\usage{
loglik_xzd(data, model, params)
}
\arguments{
\item{data}{\code{list()} with tags x, z and delta containing the data}

\item{model}{\link{ParamRegrModel} to use for the likelihood function}

\item{params}{vector with model parameters to compute likelihood function for}
}
\value{
Value of the negative log-likelihood function
}
\description{
The log-likelihood function for a parametric regression model
under random censorship with data (x,z,delta) is given by the sum of the
logarithm of the conditional density of Y given X=x evaluated at z if z was
uncensored or the logarithm of the conditional survival of Y given X=x
evaluated at z if z was censored.

This function is one option that can be used to fit a \link{ParamRegrModel}. It
returns the negative log-likelihood value in order for \code{optim()} to
maximize (instead of minimize).
}
\examples{
# Create an example dataset
n <- 100
x <- cbind(runif(n), rbinom(n, 1, 0.5))
model <- NormalGLM$new()
params.true <- list(beta = c(2,3), sd = 1)
y <- model$sample_yx(x, params = params.true)
c <- rnorm(n, mean(y) * 1.2, sd(y) * 0.5)
data <- dplyr::tibble(x = x, z = pmin(y, c), delta = as.numeric(y <= c))

# Compute negative log likelihood for true parameters
loglik_xzd(data, model, params.true)

# Compute negative log likelihood for wrong parameters (should be higher)
loglik_xzd(data, model, params = list(beta = c(1,2), sd = 0.5))
}
