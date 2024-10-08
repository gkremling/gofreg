% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ParamRegrModel.R
\name{ParamRegrModel}
\alias{ParamRegrModel}
\title{Parametric regression model (abstract class)}
\description{
This is the abstract base class for parametric regression model
objects like \link{NormalGLM}.

Parametric regression models are built around the following key tasks:
\itemize{
\item A method \code{fit()} to fit the model to given data, i.e. compute the MLE
for the model parameters
\item Methods \code{f_yx()}, \code{F_yx()} and \code{mean_yx()} to evaluate the conditional
density, distribution and regression function
\item A method \code{sample_yx()} to generate a random sample of response variables
following the model given a vector of covariates
}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ParamRegrModel-set_params}{\code{ParamRegrModel$set_params()}}
\item \href{#method-ParamRegrModel-get_params}{\code{ParamRegrModel$get_params()}}
\item \href{#method-ParamRegrModel-fit}{\code{ParamRegrModel$fit()}}
\item \href{#method-ParamRegrModel-f_yx}{\code{ParamRegrModel$f_yx()}}
\item \href{#method-ParamRegrModel-F_yx}{\code{ParamRegrModel$F_yx()}}
\item \href{#method-ParamRegrModel-F1_yx}{\code{ParamRegrModel$F1_yx()}}
\item \href{#method-ParamRegrModel-sample_yx}{\code{ParamRegrModel$sample_yx()}}
\item \href{#method-ParamRegrModel-mean_yx}{\code{ParamRegrModel$mean_yx()}}
\item \href{#method-ParamRegrModel-clone}{\code{ParamRegrModel$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamRegrModel-set_params"></a>}}
\if{latex}{\out{\hypertarget{method-ParamRegrModel-set_params}{}}}
\subsection{Method \code{set_params()}}{
Set the value of the model parameters used as default for the
class functions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamRegrModel$set_params(params)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{params}}{model parameters to use as default}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The modified object (\code{self}), allowing for method chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamRegrModel-get_params"></a>}}
\if{latex}{\out{\hypertarget{method-ParamRegrModel-get_params}{}}}
\subsection{Method \code{get_params()}}{
Returns the value of the model parameters used as default for
the class functions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamRegrModel$get_params()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
model parameters used as default
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamRegrModel-fit"></a>}}
\if{latex}{\out{\hypertarget{method-ParamRegrModel-fit}{}}}
\subsection{Method \code{fit()}}{
Calculates the maximum likelihood estimator for the model
parameters based on given data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamRegrModel$fit(data, params_init = private$params, loglik = loglik_xy)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{list containing the data to fit the model to}

\item{\code{params_init}}{initial value of the model parameters to use for the
optimization (defaults to the fitted parameter values)}

\item{\code{loglik}}{\verb{function(data, model, params)} defaults to \code{\link[=loglik_xy]{loglik_xy()}}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
MLE of the model parameters for the given data, same shape as
\code{params_init}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamRegrModel-f_yx"></a>}}
\if{latex}{\out{\hypertarget{method-ParamRegrModel-f_yx}{}}}
\subsection{Method \code{f_yx()}}{
Evaluates the conditional density function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamRegrModel$f_yx(t, x, params = private$params)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{t}}{value(s) at which the conditional density shall be evaluated}

\item{\code{x}}{vector of covariates}

\item{\code{params}}{model parameters to use, defaults to the fitted parameter
values}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
value(s) of the conditional density function, same shape as \code{t}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamRegrModel-F_yx"></a>}}
\if{latex}{\out{\hypertarget{method-ParamRegrModel-F_yx}{}}}
\subsection{Method \code{F_yx()}}{
Evaluates the conditional distribution function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamRegrModel$F_yx(t, x, params = private$params)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{t}}{value(s) at which the conditional distribution shall be
evaluated}

\item{\code{x}}{vector of covariates}

\item{\code{params}}{model parameters to use, defaults to the fitted parameter
values}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
value(s) of the conditional distribution function, same shape as
\code{t}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamRegrModel-F1_yx"></a>}}
\if{latex}{\out{\hypertarget{method-ParamRegrModel-F1_yx}{}}}
\subsection{Method \code{F1_yx()}}{
Evaluates the conditional quantile function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamRegrModel$F1_yx(t, x, params = private$params)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{t}}{value(s) at which the conditional quantile function shall be
evaluated}

\item{\code{x}}{vector of covariates}

\item{\code{params}}{model parameters to use, defaults to the fitted parameter
values}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
value(s) of the conditional quantile function, same shape as
\code{t}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamRegrModel-sample_yx"></a>}}
\if{latex}{\out{\hypertarget{method-ParamRegrModel-sample_yx}{}}}
\subsection{Method \code{sample_yx()}}{
Generates a new sample of response variables with the same
conditional distribution.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamRegrModel$sample_yx(x, params = private$params)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{vector of covariates}

\item{\code{params}}{model parameters to use, defaults to the fitted parameter
values}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
vector of sampled response variables, same length as \code{x}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamRegrModel-mean_yx"></a>}}
\if{latex}{\out{\hypertarget{method-ParamRegrModel-mean_yx}{}}}
\subsection{Method \code{mean_yx()}}{
Evaluates the regression function or in other terms the
expected value of Y given X=x.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamRegrModel$mean_yx(x, params = private$params)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{vector of covariates}

\item{\code{params}}{model parameters to use, defaults to the fitted parameter
values}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
value of the regression function
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamRegrModel-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ParamRegrModel-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamRegrModel$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
