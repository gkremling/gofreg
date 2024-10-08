% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GOFTest.R
\name{GOFTest}
\alias{GOFTest}
\title{Goodness-of-fit test for parametric regression}
\description{
This class implements functions to calculate the test statistic
for the original data as well as the statistics for bootstrap samples. It
also offers the possibility to compute the corresponding bootstrap
p-value.
}
\examples{
# Create an example dataset
n <- 100
x <- cbind(runif(n), rbinom(n, 1, 0.5))
model <- NormalGLM$new()
y <- model$sample_yx(x, params=list(beta=c(2,3), sd=1))
data <- dplyr::tibble(x = x, y = y)

# Fit the correct model
model$fit(data, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)

# Calculate the bootstrap p-value and plot the corresponding processes
goftest <- GOFTest$new(data, model, test_stat = CondKolmY$new(), nboot = 10)
goftest$get_pvalue()
goftest$plot_procs()

# Fit a wrong model
model2 <- NormalGLM$new(linkinv = function(u) {u+10})
model2$fit(data, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)

# Calculate the bootstrap p-value and plot the corresponding processes
goftest2 <- GOFTest$new(data, model2, test_stat = CondKolmY$new(), nboot = 10)
goftest2$get_pvalue()
goftest2$plot_procs()
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GOFTest-new}{\code{GOFTest$new()}}
\item \href{#method-GOFTest-get_stat_orig}{\code{GOFTest$get_stat_orig()}}
\item \href{#method-GOFTest-get_stats_boot}{\code{GOFTest$get_stats_boot()}}
\item \href{#method-GOFTest-get_pvalue}{\code{GOFTest$get_pvalue()}}
\item \href{#method-GOFTest-plot_procs}{\code{GOFTest$plot_procs()}}
\item \href{#method-GOFTest-clone}{\code{GOFTest$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GOFTest-new"></a>}}
\if{latex}{\out{\hypertarget{method-GOFTest-new}{}}}
\subsection{Method \code{new()}}{
Initialize an instance of class \link{GOFTest}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GOFTest$new(
  data,
  model_fitted,
  test_stat,
  nboot,
  resample = resample_param,
  loglik = loglik_xy
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{\code{data.frame()} containing the data}

\item{\code{model_fitted}}{object of class \link{ParamRegrModel} with fitted
parameters}

\item{\code{test_stat}}{object of class \link{TestStatistic}}

\item{\code{nboot}}{number of bootstrap iterations}

\item{\code{resample}}{\verb{function(data, model)} used to resample data in
bootstrap iterations, defaults to \code{\link[=resample_param]{resample_param()}}}

\item{\code{loglik}}{\verb{function(data, model, params)} negative log-likelihood
function used to fit model to resampled data in bootstrap iterations,
defaults to \code{\link[=loglik_xy]{loglik_xy()}}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a new instance of the class
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GOFTest-get_stat_orig"></a>}}
\if{latex}{\out{\hypertarget{method-GOFTest-get_stat_orig}{}}}
\subsection{Method \code{get_stat_orig()}}{
Calculates the test statistic for the original data and
model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GOFTest$get_stat_orig()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
object of class \link{TestStatistic}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GOFTest-get_stats_boot"></a>}}
\if{latex}{\out{\hypertarget{method-GOFTest-get_stats_boot}{}}}
\subsection{Method \code{get_stats_boot()}}{
Calculates the test statistics for the resampled data and
corresponding models.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GOFTest$get_stats_boot()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
vector of length \code{nboot} containing objects of class
\link{TestStatistic}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GOFTest-get_pvalue"></a>}}
\if{latex}{\out{\hypertarget{method-GOFTest-get_pvalue}{}}}
\subsection{Method \code{get_pvalue()}}{
Calculates the bootstrap p-value for the given model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GOFTest$get_pvalue()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
p-value for the null hypothesis that \code{y} was generated according
to \code{model}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GOFTest-plot_procs"></a>}}
\if{latex}{\out{\hypertarget{method-GOFTest-plot_procs}{}}}
\subsection{Method \code{plot_procs()}}{
Plots the processes underlying the bootstrap test statistics
(gray) and the original test statistic (red)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GOFTest$plot_procs(
  title = sprintf("Test Statistic: \%s, p-value: \%s", class(private$test_stat)[1],
    self$get_pvalue()),
  subtitle = ggplot2::waiver(),
  color_boot = "gray40",
  color_orig = "red",
  x_lab = "plot.x",
  y_lab = "plot.y"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{title}}{text to be displayed as title of the plot; defaults to "Test
statistic: xxx, p-value: xxx"}

\item{\code{subtitle}}{text to be displayed as subtitle of the plot; default is
no subtitle}

\item{\code{color_boot}}{color used to plot bootstrap test statistics; default
is "red"}

\item{\code{color_orig}}{color used to plot original test statistic; default is
"gray40"}

\item{\code{x_lab}}{label to use for the x-axis; default is "plot.x"}

\item{\code{y_lab}}{label to use for the y-axis; default is "plot.y"}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The object (\code{self}), allowing for method chaining.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GOFTest-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GOFTest-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GOFTest$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
