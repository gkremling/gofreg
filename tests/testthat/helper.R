dummy_xymodel <- function() {
  g1 <- function(u) {return(exp(u))}
  beta <- c(1,2,3)
  sd <- 2
  n <- 100
  x <- rbind(runif(n), runif(n), rnorm(n))
  y <- rnorm(n, mean=g1(beta %*% x), sd=sd)
  model <- NormalGLM$new(g1)
  model$fit(x,y, params_init=list(beta=c(1,2,3), sd=2), inplace=TRUE)
  return(list(x=x, y=y, model=model))
}