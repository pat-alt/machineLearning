stochastic_gradient_descent <- function(X,y,eta,N=1000,w_init=NULL,loss="exp") {
  # Initialization:
  n <- nrow(X)
  d <- ncol(X)
  if (is.null(w_init)) {
    w <- matrix(rep(0,d))
  } else {
    w <- matrix(w_init)
  }
  w_avg <- 1/N * w
  iter <- 1
  # Surrogate loss:
  if (loss=="exp") {
    l <- function(X,y,w) {
      exp(-1 * crossprod(X,w) * y)
    }
    grad <- function(X,y,w) {
      (-1) * X %*% y %*% exp((-1) * crossprod(X,w) * y)
    }
  } else if (loss=="hinge") {
    l <- function(X,y,w) {
      pmax(0,1+crossprod(X,w) * y)
    }
    grad <- function(X,y,w) {
      ifelse(crossprod(X,w) * y<=-1,0,1)
    }
  } else if (loss=="log") {
    l <- function(X,y,w) {
      x <- crossprod(X,w) * y
      log2(1 + exp(x))
    }
    grad <- function(X,y,w) {
      x <- crossprod(X,w) * y
      exp(x) / (exp(x) * log(2) + log(2))
    }
  }
  # Descent:
  while (iter<=N) {
    t <- sample(1:n,1)
    X_t <- matrix(X[t,])
    y_t <- matrix(y[t])
    v_t <- grad(X_t,y_t,w)
    # Update:
    w <- w - eta * v_t
    w_avg <- w_avg + 1/N * w
    iter <- iter + 1
  }
  return(w)
}