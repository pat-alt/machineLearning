# Stochastic gradient descent: ----
linear_classifier <- function(X,y,eta=0.001,n_iter=1000,w_init=NULL,loss="exp",save_steps=FALSE) {
  # Initialization:
  n <- nrow(X)
  d <- ncol(X)
  if (is.null(w_init)) {
    w <- matrix(rep(0,d))
  } else {
    w <- matrix(w_init)
  }
  if (save_steps) {
    steps <- data.table(iter=0, w=c(w), d=1:d)
  } else {
    steps <- NA
  }
  w_avg <- 1/n_iter * w
  iter <- 1
  # Surrogate loss:
  if (loss=="exp") {
    l <- function(X,y,w) {
      exp(-1 * crossprod(X,w) %*% y)
    }
    grad <- function(X,y,w) {
      (-1) * X %*% y %*% exp((-1) * crossprod(X,w) %*% y)
    }
  } else if (loss=="hinge") {
    l <- function(X,y,w) {
      x <- (-1) * crossprod(X,w) * y
      pmax(0,1 + x)
    }
    grad <- function(X,y,w) {
      x <- (-1) * crossprod(X,w) * y
      X %*% ifelse(crossprod(X,w) * y<=1,-y,0) 
    }
  } else if (loss=="log") {
    l <- function(X,y,w) {
      x <- crossprod(X,w) * y
      log2(1 + exp(x))
    }
    grad <- function(X,y,w) {
      x <- (-1) * crossprod(X,w) * y
      (-1) * X %*% y %*% (exp(x) / (exp(x) * log(2) + log(2)))
    }
  }
  # Descent:
  while (iter<=n_iter) {
    t <- sample(1:n,1)
    X_t <- matrix(X[t,])
    y_t <- matrix(y[t])
    v_t <- grad(X_t,y_t,w)
    # Update:
    w <- w - eta * v_t
    if (!is.na(w) & is.finite(w)) {
      w_avg <- w_avg + 1/n_iter * w
    }
    if (save_steps) {
      steps <- rbind(steps, data.table(iter=iter, w=c(w), d=1:d))
    }
    iter <- iter + 1
  }
  output <- list(
    X = X,
    y = matrix(y),
    coefficients = w_avg,
    eta = eta,
    n_iter = n_iter, 
    loss = loss,
    steps = steps
  )
  class(output) <- "linear_classifier"
  return(output)
}

# Methods: ----
print.linear_classifier <- function(linear_classifier) {
  print("Coefficients:")
  print(linear_classifier$coefficients)
}
print <- function(linear_classifier) {
  UseMethod("print")
}

predict.linear_classifier <- function(linear_classifier, newdata=NULL, discrete=TRUE) {
  if (!is.null(newdata)) {
    fitted <- newdata %*% linear_classifier$coefficients
  } else {
    fitted <- linear_classifier$X %*% linear_classifier$coefficients
  }
  if (discrete) {
    fitted <- sign(fitted)
  }
  return(fitted)
}
predict <- function(linear_classifier, newdata=NULL, discrete=TRUE) {
  UseMethod("predict")
}