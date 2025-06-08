#' Logistic regression using the Newton-Ralphson method.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param maxit Integer maximum number of iterations.
#' @param tol Numeric tolerance parameter.
#'
#' @return Regression vector beta of length ncol(X).
#'
#' @author Taylor Arnold, Michael Kane, Bryan Lewis.
#'
#' @references
#'
#' Taylor Arnold, Michael Kane, and Bryan Lewis.
#' \emph{A Computational Approach to Statistical Learning}.
#' Chapman & Hall/CRC Texts in Statistical Science, 2019.
#'
#' @export
casl_glm_nr_logistic <-
function(X, y, maxit=25L, tol=1e-10)
{
  beta <- rep(0,ncol(X))
  for(j in seq(1L, maxit))
  {
    b_old <- beta
    p <- 1 / (1 + exp(- X %*% beta))
    W <- as.numeric(p * (1 - p))
    XtX <- crossprod(X, diag(W) %*% X)
    score <- t(X) %*% (y - p)
    delta <- solve(XtX, score)
    beta <- beta + delta
    if(sqrt(crossprod(beta - b_old)) < tol) break
  }
  beta
}

#' Solve generalized linear models with Newton-Ralphson method.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param mu_fun Function from eta to the expected value.
#' @param var_fun Function from mean to variance.
#' @param maxit Integer maximum number of iterations.
#' @param tol Numeric tolerance parameter.
#'
#' @return Regression vector beta of length ncol(X).
#'
#' @author Taylor Arnold, Michael Kane, Bryan Lewis.
#'
#' @references
#'
#' Taylor Arnold, Michael Kane, and Bryan Lewis.
#' \emph{A Computational Approach to Statistical Learning}.
#' Chapman & Hall/CRC Texts in Statistical Science, 2019.
#'
#' @export
casl_glm_nr <-
function(X, y, mu_fun, var_fun, maxit=25, tol=1e-10)
{
  beta <- rep(0,ncol(X))
  for(j in seq_len(maxit))
  {
    b_old <- beta
    eta   <- X %*% beta
    mu    <- mu_fun(eta)
    W     <- as.numeric(var_fun(mu))
    XtX   <- crossprod(X, diag(W) %*% X)
    score <- t(X) %*% (y - mu)
    delta <- solve(XtX, score)
    beta  <- beta + delta
    if(sqrt(crossprod(beta - b_old)) < tol) break
  }
  beta
}

#' Solve generalized linear models with Newton-Ralphson method.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param family Instance of an R `family` object.
#' @param maxit Integer maximum number of iterations.
#' @param tol Numeric tolerance parameter.
#'
#' @return Regression vector beta of length ncol(X).
#'
#' @author Taylor Arnold, Michael Kane, Bryan Lewis.
#'
#' @references
#'
#' Taylor Arnold, Michael Kane, and Bryan Lewis.
#' \emph{A Computational Approach to Statistical Learning}.
#' Chapman & Hall/CRC Texts in Statistical Science, 2019.
#'
#' @export
casl_glm_irwls <-
function(X, y, family, maxit=25, tol=1e-10)
{
  beta <- rep(0,ncol(X))
  for(j in seq_len(maxit))
  {
    b_old <- beta
    eta <- X %*% beta
    mu <- family$linkinv(eta)
    mu_p <- family$mu.eta(eta)
    z <- eta + (y - mu) / mu_p
    W <- as.numeric(mu_p^2 / family$variance(mu))
    XtX <- crossprod(X, diag(W) %*% X)
    Xtz <- crossprod(X, W * z)
    beta <- solve(XtX, Xtz)
    if(sqrt(crossprod(beta - b_old)) < tol) break
  }
  beta
}

#' Generalized linear models with Newton-Ralphson and QR.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param family Instance of an R \code{family} object.
#' @param maxit Integer maximum number of iterations.
#' @param tol Numeric tolerance parameter.
#'
#' @return Regression vector beta of length ncol(X).
#'
#' @author Taylor Arnold, Michael Kane, Bryan Lewis.
#'
#' @references
#'
#' Taylor Arnold, Michael Kane, and Bryan Lewis.
#' \emph{A Computational Approach to Statistical Learning}.
#' Chapman & Hall/CRC Texts in Statistical Science, 2019.
#'
#' @export
casl_glm_irwls_qr <-
function(X, y, family, maxit=25, tol=1e-10)
{
  s <- eta <- 0
  QR <- qr(X)
  Q  <- qr.Q(QR)
  R  <- qr.R(QR)

  for(j in seq_len(maxit))
  {
    s_old <- s
    mu    <- family$linkinv(eta)
    mu_p  <- family$mu.eta(eta)
    z     <- eta + (y - mu) / mu_p
    W     <- as.numeric(mu_p^2 / family$variance(mu))
    wmin  <- min(W)
    if(wmin < sqrt(.Machine$double.eps))
      warning("Tiny weights encountered")
    C     <- chol(crossprod(Q, W*Q))
    s     <- forwardsolve(t(C), crossprod(Q, W*z))
    s     <- backsolve(C, s)
    eta   <- Q %*% s
    if(sqrt(crossprod(s - s_old)) < tol) break
  }
  beta <- backsolve(R, crossprod(Q, eta))
  beta
}
