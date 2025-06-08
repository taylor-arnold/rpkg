#' Compute OLS estimate using SVD decomposition.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
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
casl_ols_svd  <-
function(X, y)
{
  svd_output <- svd(X)
  U <- svd_output$u
  Sinv <- diag(1 / svd_output$d)
  V <- svd_output$v
  pseudo_inv <- V %*% Sinv %*% t(U)
  betahat <- pseudo_inv %*% y
  betahat
}

#' Compute OLS estimate using the Cholesky decomposition.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
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
casl_ols_chol <-
function(X, y)
{
  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  L <- chol(XtX)

  betahat <- forwardsolve(t(L), backsolve(L, Xty))
  betahat
}

#' Compute OLS estimate using the orthogonal projection.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
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
casl_ols_orth_proj <-
function(X, y)
{
  qr_obj <- qr(X)
  Q <- qr.Q(qr_obj)
  R <- qr.R(qr_obj)
  Qty <- crossprod(Q, y)

  betahat <- backsolve(R, Qty)
  betahat
}
