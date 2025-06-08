#'  Compute ridge regression vector.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param lambda_vals A sequence of penalty terms.
#'
#' @return A matrix of regression vectors with ncol(X) columns
#' and length(lambda_vals) rows.
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
casl_lm_ridge <-
function(X, y, lambda_vals)
{
  svd_obj <- svd(X)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  k <- length(lambda_vals)

  ridge_beta <- matrix(NA_real_, nrow = k, ncol = ncol(X))
  for (j in seq_len(k))
  {
    D <- diag(svals / (svals^2 + lambda_vals[j]))
    ridge_beta[j,] <- V %*% D %*% t(U) %*% y
  }

  ridge_beta
}

#' Compute PCA regression vector.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param k Number of components to use in the fit.
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
casl_lm_pca <-
function(X, y, k)
{
  svd_obj <- svd(X)
  U <- svd_obj$u
  V <- svd_obj$v
  dvals <- rep(0, ncol(X))
  dvals[seq_len(k)] <- 1 / svd_obj$d[seq_len(k)]

  D <- diag(dvals)
  pca_beta <- V %*% D %*% t(U) %*% y

  pca_beta
}

#' Root mean squared error of a prediction.
#'
#' @param y A vector of responses to predict.
#' @param y_hat A vector of predicted values.
#' @param by Optional categories to stratify results.
#'
#' @return A table of the root mean squared error for each
#' group in "by".
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
casl_util_rmse <-
function(y, y_hat, by)
{
  if (missing(by))
  {
    ret <- sqrt(mean((y - y_hat)^2))
  } else {
    ret <- sqrt(tapply((y - y_hat)^2, by, mean))
  }

  ret
}
