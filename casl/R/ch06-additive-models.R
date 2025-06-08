#' Nonlinear regression using kernel regression.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param X_new Numeric data matrix of prediction locations.
#' @param h The kernel bandwidth.
#'
#' @return A vector of predictions; one for each row in X_new.
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
casl_nlm_kernel <-
function(X, y, X_new, h)
{
  apply(X_new, 1, function(v)
  {
    dists <- apply((t(X) - v)^2, 2, sum)
    W <- diag(casl_util_kernel_epan(dists, h = h))
    beta <- solve(crossprod(X, W %*% X),
                  crossprod(W %*% X, y))
    v %*% beta
  })
}

#' Nonlinear regression using polynomial regression.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param X_new Numeric data matrix of prediction locations.
#' @param k Order of the polynomial.
#'
#' @return A vector of predictions; one for each row in X_new.
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
casl_nlm_poly <-
function(X, y, X_new, k)
{
  Z <- stats::poly(X, degree = k)
  Z_new <- cbind(1, stats::predict(Z, X_new))
  Z <- cbind(1, Z)
  beta <- stats::coef(stats::lm.fit(Z, y))
  y_hat <- Z_new %*% beta
  y_hat
}

#' Fit linear additive model using backfit algorithm.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param maxit Integer maximum number of iterations.
#'
#' @return A list of smoothing spline objects; one for each column
#' in X.
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
casl_am_backfit <-
function(X, y, maxit=10L)
{
  p <- ncol(X)
  id <- seq_len(nrow(X))
  alpha <- mean(y)
  f <- matrix(0, ncol = p, nrow = nrow(X))
  models <- vector("list", p + 1L)

  for (i in seq_len(maxit))
  {
    for (j in seq_len(p))
    {
      p_resid <- y - alpha - apply(f[, -j], 1L, sum)
      id <- order(X[,j])
      models[[j]] <- stats::smooth.spline(X[id,j], p_resid[id])
      f[,j] <- stats::predict(models[[j]], X[,j])$y
    }
    alpha <- mean(y - apply(f, 1L, sum))
  }

  models[[p + 1L]] <- alpha
  return(models)
}

#' Predict values from linear additive model.
#'
#' @param models A list of smoothing spline objects; one for each
#' column in the training data matrix.
#' @param X_new Numeric data matrix of prediction locations.
#'
#' @return A list of smoothing spline objects; one for each column
#' in X.
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
casl_am_predict <-
function(models, X_new)
{
  p <- ncol(X_new)
  f <- matrix(0, ncol = p, nrow = nrow(X_new))
  for (j in seq_len(p))
  {
    f[,j] <- stats::predict(models[[j]], X_new[,j])$y
  }

  y <- apply(f, 1L, sum) + models[[p + 1L]]

  list(y=y, f=f)
}

#' Compute additive model design matrix.
#'
#' @param X A numeric data matrix.
#' @param order Order of the polynomial.
#' @param nknots Number of knots in the power basis.
#'
#' @return A list of smoothing spline objects; one for each column
#' in X.
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
casl_am_ls_basis <-
function(X, order, nknots)
{
  Z <- matrix(rep(1, nrow(X)), ncol = 1L)
  for (j in seq_len(ncol(X)))
  {
    knots <- seq(min(X[, j]), max(X[, j]),
                 length.out = nknots + 2L)
    knots <- knots[-c(1, length(knots))]
    Z2 <- casl_nlm1d_trunc_power_x(X[, j], knots,
                                   order = order)[, -1L]
    Z <- cbind(Z, Z2)
  }
  Z
}

#' Compute penalty matrix for penalized additive models.
#'
#' @param Z A numeric data matrix.
#' @param lambda Vector of penalty terms, with one value per
#' column in the original data matrix.
#'
#' @return The penalty matrix for the additive model.
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
casl_am_ls_omega <-
function(Z, lambda)
{
  d <- (ncol(Z) - 1L) / length(lambda)
  omega <- Matrix::Matrix(0, ncol = ncol(Z), nrow = ncol(Z))
  Matrix::diag(omega)[-1L] <- rep(sqrt(lambda), each = d)
  omega
}

#' Compute predicted values for penalized additive models.
#'
#' @param Z A numeric data matrix.
#' @param beta_hat Estimated regression vector.
#' @param p Number of columns in original data matrix.
#'
#' @return Predicted values, f_j, from the additive model.
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
casl_am_ls_f <-
function(Z, beta_hat, p)
{
  d <- (ncol(Z) - 1L) / p
  f <- matrix(NA_real_, ncol = p + 1L, nrow = nrow(Z))
  for (j in seq_len(p))
  {
    id <- 1L + seq_len(d) + d * (j - 1L)
    f[, j] <- Z[, id] %*% beta_hat[id]
  }
  f[, p + 1L] <- beta_hat[1L]
  f
}

#' Compute standard errors for penalized additive models.
#'
#' @param Z A numeric data matrix.
#' @param omega A penalty matrix.
#' @param beta_hat Estimated regression vector.
#' @param s_hat Estimated noise variance.
#' @param p Number of columns in original data matrix.
#'
#' @return A matrix of standard errors for the predicted values,
#' f_j, from the additive model.
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
casl_am_ls_f_se <-
function(Z, omega, beta_hat, s_hat, p)
{
  d <- (ncol(Z) - 1L) / p
  f_se <- matrix(NA_real_, ncol = p + 1L,
                           nrow = nrow(Z))
  ZtZ <- Matrix::crossprod(Z, Z)
  B <- solve(ZtZ + Matrix::crossprod(omega, omega),
             Matrix::t(Z))
  beta_var <- Matrix::tcrossprod(B) * s_hat
  A <- Z %*% beta_var
  f_se <- apply((Z %*% beta_var) * Z, 1, sum)
  f_se
}

#' Additive model using truncated power basis.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param lambda Vector of penalty terms, with one value per
#' column in the original data matrix.
#' @param order Order of the polynomial.
#' @param nknots Number of knots in the piecewise polynomial.
#'
#' @return A vector of predictions; one for each row in X_new.
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
casl_am_ls <-
function(X, y, lambda, order=3L, nknots=5L)
{
  # get basis and penalty matricies
  Z <- casl_am_ls_basis(X, order=order, nknots=nknots)
  omega <- casl_am_ls_omega(Z, lambda)

  # compute predicted beta and f_j values
  beta_hat <- Matrix::solve(Matrix::crossprod(Z, Z) +
                            Matrix::crossprod(omega, omega),
                            Matrix::crossprod(Z, y))
  f <- casl_am_ls_f(Z, beta_hat, p = ncol(X))

  # fitted values for y and sigma^2
  y_hat <- apply(f, 1L, sum)
  s_hat <- sum((y - y_hat)^2) / (nrow(Z) - ncol(Z))

  # get standard errors for f_j
  f_se <- casl_am_ls_f_se(Z, omega, beta_hat, s_hat,
                          p = ncol(X))

  # return list of values
  return(list(y_hat=y_hat, f=f, f_se=f_se))
}
