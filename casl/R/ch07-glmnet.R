#' Soft threshold function.
#'
#' @param a Numeric vector of values to threshold.
#' @param b The soft thresholded value.
#'
#' @return Numeric vector of the soft-thresholded values of a.
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
casl_util_soft_thresh <-
function(a, b)
{
  a[abs(a) <= b] <- 0
  a[a > 0] <- a[a > 0] - b
  a[a < 0] <- a[a < 0] + b
  a
}

#' Update beta vector using coordinate descent.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param lambda The penalty term.
#' @param alpha Value from 0 and 1; balance between l1/l2 penalty.
#' @param b A vector of warm start coefficients for the algorithm.
#' @param W A vector of sample weights.
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
casl_lenet_update_beta <-
function(X, y, lambda, alpha, b, W)
{
  WX <- W * X
  WX2 <- W * X^2
  Xb <- X %*% b

  for (i in seq_along(b))
  {
    Xb <- Xb - X[, i] * b[i]
    b[i] <- casl_util_soft_thresh(sum(WX[,i, drop=FALSE] *
                                    (y - Xb)),
                                  lambda*alpha)
    b[i] <- b[i] / (sum(WX2[, i]) + lambda * (1 - alpha))
    Xb <- Xb + X[, i] * b[i]
  }
  b
}

#' Compute linear elastic net using coordinate descent.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param lambda The penalty term.
#' @param alpha Value from 0 and 1; balance between l1/l2 penalty.
#' @param b Current value of the regression vector.
#' @param tol Numeric tolerance parameter.
#' @param maxit Integer maximum number of iterations.
#' @param W Vector of sample weights.
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
casl_lenet <-
function(X, y, lambda, alpha = 1, b=matrix(0, nrow=ncol(X), ncol=1),
         tol = 1e-5, maxit=50L, W=rep(1, length(y))/length(y))
{
  for (j in seq_along(lambda))
  {
    if (j > 1)
    {
      b[,j] <- b[, j-1, drop = FALSE]
    }

    # Update the slope coefficients until they converge.
    for (i in seq(1, maxit))
    {
      b_old <- b[, j]
      b[, j] <- casl_lenet_update_beta(X, y, lambda[j], alpha,
                                       b[, j], W)
      if (all(abs(b[, j] - b_old) < tol)) {
        break
      }
    }
    if (i == maxit)
    {
      warning("Function lenet did not converge.")
    }
  }
  b
}

#' Check current KKT conditions for regression vector.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param b Current value of the regression vector.
#' @param lambda The penalty term.
#'
#' @return A logical vector indicating where the KKT conditions
#' have been violated by the variables that are currently zero.
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
casl_lenet_check_kkt <-
function(X, y, b, lambda)
{
  resids <- y - X %*% b
  s <- apply(X, 2, function(xj) crossprod(xj, resids)) /
             lambda / nrow(X)

  # Return a vector indicating where the KKT conditions have been
  # violated by the variables that are currently zero.
  (b == 0) & (abs(s) >= 1)
}

#' Update beta vector using KKT conditions.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param b Current value of the regression vector.
#' @param lambda The penalty term.
#' @param active_set Logical index of the active set of variables.
#' @param maxit Integer maximum number of iterations.
#'
#' @return A list indicating the new regression vector and active set.
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
casl_lenet_update_beta_kkt <-
function(X, y, b, lambda, active_set, maxit=10000L)
{
  if (any(active_set)) {
    b[active_set, ] <- casl_lenet(X[, active_set, drop = FALSE],
                                  y, lambda, 1,
                                  b[active_set, , drop = FALSE],
                                  maxit = maxit)
  }

  kkt_violations <- casl_lenet_check_kkt(X, y, b, lambda)

  while(any(kkt_violations))
  {
    active_set <- active_set | kkt_violations
    b[active_set, ] <- casl_lenet(X[, active_set, drop = FALSE],
                                  y, lambda, 1,
                                  b[active_set, , drop = FALSE],
                                  maxit = maxit)
    kkt_violations <- casl_lenet_check_kkt(X, y, b, lambda)
  }

  list(b=b, active_set=active_set)
}

#' Apply coordinate descent screening rules.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param lambda The penalty term.
#' @param b A matrix of warm start coefficients for the algorithm.
#' @param maxit Integer maximum number of iterations.
#'
#' @return Named list of parameters for use in the lenet algorithm.
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
casl_lenet_screen <-
function(X, y, lambda, b=matrix(0, nrow=ncol(X),
                                ncol=length(lambda)),
         maxit = 10000L)
{
  a0 <- mean(y)
  y <- y - mean(y)

  X <- scale(X)
  center <- attributes(X)[['scaledcenter']]
  scale <- attributes(X)[['scaledscale']]

  keep_cols <- which(scale > 1e-10)
  X <- X[, keep_cols]
  center <- center[keep_cols]
  scale <- scale[keep_cols]

  active_set <- b[, 1] != 0
  lsu <- casl_lenet_update_beta_kkt(X, y, b[, 1, drop=FALSE],
                                    lambda[1], active_set, maxit)
  b[, 1] <- lsu$b

  for (i in seq_along(lambda)[-1])
  {
    lsu <- casl_lenet_update_beta_kkt(X, y, b[, i - 1L,
                                              drop=FALSE],
                                      lambda[i], lsu$active_set,
                                      maxit)
    b[, i] <- lsu$b
  }

  list(b=b, a0=a0, center=center, scale=scale,
       keep_cols=keep_cols)
}

#' Compute generalized linear elastic net with coordinate descent.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param lambda The penalty term.
#' @param alpha Value from 0 and 1; balance between l1/l2 penalty.
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
casl_glenet <-
function(X, y, lambda, alpha=1, family=stats::binomial(),
         maxit=10000L, tol=1e-5)
{
  b <- matrix(0, nrow=ncol(X), ncol=length(lambda))

  if (!is.null(colnames(X)))
  {
    rownames(b) <- colnames(X)
  }

  for (j in seq_along(lambda))
  {
    if (j > 1L)
    {
      b[, j] <- b[, j - 1L]
    }
    for (i in seq_len(maxit))
    {
      eta <- X %*% b[, j]
      g <- family$linkinv(eta)
      gprime <- family$mu.eta(eta)
      z <- eta + (y - g) / gprime
      W <- as.vector(gprime^2 / family$variance(g)) / nrow(X)

      old_b <- b[,j]
      b[, j] <- casl_lenet_update_beta(X, z, lambda[j], alpha,
                                       b[, j, drop = FALSE], W)

      if (max(abs(b[, j] - old_b)) < tol)
      {
        break
      }
    }
    if (i == maxit)
    {
      warning("Function casl_glenet did not converge.")
    }
  }
  b
}
