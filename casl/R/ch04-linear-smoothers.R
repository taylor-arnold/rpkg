#' Apply one-dimensional polynomial regression.
#'
#' @param x Numeric vector of the predictor variables.
#' @param y Numeric vector of the responses.
#' @param n An integer giving the order to the polynomial.
#'
#' @return A length n numeric vector of coefficients.
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
casl_nlm1d_poly <-
function(x, y, n=1L)
{
  Z <- cbind(1/length(x), stats::poly(x, n=n))
  beta_hat <- crossprod(Z, y)
  beta_hat
}

#' Predict values from one-dimensional polynomial regression.
#'
#' @param beta A length n numeric vector of coefficients.
#' @param x Numeric vector of the original predictor variables.
#' @param x_new A vector of data values at which to estimate.
#'
#' @return A vector of predictions with the same length as x_new.
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
casl_nlm1d_poly_predict <-
function(beta, x, x_new)
{
  pobj <- stats::poly(x, n=(length(beta) - 1L))
  Z_new <- cbind(1.0, stats::predict(pobj, x_new))
  y_hat <- Z_new %*% beta
  y_hat
}

#' Evaluate the Epanechnikov kernel function.
#'
#' @param x Numeric vector of points to evaluate the function at.
#' @param h A numeric value giving the bandwidth of the kernel.
#'
#' @return A vector of values with the same length as x.
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
casl_util_kernel_epan <-
function(x, h=1)
{
  x <- x / h
  ran <- as.numeric(abs(x) <= 1)
  val <- (3/4) * ( 1 - x^2 ) * ran
  val
}

#' Apply one-dimensional (Epanechnikov) kernel regression.
#'
#' @param x Numeric vector of the original predictor variables.
#' @param y Numeric vector of the responses.
#' @param x_new A vector of data values at which to estimate.
#' @param h A numeric value giving the bandwidth of the kernel.
#'
#' @return A vector of predictions for each value in x_new.
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
casl_nlm1d_kernel <-
function(x, y, x_new, h=1)
{
  sapply(x_new, function(v)
    {
      w <- casl_util_kernel_epan(abs(x - v), h=h)
      yhat <- sum(w * y) / sum(w)
      yhat
    })
}

#' Apply one-dimensional local regression.
#'
#' @param x Numeric vector of the original predictor variables.
#' @param y Numeric vector of the responses.
#' @param x_new A vector of data values at which to estimate.
#' @param h A numeric value giving the bandwidth of the kernel.
#'
#' @return A vector of predictions for each value in x_new.
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
casl_nlm1d_local <-
function(x, y, x_new, h=1) {
  X <- cbind(1, x)
  sapply(x_new, function(v) {
    w <- casl_util_kernel_epan(abs(x - v), h=h)
    beta <- solve(t(X) %*% diag(w) %*% X,
                  t(X) %*% diag(w) %*% y)
    yhat <- cbind(1, v) %*% beta
    yhat
  })
}

#' One-dimensional regression using a truncated power basis.
#'
#' @param x Numeric vector of values.
#' @param knots Numeric vector of knot points.
#' @param order Integer order of the polynomial fit.
#'
#' @return A matrix with one row for each element of x and
#' (1 + length(knots) + order) columns.
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
casl_nlm1d_trunc_power_x <-
function(x, knots, order=1L)
{
  M <- order
  P <- length(knots)
  n <- length(x)
  k <- knots

  X <- matrix(0, ncol=(1L + M + P), nrow = n)
  for (j in seq(0L, M))
  {
    X[, j+1] <- x^j
  }
  for (j in seq(1L, P))
  {
    X[, j + M + 1L] <- (x - k[j])^M * as.numeric(x > k[j])
  }

  X
}

#' Natural cubic spline basis.
#'
#' @param x Numeric vector of values.
#' @param knots Numeric vector of knot points.
#'
#' @return A matrix with one row for each element of x and
#' length(knots) columns.
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
casl_nlm1d_nat_spline_x <-
function(x, knots)
{
  P <- length(knots)
  n <- length(x)
  k <- knots

  d <- function(z, j)
  {
    out <- (x - k[j])^3 * as.numeric(x > k[j])
    out <- out - (x - k[P])^3 * as.numeric(x > k[P])
    out <- out / (k[P] - k[j])
    out
  }

  X <- matrix(0, ncol=P, nrow=n)
  X[, 1L] <- 1
  X[, 2L] <- x
  for (j in seq(1L, (P-2L)))
  {
    X[, j + 2L] <- d(x, j) - d(x, P - 1L)
  }

  X
}

#' B-spline basis.
#'
#' @param x Numeric vector of values.
#' @param knots Numeric vector of knot points.
#' @param order Integer order of the polynomial fit.
#'
#' @return A matrix with one row for each element of x and
#' (length(knots) + order) columns.
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
casl_nlm1d_bspline_x <-
function(x, knots, order=1L)
{
  P <- length(knots)
  k <- knots
  k <- c(rep(min(x), order + 1L), k,
         rep(max(x), order + 1L))

  # Note this is NOT vectorized over x
  b <- function(x, j, m)
  {
    if (m == 0L) {
      return(as.numeric(k[j] <= x & x <= k[j + 1L]))
    } else {
      r <- (x - k[j]) / (k[j + m] - k[j]) *
              b(x, j, m - 1)
      if (is.nan(r)) r <- 0
      r <- r + (k[j + m + 1] - x) /
              (k[j + m + 1] - k[j + 1]) *
              b(x, j + 1, m - 1)
      if (is.nan(r)) r <- 0
      r
    }
  }

  X <- matrix(0, ncol=(P + order), nrow=length(x))
  for (j in seq(1L, (P + order)))
  {
    for (i in seq(1L, length(x)))
    {
      X[i, j] <- b(x[i], j, order)
    }
  }

  X
}
