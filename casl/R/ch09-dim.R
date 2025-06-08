#' Compute square kernel matrix for polynomial kernel.
#'
#' @param X A numeric data matrix.
#' @param d Integer degree of the polynomial.
#' @param c Numeric constant to modify the kernel.
#'
#' @return A square kernel matrix with the nrow(X) rows and columns.
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
casl_util_kernel_poly <-
function(X, d=2L, c=0)
{
  cross <- tcrossprod(X, X)
  M <- (cross + c)^d
  M
}

#' Compute square kernel matrix for a radial kernel.
#'
#' @param X A numeric data matrix.
#' @param gamma Positive tuning parameter to set kernel shape.
#'
#' @return A square kernel matrix with the nrow(X) rows and columns.
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
casl_util_kernel_radial <-
function(X, gamma=1)
{
  d <- as.matrix(stats::dist(X))^2
  M <- exp(-1 * gamma * d)
  M
}

#' Calculate normalized kernel matrix.
#'
#' @param M A raw kernel matrix.
#'
#' @return A normalized version of the kernel matrix.
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
casl_util_kernel_norm <-
function(M)
{
  n <- ncol(M)
  ones <- matrix(1 / n, ncol = n, nrow = n)
  Ms <- M - 2 * ones %*% M  + ones %*% M %*% ones
  Ms
}

#' Compute kernel version of PCA matrix.
#'
#' @param X A numeric data matrix.
#' @param k Integer number of components to return.
#' @param kfun The kernel function.
#' @param ... Other options passed to the kernel function.
#'
#' @return The nrow(X)-by-k kernel PCA matrix.
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
casl_kernel_pca <-
function(X, k=2L, kfun=casl_util_kernel_poly, ...)
{
  M <- casl_util_kernel_norm(kfun(X, ...))
  e <- irlba::partial_eigen(M, n = k)
  pc <- e$vectors %*% diag(sqrt(e$values))
  pc[]
}

#' Compute symmetric k-nearest neighbors similarity matrix.
#'
#' @param X A numeric data matrix.
#' @param k Number of nearest neighbors to consider.
#'
#' @return A square similarity matrix with nrow(X) rows & columns.
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
casl_util_knn_sim <-
function(X, k=10L)
{
  d <- as.matrix(stats::dist(X, upper = TRUE))^2
  N <- apply(d, 1L, function(v) v <= sort(v)[k + 1L])
  S <- (N + t(N) >= 2)
  diag(S) <- 0
  S
}

#' Perform spectral clustering using a similarity matrix.
#'
#' @param S A symmetric similarity matrix.
#' @param k Number of components to return.
#'
#' @return A matrix with nrow(S) rows and k columns.
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
casl_spectral_clust <-
function(S, k=1)
{
  D <- diag(apply(S, 1, sum))
  L <- D - S
  e <- eigen(L)
  Z <- e$vector[,rev(which(e$value > 1e-8))[seq_len(k)]]
  Z
}

#' Compute t-SNE probability scores.
#'
#' @param X A numeric data matrix.
#' @param perplexity Desired perplexity score for all variables.
#'
#' @return A matrix of probability densities.
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
casl_tsne_p <-
function(X, perplexity = 15)
{

  D <- as.matrix(stats::dist(X))^2
  P <- matrix(0, nrow(X), nrow(X))
  svals <- rep(1, nrow(X))

  for (i in seq_along(svals))
  {
    srange <- c(0, 100)
    tries <- 0

    for(j in seq_len(50))
    {
      Pji <- exp(-D[i, -i] / (2 * svals[i]))
      Pji <- Pji / sum(Pji)
      H <- -1 * Pji %*% log(Pji, 2)

      if (H < log(perplexity, 2))
      {
        srange[1] <- svals[i]
        svals[i] <- (svals[i] + srange[2]) / 2
      } else {
        srange[2] <- svals[i]
        svals[i] <- (svals[i] + srange[1]) / 2
      }
    }
    P[i, -i] <- Pji
  }

  return(0.5 * (P + t(P)) / sum(P))
}

#' Compute t-SNE variance values.
#'
#' @param X A numeric data matrix.
#' @param perplexity Desired perplexity score for all variables.
#' @param k Dimensionality of the output.
#' @param iter Number of iterations to perform.
#' @param rho A positive numeric learning rate.
#'
#' @return An nrow(X) by k matrix of t-SNE embeddings.
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
casl_tsne <-
function(X, perplexity=30, k=2L, iter=1000L, rho=100) {

  Y <- matrix(stats::rnorm(nrow(X) * k), ncol = k)
  P <- casl_tsne_p(X, perplexity)
  del <- matrix(0, nrow(Y), ncol(Y))

  for (inum in seq_len(iter))
  {
    num <- matrix(0, nrow(X), nrow(X))
    for (j in seq_len(nrow(X))) {
      for (k in seq_len(nrow(X))) {
        num[j, k] = 1 / (1 + sum((Y[j,] - Y[k, ])^2))
      }
    }
    diag(num) <- 0
    Q <- num / sum(num)

    stiffnesses <- 4 * (P - Q) * num
    for (i in seq_len(nrow(X)))
    {
      del[i, ] <- stiffnesses[i, ] %*% t(Y[i, ] - t(Y))
    }

    Y <- Y - rho * del
    Y <- t(t(Y) - apply(Y, 2, mean))
  }

  Y
}
