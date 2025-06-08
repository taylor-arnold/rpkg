#' Create list of weights to describe a dense neural network.
#'
#' @param sizes A vector giving the size of each layer, including
#' the input and output layers.
#'
#' @return A list containing initialized weights and biases.
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
casl_nn_make_weights <-
function(sizes)
{
  L <- length(sizes) - 1L
  weights <- vector("list", L)
  for (j in seq_len(L))
  {
    w <- matrix(stats::rnorm(sizes[j] * sizes[j + 1L]),
                ncol = sizes[j],
                nrow = sizes[j + 1L])
    weights[[j]] <- list(w=w,
                         b=stats::rnorm(sizes[j + 1L]))
  }
  weights
}


#' Apply a rectified linear unit (ReLU) to a vector/matrix.
#'
#' @param v A numeric vector or matrix.
#'
#' @return The original input with negative values truncated to zero.
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
casl_util_ReLU <-
function(v)
{
  v[v < 0] <- 0
  v
}

#' Apply derivative of the rectified linear unit (ReLU).
#'
#' @param v A numeric vector or matrix.
#'
#' @return Sets positive values to 1 and negative values to zero.
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
casl_util_ReLU_p <-
function(v)
{
  p <- v * 0
  p[v > 0] <- 1
  p
}

#' Derivative of the mean squared error (MSE) function.
#'
#' @param y A numeric vector of responses.
#' @param a A numeric vector of predicted responses.
#'
#' @return Returned current derivative the MSE function.
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
casl_util_mse_p <-
function(y, a)
{
  (a - y)
}

#' Apply forward propagation to a set of NN weights and biases.
#'
#' @param x A numeric vector representing one row of the input.
#' @param weights A list created by casl_nn_make_weights.
#' @param sigma The activation function.
#'
#' @return A list containing the new weighted responses (z) and
#' activations (a).
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
casl_nn_forward_prop <-
function(x, weights, sigma)
{
  L <- length(weights)
  z <- vector("list", L)
  a <- vector("list", L)
  for (j in seq_len(L))
  {
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    z[[j]] <- weights[[j]]$w %*% a_j1 + weights[[j]]$b
    a[[j]] <- if (j != L) sigma(z[[j]]) else z[[j]]
  }

  list(z=z, a=a)
}

#' Apply backward propagation algorithm.
#'
#' @param x A numeric vector representing one row of the input.
#' @param y A numeric vector representing one row of the response.
#' @param weights A list created by casl_nn_make_weights.
#' @param f_obj Output of the function casl_nn_forward_prop.
#' @param sigma_p Derivative of the activation function.
#' @param f_p Derivative of the loss function.
#'
#' @return A list containing the new weighted responses (z) and
#' activations (a).
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
casl_nn_backward_prop <-
function(x, y, weights, f_obj, sigma_p, f_p)
{
  z <- f_obj$z; a <- f_obj$a
  L <- length(weights)
  grad_z <- vector("list", L)
  grad_w <- vector("list", L)
  for (j in rev(seq_len(L)))
  {
    if (j == L)
    {
      grad_z[[j]] <- f_p(y, a[[j]])
    } else {
      grad_z[[j]] <- (t(weights[[j + 1]]$w) %*%
                      grad_z[[j + 1]]) * sigma_p(z[[j]])
    }
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    grad_w[[j]] <- grad_z[[j]] %*% t(a_j1)
  }

  list(grad_z=grad_z, grad_w=grad_w)
}

#' Apply stochastic gradient descent (SGD) to estimate NN.
#'
#' @param X A numeric data matrix.
#' @param y A numeric vector of responses.
#' @param sizes A numeric vector giving the sizes of layers in
#' the neural network.
#' @param epochs Integer number of epochs to computer.
#' @param eta Positive numeric learning rate.
#' @param weights Optional list of starting weights.
#'
#' @return A list containing the trained weights for the network.
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
casl_nn_sgd <-
function(X, y, sizes, epochs, eta, weights=NULL)
{
  if (is.null(weights))
  {
    weights <- casl_nn_make_weights(sizes)
  }

  for (epoch in seq_len(epochs))
  {
    for (i in seq_len(nrow(X)))
    {
      f_obj <- casl_nn_forward_prop(X[i,], weights,
                                    casl_util_ReLU)
      b_obj <- casl_nn_backward_prop(X[i,], y[i,], weights,
                                     f_obj, casl_util_ReLU_p,
                                     casl_util_mse_p)

      for (j in seq_along(b_obj))
      {
        weights[[j]]$b <- weights[[j]]$b -
                            eta * b_obj$grad_z[[j]]
        weights[[j]]$w <- weights[[j]]$w -
                            eta * b_obj$grad_w[[j]]
      }
    }
  }

  weights
}

#' Predict values from a training neural network.
#'
#' @param weights List of weights describing the neural network.
#' @param X_test A numeric data matrix for the predictions.
#'
#' @return A matrix of predicted values.
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
casl_nn_predict <-
function(weights, X_test)
{

  p <- length(weights[[length(weights)]]$b)
  y_hat <- matrix(0, ncol = p, nrow = nrow(X_test))
  for (i in seq_len(nrow(X_test)))
  {
    a <- casl_nn_forward_prop(X_test[i,], weights,
                              casl_util_ReLU)$a
    y_hat[i, ] <- a[[length(a)]]
  }

  y_hat
}

#' Perform a gradient check for the dense NN code.
#'
#' @param X A numeric data matrix.
#' @param y A numeric vector of responses.
#' @param weights List of weights describing the neural network.
#' @param h Positive numeric bandwidth to test.
#'
#' @return The largest difference between the empirical and analytic
#' gradients of the weights and biases.
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
casl_nn_grad_check <-
function(X, y, weights, h=0.0001)
{
  max_diff <- 0
  for (level in seq_along(weights))
  {
    for (id in seq_along(weights[[level]]$w))
    {
      grad <- rep(0, nrow(X))
      for (i in seq_len(nrow(X)))
      {
        f_obj <- casl_nn_forward_prop(X[i, ], weights,
                                      casl_util_ReLU)
        b_obj <- casl_nn_backward_prop(X[i, ], y[i, ], weights,
                                       f_obj, casl_util_ReLU_p,
                                       casl_util_mse_p)
        grad[i] <- b_obj$grad_w[[level]][id]
      }

      w2 <- weights
      w2[[level]]$w[id] <- w2[[level]]$w[id] + h
      f_h_plus <- 0.5 * (casl_nn_predict(w2, X) - y)^2
      w2[[level]]$w[id] <- w2[[level]]$w[id] - 2 * h
      f_h_minus <- 0.5 * (casl_nn_predict(w2, X) - y)^2

      grad_emp <- sum((f_h_plus - f_h_minus) / (2 * h))

      max_diff <- max(max_diff,
                      abs(sum(grad) - grad_emp))
    }
  }
  max_diff
}

#' Create list of weights and momentum to describe a NN.
#'
#' @param sizes A vector giving the size of each layer, including the input
#' and output layers.
#'
#' @return A list containing initialized weights, biases, and momentum.
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
casl_nn_make_weights_mu <-
function(sizes)
{
  L <- length(sizes) - 1L
  weights <- vector("list", L)
  for (j in seq_len(L))
  {
    w <- matrix(stats::rnorm(sizes[j] * sizes[j + 1L],
                      sd = 1/sqrt(sizes[j])),
                ncol = sizes[j],
                nrow = sizes[j + 1L])
    v <- matrix(0,
                ncol = sizes[j],
                nrow = sizes[j + 1L])
    weights[[j]] <- list(w=w,
                         v=v,
                         b=stats::rnorm(sizes[j + 1L]))
  }
  weights
}

#' Apply stochastic gradient descent (SGD) to estimate NN.
#'
#' @param X A numeric data matrix.
#' @param y A numeric vector of responses.
#' @param sizes A numeric vector giving the sizes of layers in the neural network.
#' @param epochs Integer number of epochs to computer.
#' @param eta Positive numeric learning rate.
#' @param mu Non-negative momentum term.
#' @param l2 Non-negative penalty term for l2-norm.
#' @param weights Optional list of starting weights.
#'
#' @return A list containing the trained weights for the network.
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
casl_nn_sgd_mu <-
function(X, y, sizes, epochs, eta, mu=0, l2=0, weights=NULL) {

  if (is.null(weights))
  {
    weights <- casl_nn_make_weights_mu(sizes)
  }

  for (epoch in seq_len(epochs))
  {
    for (i in seq_len(nrow(X)))
    {
      f_obj <- casl_nn_forward_prop(X[i, ], weights,
                                    casl_util_ReLU)
      b_obj <- casl_nn_backward_prop(X[i, ], y[i, ], weights,
                                     f_obj, casl_util_ReLU_p,
                                     casl_util_mse_p)

      for (j in seq_along(b_obj))
      {
        weights[[j]]$b <- weights[[j]]$b -
                            eta * b_obj$grad_z[[j]]
        weights[[j]]$v <- mu * weights[[j]]$v -
                          eta * b_obj$grad_w[[j]]
        weights[[j]]$w <- (1 - eta * l2) *
                          weights[[j]]$w +
                               weights[[j]]$v
      }
    }
  }

  weights
}

#' Apply the softmax function to a vector.
#'
#' @param z A numeric vector of inputs.
#'
#' @return Output after applying the softmax function.
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
casl_util_softmax <-
function(z)
{
  exp(z) / sum(exp(z))
}

#' Apply forward propagation to for a multinomial NN.
#'
#' @param x A numeric vector representing one row of the input.
#' @param weights A list created by casl_nn_make_weights.
#' @param sigma The activation function.
#'
#' @return A list containing the new weighted responses (z) and activations (a).
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
casl_nnmulti_forward_prop <-
function(x, weights, sigma)
{
  L <- length(weights)
  z <- vector("list", L)
  a <- vector("list", L)
  for (j in seq_len(L))
  {
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    z[[j]] <- weights[[j]]$w %*% a_j1 + weights[[j]]$b
    if (j != L) {
      a[[j]] <- sigma(z[[j]])
    } else {
      a[[j]] <- casl_util_softmax(z[[j]])
    }
  }

  list(z=z, a=a)
}

#' Apply backward propagation algorithm for a multinomial NN.
#'
#' @param x A numeric vector representing one row of the input.
#' @param y A numeric vector representing one row of the response.
#' @param weights A list created by casl_nn_make_weights.
#' @param f_obj Output of the function casl_nn_forward_prop.
#' @param sigma_p Derivative of the activation function.
#'
#' @return A list containing the new weighted responses (z) and activations (a).
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
casl_nnmulti_backward_prop <-
function(x, y, weights, f_obj, sigma_p)
{
  z <- f_obj$z; a <- f_obj$a
  L <- length(weights)
  grad_z <- vector("list", L)
  grad_w <- vector("list", L)
  for (j in rev(seq_len(L)))
  {
    if (j == L)
    {
      grad_z[[j]] <- a[[j]] - y
    } else {
      grad_z[[j]] <- (t(weights[[j + 1L]]$w) %*%
                      grad_z[[j + 1L]]) * sigma_p(z[[j]])
    }
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    grad_w[[j]] <- grad_z[[j]] %*% t(a_j1)
  }

  list(grad_z=grad_z, grad_w=grad_w)
}

#' Apply stochastic gradient descent (SGD) for multinomial NN.
#'
#' @param X A numeric data matrix.
#' @param y A numeric vector of responses.
#' @param sizes A numeric vector giving the sizes of layers in the neural network.
#' @param epochs Integer number of epochs to computer.
#' @param eta Positive numeric learning rate.
#' @param mu Non-negative momentum term.
#' @param l2 Non-negative penalty term for l2-norm.
#' @param weights Optional list of starting weights.
#'
#' @return A list containing the trained weights for the network.
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
casl_nnmulti_sgd <-
function(X, y, sizes, epochs, eta, mu=0, l2=0, weights = NULL) {

  if (is.null(weights))
  {
    weights <- casl_nn_make_weights_mu(sizes)
  }

  for (epoch in seq_len(epochs))
  {
    for (i in seq_len(nrow(X)))
    {
      f_obj <- casl_nnmulti_forward_prop(X[i, ], weights,
                                         casl_util_ReLU)
      b_obj <- casl_nnmulti_backward_prop(X[i, ], y[i, ],
                                          weights, f_obj,
                                          casl_util_ReLU_p)

      for (j in seq_along(b_obj))
      {
        weights[[j]]$b <- weights[[j]]$b -
                            eta * b_obj$grad_z[[j]]
        weights[[j]]$v <- mu * weights[[j]]$v -
                          eta * b_obj$grad_w[[j]]
        weights[[j]]$w <- (1 - eta * l2) *
                          weights[[j]]$w +
                               weights[[j]]$v
      }
    }
  }

  weights
}

#' Predict values from training a multinomial neural network.
#'
#' @param weights List of weights describing the neural network.
#' @param X_test A numeric data matrix for the predictions.
#'
#' @return A matrix of predicted values.
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
casl_nnmulti_predict <-
function(weights, X_test)
{

  p <- length(weights[[length(weights)]]$b)
  y_hat <- matrix(0, ncol = p, nrow = nrow(X_test))
  for (i in seq_len(nrow(X_test)))
  {
    a <- casl_nnmulti_forward_prop(X_test[i, ], weights,
                                   casl_util_ReLU)$a
    y_hat[i,] <- a[[length(a)]]
  }

  y_hat
}

#' Create list of weights and momentum to describe a CNN.
#'
#' @param sizes A vector giving the size of each layer, including
#' the input and output layers.
#'
#' @return A list containing initialized weights, biases, and momentum.
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
casl_cnn_make_weights <-
function(sizes)
{
  L <- length(sizes) - 1L
  weights <- vector("list", L)
  for (j in seq_len(L))
  {
    if (j == 1)
    {
      w <- array(stats::rnorm(3 * 3 * sizes[j + 1]),
                  dim = c(3, 3, sizes[j + 1]))
      v <- array(0,
                  dim = c(3, 3, sizes[j + 1]))
    } else {
      if (j == 2) sizes[j] <- sizes[2] * sizes[1]
      w <- matrix(stats::rnorm(sizes[j] * sizes[j + 1],
                        sd = 1/sqrt(sizes[j])),
                  ncol = sizes[j],
                  nrow = sizes[j + 1])
      v <- matrix(0,
                  ncol = sizes[j],
                  nrow = sizes[j + 1])
    }

    weights[[j]] <- list(w=w,
                         v=v,
                         b=stats::rnorm(sizes[j + 1]))
  }
  weights
}

#' Apply forward propagation to a set of CNN weights and biases.
#'
#' @param x A numeric vector representing one row of the input.
#' @param weights A list created by casl_nn_make_weights.
#' @param sigma The activation function.
#'
#' @return A list containing the new weighted responses (z) and activations (a).
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
casl_cnn_forward_prop <-
function(x, weights, sigma)
{
  L <- length(weights)
  z <- vector("list", L)
  a <- vector("list", L)
  for (j in seq_len(L))
  {
    if (j == 1)
    {
      a_j1 <- x
      z[[j]] <- casl_util_conv(x, weights[[j]])
    } else {
      a_j1 <- a[[j - 1L]]
      z[[j]] <- weights[[j]]$w %*% a_j1 + weights[[j]]$b
    }
    if (j != L)
    {
      a[[j]] <- sigma(z[[j]])
    } else {
      a[[j]] <- casl_util_softmax(z[[j]])
    }
  }

  list(z=z, a=a)
}

#' Apply the convolution operator.
#'
#' @param x The input image as a matrix.
#' @param w Matrix of the kernel weight.
#'
#' @return Vector of the output convolution.
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
casl_util_conv <-
function(x, w) {
  d1 <- nrow(x) - 2L
  d2 <- ncol(x) - 2L
  d3 <- dim(w$w)[3]
  z <- rep(0, d1 * d2 * d3)
  for (i in seq_len(d1))
  {
    for (j in seq_len(d2))
    {
      for (k in seq_len(d3))
      {
        val <- x[i + (0:2), j + (0:2)] * w$w[,,k]
        q <- (i - 1) * d2 * d3 + (j - 1) * d3 + k
        z[q] <- sum(val) + w$b[k]
      }
    }
  }

  z
}

#' Apply backward propagation algorithm for a CNN.
#'
#' @param x A numeric vector representing one row of the input.
#' @param y A numeric vector representing one row of the response.
#' @param weights A list created by casl_nn_make_weights.
#' @param f_obj Output of the function casl_nn_forward_prop.
#' @param sigma_p Derivative of the activation function.
#'
#' @return A list containing the new weighted responses (z) and activations (a).
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
casl_cnn_backward_prop <-
function(x, y, weights, f_obj, sigma_p)
{
  z <- f_obj$z; a <- f_obj$a
  L <- length(weights)
  grad_z <- vector("list", L)
  grad_w <- vector("list", L)
  for (j in rev(seq_len(L)))
  {
    if (j == L)
    {
      grad_z[[j]] <- a[[j]] - y
    } else {
      grad_z[[j]] <- (t(weights[[j + 1]]$w) %*%
                      grad_z[[j + 1]]) * sigma_p(z[[j]])
    }
    if (j == 1)
    {
      a_j1 <- x

      d1 <- nrow(a_j1) - 2L
      d2 <- ncol(a_j1) - 2L
      d3 <- dim(weights[[j]]$w)[3]
      grad_z_arr <- array(grad_z[[j]],
                          dim = c(d1, d2, d3))
      grad_b <- apply(grad_z_arr, 3, sum)
      grad_w[[j]] <- array(0, dim = c(3, 3, d3))

      for (n in 0:2)
      {
        for (m in 0:2)
        {
          for (k in seq_len(d3))
          {
            val <- grad_z_arr[, , k] * x[seq_len(d1) + n,
                                         seq_len(d2) + m]
            grad_w[[j]][n + 1L, m + 1L, k] <- sum(val)
          }
        }
      }

    } else {
      a_j1 <- a[[j - 1L]]
      grad_w[[j]] <- grad_z[[j]] %*% t(a_j1)
    }

  }

  list(grad_z=grad_z, grad_w=grad_w, grad_b=grad_b)
}

#' Apply stochastic gradient descent (SGD) to estimate a CNN model.
#'
#' @param X A numeric data matrix.
#' @param y A numeric vector of responses.
#' @param sizes A numeric vector giving the sizes of layers in the neural network.
#' @param epochs Integer number of epochs to computer.
#' @param rho Positive numeric learning rate.
#' @param mu Non-negative momentum term.
#' @param l2 Non-negative penalty term for l2-norm.
#' @param weights Optional list of starting weights.
#'
#' @return A list containing the trained weights for the network.
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
casl_cnn_sgd <-
function(X, y, sizes, epochs, rho, mu=0, l2=0, weights=NULL) {

  if (is.null(weights))
  {
    weights <- casl_cnn_make_weights(sizes)
  }

  for (epoch in seq_len(epochs))
  {
    for (i in seq_len(nrow(X)))
    {
      f_obj <- casl_cnn_forward_prop(X[i,,], weights,
                                     casl_util_ReLU)
      b_obj <- casl_cnn_backward_prop(X[i,,], y[i,], weights,
                                      f_obj, casl_util_ReLU_p)

      for (j in seq_along(b_obj))
      {
        grad_b <- if(j == 1) b_obj$grad_b else b_obj$grad_z[[j]]
        weights[[j]]$b <- weights[[j]]$b -
                            rho * grad_b
        weights[[j]]$v <- mu * weights[[j]]$v -
                          rho * b_obj$grad_w[[j]]
        weights[[j]]$w <- (1 - rho * l2) *
                          weights[[j]]$w +
                               weights[[j]]$v
      }
    }
  }

  weights
}

#' Predict values from training a CNN.
#'
#' @param weights List of weights describing the neural network.
#' @param X_test A numeric data matrix for the predictions.
#'
#' @return A matrix of predicted values.
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
casl_cnn_predict <- function(weights, X_test)
{

  p <- length(weights[[length(weights)]]$b)
  y_hat <- matrix(0, ncol = p, nrow = nrow(X_test))
  for (i in seq_len(nrow(X_test)))
  {
    a <- casl_cnn_forward_prop(X_test[i, , ], weights,
                               casl_util_ReLU)$a
    y_hat[i, ] <- a[[length(a)]]
  }

  y_hat
}
