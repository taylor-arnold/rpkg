#' Add two sparse matrices stored as triplet lists.
#'
#' @param a A list describing a sparse matrix.
#' @param b A list describing a sparse matrix.
#'
#' @return A list describing the sum of a and b as a sparse matrix.
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
casl_sparse_add <-
function(a, b)
{
  c <- merge(a, b, by = c("i", "j"), all = TRUE,
             suffixes = c("", "2"))
  c$x[is.na(c$x)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x + c$x2
  c[, c("i", "j", "x")]
}

#' Multiply two sparse matrices stored as triplet lists.
#'
#' @param a A list describing a sparse matrix.
#' @param b A list describing a sparse matrix.
#'
#' @return A list describing the product of a and b as a sparse matrix.
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
casl_sparse_multiply <-
function(a, b)
{
  colnames(b) <- c("i2", "j2", "x2")
  c <- merge(a, b, by.x = "j", by.y = "i2",
             all = FALSE, suffixes = c("1", "2"))
  c$x <- c$x * c$x2
  c$key <- paste(c$i, c$j, sep = "-")
  x <- tapply(c$x, c$key, sum)
  key <- strsplit(names(x), "-")
  d <- data.frame(i = sapply(key, getElement, 1),
                  j = sapply(key, getElement, 2),
                  x = as.numeric(x))
  d
}

#' Compute starting lambda for lasso regression.
#'
#' @param X A sparse or dense numeric matrix.
#' @param y The numeric response vector.
#'
#' @return The numeric value of lambda max.
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
casl_sparse_lmax <-
function(X, y)
{
  svals <- apply(X, 2, stats::sd)
  mvals <- apply(X, 2, mean)
  v <- (t(y) %*% X) / svals - sum(y) * mvals / svals
  max(abs(v))
}

#' Compute GLM regression with a sparse data matrix.
#'
#' @param X A sparse or dense numeric matrix.
#' @param y The numeric response vector.
#' @param family Instance of an R `family` object.
#' @param maxit Integer maximum number of iterations.
#' @param tol Numeric tolerance parameter.
#'
#' @return The estimated regression vector.
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
casl_sparse_irwls <-
function(X, y, family=stats::binomial(), maxit=25L, tol=1e-08)
{
  p <- ncol(X)
  b <- rep(0, p)
  for(j in seq_len(maxit))
  {
    eta <- as.vector(X %*% b)
    g <- family$linkinv(eta)
    gprime <- family$mu.eta(eta)
    z <- eta + (y - g) / gprime
    W <- as.vector(gprime^2 / family$variance(g))
    bold <- b
    XTWX <- crossprod(X,X * W)
    wz <- W * z
    XTWz <- as.vector(crossprod(X, wz))

    C <- chol(XTWX, pivot=TRUE)
    if(attr(C,"rank") < ncol(XTWX))
    {
      stop("Rank-deficiency detected.")
    }
    piv <- attr(C, "pivot")
    s <- forwardsolve(t(C), XTWz[piv])
    b <- backsolve(C,s)[piv]

    if(sqrt(crossprod(b-bold)) < tol) break
  }
  list(coefficients=b, iter=j)
}

#' Compute GLM regression with data stored on disk.
#'
#' @param filename Path to a file containing the dataset.
#' @param nmax Maximum number of rows in the chunk.
#' @param family Instance of an R `family` object.
#' @param maxit Integer maximum number of iterations.
#' @param tol Numeric tolerance parameter.
#'
#' @return A list containing the regression vector beta and
#' number of iterations.
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
casl_blockwise_irwls <-
function(filename, nmax, family=stats::gaussian(), maxit=25L,
         tol=1e-08)
{
  b <- NULL

  for(j in seq_len(maxit))
  {
    fin <- file(filename, "r")
    X <- utils::read.table(fin, sep=",", nrows=nmax)
    p <- ncol(X) - 1L
    if (is.null(b)) b <- rep(0, p)

    XTWX <- matrix(0, ncol = p, nrow = p)
    XTWz <- rep(0, p)
    while(!is.null(X))
    {
      X <- as.matrix(X)
      y <- X[, 1]; X <- X[, -1, drop=FALSE]
      eta <- as.vector(X %*% b)
      g <- family$linkinv(eta)
      gprime <- family$mu.eta(eta)
      z <- eta + (y - g) / gprime
      W <- as.vector(gprime^2 / family$variance(g))
      XTWX <- XTWX + crossprod(X,X * W)
      XTWz <- XTWz + as.vector(crossprod(X, W * z))

      X <- tryCatch(utils::read.table(fin, sep=",", nrows=nmax),
                    error=function(e) NULL)
    }
    close(fin)

    bold  <- b
    C <- chol(XTWX, pivot=TRUE)
    if(attr(C,"rank") < ncol(XTWX))
    {
      stop("Rank-deficiency detected.")
    }
    piv <- attr(C, "pivot")
    s <- forwardsolve(t(C), XTWz[piv])
    b <- backsolve(C,s)[order(piv)]

    if(sqrt(crossprod(b-bold)) < tol) break
  }

  list(coefficients=b, iter=j)
}

#' Map a string object to a numeric hash value.
#'
#' @param word A string object to hash.
#' @param k Size of the hash, given as 2^(4*k).
#'
#' @return A numeric hash value from 0 to 2^(4*k) - 1.
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
casl_hash_word2index <- function(word, k=4L)
{
  h <- digest::digest(word, algo="murmur32")
  v <- strtoi(stringi::stri_sub(h, -k, -1), base=16L)
  v
}

#' Map a string object to a sign function.
#'
#' @param word A string object to hash.
#'
#' @return A numeric value of either +1 or -1.
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
casl_hash_word2sign <- function(word)
{
  h <- digest::digest(word, algo="murmur32", seed=1)
  s <- ifelse(stringi::stri_sub(h, 1, 1) %in% seq(0, 7), 1, -1)
  s
}

#' Map multiple string objects to a numeric hash values.
#'
#' @param words A vector of string objects to hash.
#' @param k Size of the hash, given as 2^(4*k).
#'
#' @return A data frame giving hash values and signs.
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
casl_hash_matrix <- function(words, k=4L)
{
  hash <- data.frame(tok=words, hash=0, sign=0,
                     stringsAsFactors = FALSE)
  for (i in seq_along(hash$tok))
  {
    hash$hash[i] <- casl_hash_word2index(hash$tok[i], k)
    hash$sign[i] <- casl_hash_word2sign(hash$tok[i])
  }
  hash
}

#' Map text to numeric hash values.
#'
#' @param text A character vector of inputs.
#' @param k Size of the hash, given as 2^(4*k).
#'
#' @return A data frame giving hashed values.
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
casl_fhash <- function(text, k=4L)
{
  tok <- stringi::stri_trans_tolower(text)
  tok <- stringi::stri_split_boundaries(tok, type="word",
                                        skip_word_none=TRUE)
  id <- mapply(function(u,v) rep(u, v),
               seq_along(tok), sapply(tok, length))
  id <- as.vector(unlist(id))
  df <- data.frame(tok=unlist(tok), id=id,
                   hash=0L, sign=0L,
                   stringsAsFactors = FALSE)
  hash <- casl_hash_matrix(unique(df$tok), k=k)
  id <- match(df$tok, hash$tok)
  df$hash <- hash$hash[id]
  df$sign <- hash$sign[id]
  df
}

#' Construct a data matrix using hash function.
#'
#' @param df The output of casl_fhash.
#' @param k Size of the hash, given as 2^(4*k).
#'
#' @return Sparse matrix representation of the data matrix.
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
casl_fhash_matrix <-
function(df, k=4L)
{
  Matrix::sparseMatrix(i=df$id, j=(df$hash + 1L), x=df$sign,
                       dims = c(max(df$id), 16^k))
}

#' Construct a data matrix using hash function.
#'
#' @param text Input vector of text.
#' @param k Size of the hash, given as 2^(4*k).
#' @param hash Either NULL or cached hash data frame.
#'
#' @return A list containing the data frame of hashed values and the
#' updated hash table.
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
casl_fhash_cache <-
function(text, k=4L, hash=NULL)
{
  tok <- stringi::stri_trans_tolower(text)
  tok <- stringi::stri_split_boundaries(tok, type="word",
                                        skip_word_none=TRUE)
  id <- mapply(function(u,v) rep(u, v),
               seq_along(tok), sapply(tok, length))
  id <- as.vector(unlist(id))
  df <- data.frame(tok=unlist(tok), id=id,
                   hash=0L, sign=0L,
                   stringsAsFactors = FALSE)

  if (!is.null(hash))
  {
    words <- setdiff(df$tok, hash$tok)
  } else {
    words <- unique(df$tok)
  }

  if (length(words) > 0L)
  {
    hash <- rbind(hash, casl_hash_matrix(words, k = k))
  }

  id <- match(df$tok, hash$tok)
  df$hash <- hash$hash[id]
  df$sign <- hash$sign[id]
  list(df=df, hash=hash)
}
