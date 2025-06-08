#' Compute Principal Components and store as a Data Frame
#'
#' Takes a matrix and returns a data frame with the top principal components
#' extracted. This is a simple but powerful technique for visualizing a corpus
#' of documents.
#'
#' @param x       a matrix object to pass to \code{prcomp}
#' @param k       integer. The number of components to include in the output.
#' @param center  logical. Should the data be centered?
#' @param scale   logical. Should the data be scaled? Note that this will
#'                need to be set to false if any columns in \code{x} are
#'                constant if \code{center} is also true.
#'
#' @return a data frame object containing the top \code{k} principal
#'         components of the data in x.
#'
#' @export
cnlp_utils_pca <- function(x, k = 2, center = TRUE, scale = TRUE) {

  m <- stats::prcomp(x, center = center, scale. = scale)$x

  structure(
    as.data.frame(m[,1:k]),
    class = c("tbl_df", "tbl", "data.frame")
  )
}

#' Construct the TF-IDF Matrix from Annotation or Data Frame
#'
#' Given annotations, this function returns the term-frequency inverse
#' document frequency (tf-idf) matrix from the extracted lemmas.
#'
#' @param  object       a data frame containing an identifier for the document
#'                      (set with \code{doc_var}) and token (set with
#'                      \code{token_var})
#' @param  tf_weight    the weighting scheme for the term frequency matrix.
#'                      The selection \code{lognorm} takes one plus
#'                      the log of the raw frequency (or zero if zero),
#'                      \code{binary} encodes a zero one matrix
#'                      indicating simply whether the token exists at all
#'                      in the document, \code{raw} returns raw counts,
#'                      and \code{dnorm} uses double normalization.
#' @param  idf_weight   the weighting scheme for the inverse document
#'                      matrix. The selection \code{idf} gives the
#'                      logarithm of the simple inverse frequency,
#'                      \code{smooth} gives the logarithm of one plus
#'                      the simple inverse frequency, and \code{prob}
#'                      gives the log odds of the the token occurring
#'                      in a randomly selected document. Set to \code{uniform}
#'                      to return just the term frequencies.
#' @param min_df        the minimum proportion of documents a token
#'                      should be in to be included in the vocabulary
#' @param max_df        the maximum proportion of documents a token
#'                      should be in to be included in the vocabulary
#' @param max_features  the maximum number of tokens in the vocabulary
#' @param doc_var       character vector. The name of the column in
#'                      \code{object} that contains the document ids. Defaults
#'                      to "doc_id".
#' @param token_var     character vector. The name of the column in
#'                      \code{object} that contains the tokens. Defaults to
#'                      "lemma".
#' @param vocabulary    character vector. The vocabulary set to use in
#'                      constructing the matrices. Will be computed
#'                      within the function if set to \code{NULL}. When
#'                      supplied, the options \code{min_df}, \code{max_df},
#'                      and \code{max_features} are ignored.
#' @param doc_set       optional character vector of document ids. Useful to
#'                      create empty rows in the output matrix for documents
#'                      without data in the input. Most users will want to keep
#'                      this equal to \code{NULL}, the default, to have the
#'                      function compute the document set automatically.
#'
#' @return  a sparse matrix with dimnames giving the documents and vocabular.
#'
#' @export
#' @name cnlp_utils_tfidf
cnlp_utils_tfidf <- function(
  object,
  tf_weight=c("lognorm", "binary", "raw", "dnorm"),
  idf_weight=c("idf", "smooth", "prob", "uniform"),
  min_df=0.1,
  max_df=0.9,
  max_features=1e4,
  doc_var="doc_id",
  token_var="lemma",
  vocabulary=NULL,
  doc_set=NULL
) {

  assert(inherits(object, "data.frame"), "'input' must be a data frame.")
  assert(doc_var %in% names(object), "no valid 'doc_var' found")
  assert(token_var %in% names(object), "no valid 'token_var' found")

  tf_weight <- match.arg(tf_weight)
  idf_weight <- match.arg(idf_weight)
  x <- data.frame(
    doc = object[[doc_var]],
    token = object[[token_var]],
    stringsAsFactors=FALSE
  )

  if (is.null(vocabulary)) {
    N <- length(unique(x$doc))
    possible_vocab <- table(x[!duplicated(x),]$token) / N
    possible_vocab <- possible_vocab[
      possible_vocab >= min_df & possible_vocab <= max_df
    ]
    possible_vocab <- sort(possible_vocab, decreasing=TRUE)
    vocabulary <- names(possible_vocab[
      seq(1, min(max_features, length(possible_vocab)))
    ])
  }

  assert(length(vocabulary) >= 1, "vocabulary length is too small to continue")

  # create counts
  if (is.null(doc_set)) {
    doc_set <- unique(x[["doc"]])
  }
  x <- x[x$token %in% vocabulary, ]
  x$token <- factor(x$token, levels = vocabulary)
  doc <- x[["doc"]]
  N <- length(doc_set)
  id <- match(doc, doc_set)
  mat <- methods::as(
    Matrix::sparse.model.matrix(~ token - 1,data = x),
    "TsparseMatrix"
  )

  df <- data.frame(
    id = id[mat@i + 1],
    lid = mat@j,
    count = mat@x
  )

  term_counts <- Matrix::spMatrix(
    nrow = length(doc_set),
    ncol = ncol(mat),
    i = df$id,
    j = df$lid + 1,
    x = df$count
  )

  if (tf_weight == "lognorm") {
    tf <- term_counts
    tf@x <- 1 + log2(tf@x)
    tf@x[tf@x < 0] <- 0
  } else if (tf_weight == "binary") {
    tf <- term_counts * 1.0
    tf[term_counts != 0] <- 1L
  } else if (tf_weight == "raw") {
    tf <- term_counts
  } else if (tf_weight == "dnorm") {
    tf <- 0.5 + 0.5 * term_counts / apply(term_counts, 1, max)
  }

  if (idf_weight == "idf") {
    idf <- log2(N / apply(term_counts > 0, 2, sum))
  } else if (idf_weight == "smooth") {
    idf <- log2(1 + N / apply(term_counts > 0, 2, sum))
  } else if (idf_weight == "prob") {
    n <- apply(term_counts > 0, 2, sum)
    idf <- log2( (N - n) / n)
  } else if (idf_weight == "uniform") {
    idf <- 1
  }

  tfidf <- Matrix::t(Matrix::t(tf) * idf)
  rownames(tfidf) <- doc_set
  colnames(tfidf) <- vocabulary

  return(tfidf)
}

#' @rdname cnlp_utils_tfidf
#' @export
cnlp_utils_tf <- function(
  object,
  tf_weight="raw",
  idf_weight="uniform",
  min_df=0,
  max_df=1,
  max_features=1e4,
  doc_var="doc_id",
  token_var="lemma",
  vocabulary=NULL,
  doc_set=NULL
) {
  cnlp_utils_tfidf(
    object,
    tf_weight=tf_weight,
    idf_weight=idf_weight,
    min_df=min_df,
    max_df=max_df,
    max_features=max_features,
    doc_var=doc_var,
    token_var=token_var,
    vocabulary=vocabulary,
    doc_set=doc_set
  )
}
