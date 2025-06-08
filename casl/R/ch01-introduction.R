#' Run the annotation pipeline on a set of documents
#'
#' @param x Numeric vector with which to classify the data.
#' @param y Vector of responses coded as 0s and 1s.
#'
#' @return The best split value(s).
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
casl_utils_best_split <-
function(x, y)
{
  unique_values <- unique(x)
  class_rate <- rep(0, length(unique_values))
  for (i in seq_along(unique_values))
  {
    class_rate[i] <- sum(y == (x >= unique_values[i]))
  }
  unique_values[class_rate == max(class_rate)]
}
