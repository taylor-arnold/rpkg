#' Save State of the Union Addresses in Directory
#'
#' This function saves the State of the Union
#' Addresses to disk, with one address in each
#' file. Useful for testing and demonstrating
#' text analysis functions that read from files.
#'
#' @param dir     an optional directory of where
#'                the files should be saved. Otherwise,
#'                a temporary location is used. Will be
#'                created if it does not exist. Note
#'                that these temporary files are erased
#'                (eventually) after exiting R.
#'
#' @return        a character vector with complete paths
#'                to the texts
#'
#' @examples
#'
#' fp <- sotu_dir()
#' txt <- readLines(fp[200])
#' txt
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @export
sotu_dir <- function(dir = tempfile()) {

  # Create directory if not already extant
  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  # Load the data
  sotu_meta <- sotu_text <- NULL
  utils::data("sotu_meta", envir = environment())
  utils::data("sotu_text", envir = environment())

  # Complex logic to get nice files names
  fnames <- gsub(".", "", tolower(sotu_meta$president), fixed = TRUE)
  fnames <- gsub(" ", "-", fnames, fixed = TRUE)
  fnames <- sprintf("%s-%04d", fnames, sotu_meta$year)
  append <- rep("", length(fnames))
  index <- which(duplicated(fnames))
  append[index] <- "b"
  append[index - 1] <- "a"
  fnames <- sprintf("%s%s.txt", fnames, append)
  fnames <- file.path(dir, fnames)

  # Save the results
  res <- mapply(function(u, v) writeLines(u, v), sotu_text, fnames)

  # Return file paths
  return(fnames)
}
