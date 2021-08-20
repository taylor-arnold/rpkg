#' Download java files needed for CoreNLP
#'
#' The cleanNLP package does not supply the raw java files
#' provided by the Stanford NLP Group as they are quite large.
#' This function downloads the libraries automatically, by default
#' into the directory where the package was installed. These are
#' not required for using the spaCy Python implementation.
#'
#' @param type           type of files to download. The base package
#                        is always required.
#'                       By default, the function downloads
#'                       the base package and English model files.
#' @param output_loc     a string showing where the files are to be
#'                       downloaded. If missing, will try to download
#'                       files into the directory where the package was
#'                       original installed.
#' @param url            the url to try to download components from. Setting
#'                       to NULL uses the default location on the Stanford
#'                       NLP server, but you can set this manually by using
#'                       this option. It also allows for local files, but
#'                       note that the path must include the prefix
#'                       \code{file://}. For details, see
#'                       \code{\link{download.file}}.
#' @param url_core       if \code{url} is not null, this flag indicates
#'                       whether the path given to url points to the core
#'                       nlp files (which are zipped) or
#'                       to model files (which are unzipped).
#' @param force          logical. Should files be downloaded if they appear
#'                       to already exist?
#' @param baseURL        the url that contains the required CoreNLP packages
#' @param coreFile       the name of the zip and jar (without extentsion)
#'
#'
#'@examples
#'\dontrun{
#'cnlp_download_corenlp()
#'cnlp_download_corenlp(type="spanish")
#'cnlp_download_corenlp(baseURL=`https://displayrreuspackagecache.blob.core.windows.net/r-package-cache`)
#'}
#' @importFrom utils download.file
#' @export
cnlp_download_corenlp <- function(
  type = c("default", "base", "en"),
  output_loc, 
  url = NULL, 
  url_core = TRUE, 
  force = FALSE,
  baseURL = "https://downloads.cs.stanford.edu/nlp/software"
  coreFile = "stanford-corenlp-full-2018-10-05"
) {

  op <- options(timeout = 900)
  on.exit(options(op))
  
  # Determine where files should be saved
  if (missing(output_loc)) {
    output_loc <- system.file("extdata", package="cleanNLP")
    if (file.access(output_loc, "6") == -1)
      stop("You do not have read+write access to location where the",
           "cleanNLP package is installed! You must specify an output",
           "location with output_loc.")
  }

  if (!dir.exists(output_loc)) {
    stop(sprintf("The output directory '%s' does not exist", output_loc))
  }

  # if url is given, simply download the specified files as required
  if (!is.null(url)) {
    if (url_core) {
      if (!dir.exists(file.path(output_loc, coreFile))) {
        fp <- check_file_exists(file.path(output_loc,
                                           paste0(coreFile, ".zip")),
                                 force = force)
        ret <- download.file(url = file.path(baseURL, paste0(coreFile, ".zip")), destfile = fp, mode = "wb")
        if (ret != 0) stop("Download error!")

        utils::unzip(file.path(output_loc, paste0(coreFile, ".zip")),
                     exdir = output_loc)
        file.remove(file.path(output_loc, paste0(coreFile, ".zip")))
        return(ret)
      }
    } else {
      fname <- basename(url)
      file.to.download <- file.path(output_loc, coreFile, fname)
      fp <- check_file_exists(file.to.download, force = force)
      ret <- download.file(url = file.to.download, destfile = fp, mode = "wb")
    }
  }

  # otherwise, determine what file types should be downloaded
  type <- match.arg(type)

  if (type %in% c("default", "base")) {
    if (!dir.exists(file.path(output_loc, coreFile))) {
      file.location <- file.path(output_loc, paste0(coreFile, ".zip"))
      fp <- check_file_exists(file.location, force = force)
      ret <- download.file(url = file.path(baseURL, paste0(coreFile, ".zip")),
                           destfile = fp, mode = "wb")
      if (ret != 0) stop("Download error!")

      utils::unzip(file.location, exdir = output_loc)
      file.remove(file.location)
      ret <- 0L
    }
  }

  if (!file.exists(file.path(output_loc)))
    stop("Must download base files to this location first!",
         "Set type='base'.")

  if (type %in% c("default", "en")) {
    fp <- check_file_exists(file.path(output_loc, coreFile,
             "stanford-english-corenlp-2018-10-05-models.jar"),
             force = force)
    ret <- download.file(url = file.path(baseURL, "stanford-english-corenlp-2018-10-05-models.jar"),
                         destfile = fp, mode = "wb")
  }

  ret
}

check_file_exists <- function(path, force = FALSE) {
  if (file.exists(path) & !force) {
    stop(sprintf("file already exists at: %s", path))
  } else if (file.exists(path) & force) {
    message(sprintf("overwriting file at: %s", path))
  }

  return(path)
}

#' Download model files needed for udpipe
#'
#' The cleanNLP package does not supply the model files required
#' for using the udpipe backend. These files can be downloaded
#' with this function. The models are saved, by default, in the
#' location where the package is installed. They will be saved
#' between R sessions. This function is called internally by
#' \code{\link{cnlp_init_udpipe}} if a model is not available.
#'
#' @param model_name   string giving the model namel.
#'                     Defaults to "english" if NULL.
#' @param model_loc    where should be model be downloaded to. If
#'                     set to NULL, will be downloaded in the location
#'                     that the cleanNLP package is installed.
#'
#'@examples
#'\dontrun{
#'cnlp_download_core_nlp()
#'cnlp_download_core_nlp(type="spanish")
#'}
#' @export
cnlp_download_udpipe <- function(model_name = "english", model_loc = NULL) {
  if (is.null(model_loc)) {
    model_loc <- system.file("extdata", package="cleanNLP")
  }

  udpipe::udpipe_download_model(language = model_name,
                                model_dir = model_loc)

}