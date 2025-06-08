#' Download model files needed for spacy
#'
#' The cleanNLP package does not supply the model files required
#' for using the spacy backend. These files can be downloaded
#' with this function. If you need more control, download directly from
#' Python.
#'
#' @param model_name   string giving the model namel.
#'                     Defaults to "en_core_web_sm".
#'
#'@examples
#'\dontrun{
#'cnlp_download_spacy(model_name="en_core_web_sm")
#'}
#' @export
cnlp_download_spacy <- function(model_name="en_core_web_sm") {
  spacy <- reticulate::import("spacy")
  spacy$cli$download(model_name)
}
