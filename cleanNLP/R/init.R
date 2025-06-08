#' Interface for initializing the spacy backend
#'
#' This function must be run before annotating text with
#' the spacy backend. It sets the properties for the
#' spacy engine and loads the file using the R to Python
#' interface provided by reticulate.
#'
#' @param model_name    string giving the model name for the spacy backend.
#'                      Defaults to "en_core_web_sm" (English) if set to NULL.
#' @param disable       an optional vector of pipes to disable.
#' @param max_length    amount of temporary memory provided to Spacy, in
#'                      characters. The default of 1000000 should work for most
#'                      applications, but can be increased when working with
#'                      long documents.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'cnlp_init_spacy(model_name = "en_core_web_sm")
#'}
#'
#' @export
cnlp_init_spacy <- function(model_name=NULL, disable=NULL, max_length=NULL) {

  check_python()
  volatiles$spacy$model_name  <- ifnull(model_name, "en_core_web_sm")
  volatiles$spacy$max_length  <- ifnull(max_length, 1000000)

  if (is.null(disable))
  {
    volatiles$spacy$obj <- volatiles$cleannlp$spacy$spacyCleanNLP(
      volatiles$spacy$model_name,
      volatiles$spacy$max_length
    )
  } else {
    volatiles$spacy$obj <- volatiles$cleannlp$spacy$spacyCleanNLP(
      volatiles$spacy$model_name,
      volatiles$spacy$max_length,
      disable
    )
  }

  assert(
    !is.null(volatiles$spacy$obj$nlp),
    sprintf(
      "model %s not found; use cnlp_download_spacy(\"%s\") to install",
      volatiles$spacy$model_name,
      volatiles$spacy$model_name
    )
  )
  volatiles$spacy$init <- TRUE
  volatiles$model_init_last <- "spacy"
}

#' Interface for initializing the udpipe backend
#'
#' This function must be run before annotating text with
#' the udpipe backend. It will parse in English by default,
#' but you can load other models as well.
#'
#' @param model_name   string giving the model namel.
#'                     Defaults to "english" if NULL.
#'                     Ignored if \code{model_path} is not NULL.
#' @param model_path   provide a full path to a model file.
#' @param tokenizer    a character string of length 1, which is either
#'                     'tokenizer' (default udpipe tokenisation) or a
#'                     character string with more
#'                     complex tokenisation options as specified in <URL:
#'                     http://ufal.mff.cuni.cz/udpipe/users-manual> in which
#'                     case tokenizer should be a character string where the
#'                     options are put after each other using the semicolon as
#'                     separation.
#' @param tagger       a character string of length 1, which is either 'default'
#'                     (default udpipe POS tagging and lemmatisation) or 'none' (no
#'                     POS tagging and lemmatisation needed) or a character string
#'                     with more complex tagging options as specified in <URL:
#'                     http://ufal.mff.cuni.cz/udpipe/users-manual> in which case
#'                     tagger should be a character string where the options are
#'                     put after each other using the semicolon as separation.
#' @param parser       a character string of length 1, which is either 'default'
#'                     (default udpipe dependency parsing) or 'none' (no dependency
#'                     parsing needed) or a character string with more complex
#'                     parsing options as specified in <URL:
#'                     http://ufal.mff.cuni.cz/udpipe/users-manual> in which case
#'                     parser should be a character string where the options are
#'                     put after each other using the semicolon as separation.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'cnlp_init_udpipe(model_name = "english")
#'}
#'
#' @export
cnlp_init_udpipe <- function(
  model_name = NULL,
  model_path = NULL,
  tokenizer = "tokenizer",
  tagger = "default",
  parser = "default"
)
{
  model_name <- ifnull(model_name, "english")
  model_loc <- system.file("extdata", package="cleanNLP")

  if (is.null(model_path))
  {
    model_loc <- system.file("extdata", package="cleanNLP")
    model_path <- Sys.glob(file.path(model_loc,
                                    sprintf("%s-*.udpipe", model_name)))[1]

    # If model does not exist, download it
    if (is.na(model_path)) {
      udpipe::udpipe_download_model(
        language = model_name,
        model_dir = model_loc
      )
    }

    model_path <- Sys.glob(file.path(model_loc,
                                    sprintf("%s-*.udpipe", model_name)))[1]
  }

  volatiles$udpipe$model_name   <- ifnull(model_name, "english")
  volatiles$udpipe$model_path   <- model_path
  volatiles$udpipe$model_obj    <- udpipe::udpipe_load_model(model_path)
  volatiles$udpipe$tokenizer    <- tokenizer
  volatiles$udpipe$tagger       <- tagger
  volatiles$udpipe$parser       <- parser

  volatiles$udpipe$init <- TRUE
  volatiles$model_init_last <- "udpipe"
}

#' Interface for initializing the standard R backend
#'
#' This function must be run before annotating text with
#' the tokenizers backend.
#'
#' @param locale            string giving the locale name to
#'                          pass to the stringi functions. If
#'                          \code{NULL}, the default locale is
#'                          selected
#'
#' @param include_spaces    logical. Should spaces be included as tokens in
#'                          the output. Defaults to FALSE
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'cnlp_init_stringi()
#'}
#'
#' @export
cnlp_init_stringi <- function(locale=NULL, include_spaces=FALSE) {

  volatiles$stringi$locale         <- ifnull(locale, stringi::stri_locale_get())
  volatiles$stringi$init           <- TRUE
  volatiles$stringi$include_spaces <- include_spaces
  volatiles$model_init_last        <- "stringi"

}

check_python <- function() {

  disc <- reticulate::py_discover_config(required_module="cleannlp")
  assert(
    !is.null(disc$required_module_path),
    "Python module 'cleannlp' not found. Install with:\n  pip install cleannlp"
  )

  assert(
    reticulate::py_module_available("cleannlp"),
    paste(c(
      "The 'cleannlp' appears to be available on your system, however\n",
      "the reticulate package has selected an alternative version of Python\n",
      "to the one where you installed the module. Restart R and run:\n\n",
      "   library(cleanNLP)\n\n",
      "prior to running any other code. If that still produces this error,\n",
      "restart R and manually select the version of Python before running\n",
      "any other functions with:\n\n",
      sprintf("   use_python(\"%s\")\n\n", disc$python)
    ), collapse=" ")
  )

  if (is.null(volatiles$cleannlp))
  {
    volatiles$cleannlp <- reticulate::import("cleannlp")
  }

  version_num_required <- "1.0.3"
  assert(
    volatiles$cleannlp$VERSION >= "1.0.3",
    paste(c(
      "Python module 'cleannlp' was found, but is out of date.",
      "Upgrade with:\n  pip install -U cleannlp"
    ), collapse=" ")
  )
}
