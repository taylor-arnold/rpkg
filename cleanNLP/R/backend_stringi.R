annotate_with_stringi <- function(input, verbose) {

  assert(volatiles$stringi$init, "You must initilize the backend.")
  assert(requireNamespace("stringi"), "The stringi package is required")

  token <- vector("list", length(input))
  doc <- vector("list", length(input))

  for (i in seq_len(nrow(input))) {
    x <- input$text[i]
    doc_id <- input$doc_id[i]

    if (stringi::stri_length(x) == 0L) next

    sent <- stringi::stri_extract_all_boundaries(
      x, type="sentence", locale=volatiles$stringi$locale
    )[[1]]

    word <- stringi::stri_extract_all_boundaries(
      sent, type="word", locale=volatiles$stringi$locale
    )

    # do we keep the white spaces?
    if (!volatiles$stringi$include_spaces)
    {
      word <- lapply(word, function(v)
        v[!stringi::stri_detect(
          v,
          regex="\\A[\\h\\n\\t\\f]+\\Z"
        )]
      )
    }

    # create ids
    sid <- rep(seq_along(word), sapply(word, length))
    tid <- unlist(lapply(word, seq_along))

    # construct xpos codes
    word <- unlist(word)
    upos <- rep("X", length(word))
    upos[stringi::stri_detect(
      word, regex="\\A[\\h\\n\\t\\f]+\\Z"
    )] <- "SYM"
    upos[stringi::stri_detect(
      word,
      regex="\\A[\\p{Terminal_Punctuation}]+\\Z"
    )] <- "PUNCT"

    if (length(word) == 0L) next

    token[[i]] <- data.frame(
      doc_id = doc_id,
      sid = as.integer(sid),
      tid = as.integer(tid),
      token = word,
      lemma = stringi::stri_trans_tolower(word),
      upos = upos,
      stringsAsFactors = FALSE
    )

    if (verbose > 0)
    {
      if ((i %% verbose) == 0)
      {
        cmsg(verbose, "Processed document %d of %d\n", i, nrow(input))
      }
    }
  }

  anno <- list()
  if (!all(unlist(lapply(token, is.null))))
  {
    anno$token <- structure(do.call("rbind", token),
                            class = c("tbl_df", "tbl", "data.frame"))
  }
  anno$document <- input[,!(names(input) == "text"),drop=FALSE]

  return(anno)
}
