annotate_with_udpipe <- function(input, verbose) {

  assert(volatiles$udpipe$init, "You must initilize the backend.")
  assert(requireNamespace("udpipe"), "The udpipe package is required")

  doc <- vector("list", length(input))
  token <- vector("list", length(input))

  for (i in seq_len(nrow(input))) {
    x <- input$text[i]
    doc_id <- input$doc_id[i]

    if (stringi::stri_length(x))
    {
        anno <- as.data.frame(
          udpipe::udpipe_annotate(
            object=volatiles$udpipe$model_obj,
            x=x,
            tokenizer=volatiles$udpipe$tokenizer,
            tagger=volatiles$udpipe$tagger,
            parser=volatiles$udpipe$parser,
          )
        )
        token_with_ws <- udpipe_reconstruct(
          anno$sentence_id,
          anno$token,
          anno$token_id,
          anno$misc
        )

        token[[i]] <- data.frame(
          doc_id=doc_id,
          sid=anno$sentence_id,
          tid=anno$token_id,
          token=anno$token,
          token_with_ws=token_with_ws,
          lemma=anno$lemma,
          upos=anno$upos,
          xpos=anno$xpos,
          feats=anno$feats,
          tid_source=anno$head_token_id,
          relation=anno$dep_rel,
          stringsAsFactors=FALSE
        )
    }

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

# taken from udpipe package as it is currently not exported
udpipe_reconstruct <- function(sentence_id, token, token_id, misc){

  rawtxt <- token

  has_spacesafter_no <- grepl(pattern = "SpaceAfter=No", misc)
  has_spacesafter    <- grepl(pattern = "SpacesAfter=", misc)
  has_spacesbefore   <- grepl(pattern = "SpacesBefore=", misc)
  has_spacesintoken  <- grepl(pattern = "SpacesInToken=", misc)
  has_multiple       <- grepl(pattern = "\\|", misc)

  after <- rep("", length(token))
  after[!has_spacesafter] <- " "
  after[is.na(misc)] <- " "
  after[has_spacesafter_no] <- ""
  idx <- which(has_spacesafter)
  addme <- gsub(pattern = "(SpacesAfter=)(.+)", "\\2", misc[idx])
  idx_multiple <- which(has_spacesafter & has_multiple)
  if(length(idx_multiple) > 0){
    addme_multiple <- sapply(strsplit(misc[idx_multiple], split = "\\|"), FUN=function(x) grep(pattern = "SpacesAfter", x = x, value = TRUE))
    addme_multiple <- gsub(pattern = "SpacesAfter=", replacement = "", addme_multiple)
    addme[which(idx_multiple %in% idx)] <- addme_multiple
  }
  addme <- gsub("\\\\s", " ", addme)
  addme <- gsub("\\\\n", "\n", addme)
  addme <- gsub("\\\\t", "\t", addme)
  addme <- gsub("\\\\r", "\r", addme)
  addme <- gsub("\\\\p", "|", addme)
  addme <- gsub("\\\\", "\\", addme)
  after[idx] <- addme
  after[length(after)] <- gsub("\n$", "", after[length(after)])

  before <- rep("", length(token))
  idx <- which(has_spacesbefore)
  addme <- gsub(pattern = "(SpacesBefore=)(.+)", "\\2", misc[idx])
  idx_multiple <- which(has_spacesbefore & has_multiple)
  if(length(idx_multiple) > 0){
    addme_multiple <- sapply(strsplit(misc[idx_multiple], split = "\\|"), FUN=function(x) grep(pattern = "SpacesBefore", x = x, value = TRUE))
    addme_multiple <- gsub(pattern = "SpacesBefore=", replacement = "", addme_multiple)
    addme[which(idx_multiple %in% idx)] <- addme_multiple
  }
  addme <- gsub("\\\\s", " ", addme)
  addme <- gsub("\\\\n", "\n", addme)
  addme <- gsub("\\\\t", "\t", addme)
  addme <- gsub("\\\\r", "\r", addme)
  addme <- gsub("\\\\p", "|", addme)
  addme <- gsub("\\\\", "\\", addme)
  before[idx] <- addme

  idx <- which(has_spacesintoken)
  addme <- gsub(pattern = "(SpacesInToken=)(.+)", "\\2", misc[idx])
  idx_multiple <- which(has_spacesintoken & has_multiple)
  if(length(idx_multiple) > 0){
    addme_multiple <- sapply(strsplit(misc[idx_multiple], split = "\\|"), FUN=function(x) grep(pattern = "SpacesInToken", x = x, value = TRUE))
    addme_multiple <- gsub(pattern = "SpacesInToken=", replacement = "", addme_multiple)
    addme[which(idx_multiple %in% idx)] <- addme_multiple
  }

  return(sprintf("%s%s%s", before, token, after))

}
