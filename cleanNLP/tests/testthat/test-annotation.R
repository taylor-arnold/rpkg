library(cleanNLP)

context("Testing annotation backends")

data(un)

test_that("testing stringi", {
  cnlp_init_stringi()
  anno <- cnlp_annotate(un, verbose=FALSE)
  expect_equal(names(anno), c("token", "document"))

  cnlp_init_stringi()
  anno <- cnlp_annotate(un$text, verbose=FALSE)
  expect_equal(names(anno), c("token", "document"))

  cnlp_init_stringi()
  input <- un$text
  names(input) <- un$doc_id
  anno <- cnlp_annotate(input, verbose=FALSE)
  expect_equal(names(anno), c("token", "document"))
})

test_that("testing spacy", {
  testthat::skip_on_cran()

  cnlp_init_spacy()
  anno <- cnlp_annotate(un, verbose=FALSE)
  expect_equal(names(anno), c("token", "entity", "document"))
  expect_equal(names(anno$token), c("doc_id", "sid", "tid", "token", "token_with_ws", "lemma", "upos", "xpos", "tid_source", "relation"))
})

test_that("testing udpipe", {
  testthat::skip_on_cran()

  cnlp_init_udpipe()
  anno <- cnlp_annotate(un, verbose=FALSE)
  expect_equal(names(anno), c("token", "document"))
  expect_equal(names(anno$token), c("doc_id", "sid", "tid", "token", "token_with_ws", "lemma", "upos", "xpos", "feats", "tid_source", "relation"))
})
