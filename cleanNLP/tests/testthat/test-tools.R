library(cleanNLP)

context("Testing tools for working with textual data")

data(un)

test_that("testing utils_tfidf", {
  cnlp_init_stringi()
  anno <- cnlp_annotate(un, verbose=FALSE)

  tf_direct <- cnlp_utils_tfidf(anno$token)
  expect_equal(dim(tf_direct), c(30, 79))
  expect_equal(anno$document$doc_id, rownames(tf_direct))
})


test_that("testing tidy_pca", {

  cnlp_init_stringi()
  anno <- cnlp_annotate(un, verbose=FALSE)

  res <- cnlp_utils_pca(cnlp_utils_tfidf(anno$token))
  expect_equal(rownames(res), anno$document$doc_id)
  expect_equal(colnames(res), c("PC1", "PC2"))

  res <- cnlp_utils_pca(cnlp_utils_tfidf(anno$token), k=4)
  expect_equal(rownames(res), anno$document$doc_id)
  expect_equal(colnames(res), c("PC1", "PC2", "PC3", "PC4"))

})
