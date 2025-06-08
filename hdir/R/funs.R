#' Download URL
#'
#' Downloads the content of a URL, with a cache implemented to avoiding
#' re-downloading the same data multiple times.
#'
#' @param url         string of the url to grab      
#' @param cache_dir   string of the cache directory    
#' @param page        name of the page; used only for progress message    
#' @param force       if set, will ignore and overwrite current page in the cache   
#'
#' @return A response object from the httr package. 
#'
#' @importFrom rlang hash
#' @importFrom httr GET
#' @export
hdir_cache_get <- function(url, cache_dir, page = NULL, force = FALSE)
{
  # create cache directory if it does not yet exist
  dir.create(cache_dir, showWarnings = FALSE)

  # create a cache of the query
  cache_file <- file.path(cache_dir, paste0(rlang::hash(url), ".rds"))

  # check if file exists and either load or query and save
  if (file.exists(cache_file) & !force)
  {
    res <- readRDS(cache_file)
  } else {
    res <- httr::GET(url)
    saveRDS(res, cache_file)
    if (!is.null(page)) { message(sprintf("Downloading %s", page)) }
  }

  return(res)
}

#' Construct the TF-IDF Matrix from Annotation or Data Frame
#'
#' Given annotations, this function returns the term-frequency inverse
#' document frequency (tf-idf) matrix from the extracted lemmas.
#'
#' @param  object       a data frame containing an identifier for the document
#'                      (set with \code{doc_var}) and token (set with
#'                      \code{token_var})
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
#'
#' @return  a tibble in wide format with term frequencies and tf-idf values.
#'
#' @importFrom dplyr group_by summarize mutate ungroup n
#' @export
hdir_text_tfidf <- function(
  object,
  min_df = 0.1,
  max_df = 0.9,
  max_features = 1e4,
  doc_var = "doc_id",
  token_var = "lemma",
  vocabulary = NULL
) {

  .assert(inherits(object, "data.frame"), "'input' must be a data frame.")
  .assert(doc_var %in% names(object), "no valid 'doc_var' found")
  .assert(token_var %in% names(object), "no valid 'token_var' found")

  doc_id <- token <- tf <- NULL
  x <- data.frame(
    doc_id = object[[doc_var]],
    token = object[[token_var]],
    stringsAsFactors=FALSE
  )

  N <- length(unique(x$doc_id))

  if (is.null(vocabulary)) {
    possible_vocab <- table(x[!duplicated(x),]$token) / N
    possible_vocab <- possible_vocab[
      possible_vocab >= min_df & possible_vocab <= max_df
    ]
    possible_vocab <- sort(possible_vocab, decreasing=TRUE)
    vocabulary <- names(possible_vocab[
      seq(1, min(max_features, length(possible_vocab)))
    ])
  }

  .assert(length(vocabulary) >= 1, "vocabulary length is too small to continue")

  # create counts
  x <- x[x$token %in% vocabulary, ]
  tf_tibble <- dplyr::group_by(x, doc_id, token)
  tf_tibble <- dplyr::summarize(tf_tibble, tf = dplyr::n())
  tf_tibble <- dplyr::group_by(tf_tibble, token)
  tf_tibble <- dplyr::mutate(tf_tibble,
    tfidf = (1 + log2(tf)) * log2(N / dplyr::n())
  )
  tf_tibble <- dplyr::ungroup(tf_tibble)

  return(tf_tibble)
}

#' Add network metadata and layout information
#'
#' Given a data frame with two columns giving edges, runs a suite
#' of network algorithms over the resulting graph. 
#'
#' @param edges       data frame describing the edges
#' @param node_name   name of the node id column; defaults to "id"
#' @param directed    logical; is the graph directed?
#'
#' @return  a list with two elements, each containing a tibble.
#'
#' @importFrom igraph graph.edgelist layout_nicely V get.edgelist components 
#'                    degree membership cluster_walktrap induced_subgraph
#'                    eigen_centrality closeness betweenness
#' @importFrom tibble tibble
#' @export
hdir_network_metrics <- function(edges, node_name = "id", directed = FALSE)
{
  H <- igraph::graph.edgelist(as.matrix(edges[,1:2]), directed = directed)
  L <- igraph::layout_nicely(H)

  vs <- igraph::V(H)
  es <- igraph::get.edgelist(H)
  noms <- names(vs)
  cmp <- igraph::components(H)
  ids <- cbind(match(es[,1], noms),  match(es[,2], noms))

  # Properties from the whole graph
  if (directed)
  {
    node_out <- tibble::tibble(
      id = as.character(noms),
      x = L[,1],
      y = L[,2],
      degree_out = igraph::degree(H, mode = "out"),
      degree_in = igraph::degree(H, mode = "in"),
      degree_total = igraph::degree(H, mode = "total"),
      eigen = NA_real_,
      between = NA_real_,
      cluster = as.character(as.integer(
        igraph::membership(igraph::cluster_walktrap(H))
      )),
      component = as.integer(cmp$membership),
      component_size = cmp$csize[as.integer(cmp$membership)]
    )
  } else {
    node_out <- tibble::tibble(
      id = as.character(noms),
      x = L[,1],
      y = L[,2],
      degree = igraph::degree(H, mode = "all"),
      eigen = NA_real_,
      close = NA_real_,
      between = NA_real_,
      cluster = as.character(as.integer(
        igraph::membership(igraph::cluster_walktrap(H))
      )),
      component = as.integer(cmp$membership),
      component_size = cmp$csize[as.integer(cmp$membership)]
    )
  }

  names(node_out)[1] <- node_name

  # Properties by component
  membership <- as.integer(cmp$membership)
  for (i in unique(membership))
  {
    Hs <- igraph::induced_subgraph(H, membership == i)
    index <- which(node_out$component == i)
    node_out$eigen[index] <- igraph::eigen_centrality(Hs, directed = FALSE)$vector
    if (!directed) { node_out$close[index] <- igraph::closeness(Hs) }
    node_out$between[index] <- igraph::betweenness(Hs, directed = directed)
  }

  # Reorder components by size
  tab <- as.integer(names(sort(table(membership), decreasing = TRUE)))
  node_out$component <- match(node_out$component, tab)

  edge_out <- tibble::tibble(
    x = L[ids[,1],1],
    xend = L[ids[,2],1],
    y = L[ids[,1],2],
    yend = L[ids[,2],2]
  )

  list(node = node_out, edge = edge_out)
}

#' Create a data frame of pairwise (Euclidiean) distances
#'
#' Given a matrix with observations in the rows labeled with the row names
#' and numeric features in the columns, this produces a data frame of the
#' distances between all pairs of observations.
#'
#' @param x           the matrix to compute distances between rows.
#' @param item_name   name to give to the set of rows; defaults to "document"
#'
#' @return  a tibble in long format, with one row for each pair of observations.
#'
#' @examples
#' mat <- matrix(rnorm(26 * 5), ncol = 5)
#' rownames(mat) <- letters
#' hdir_tidy_distance(mat)
#'
#' @importFrom stats dist
#' @export
hdir_tidy_distance <- function(x, item_name = "document")
{
  d <- as.matrix(stats::dist(as.matrix(x)))
  rownames(d) <- rownames(x)
  colnames(d) <- rownames(x)

  hdir_tidy_matrix(
    d,
    sprintf("%s1", item_name),
    sprintf("%s2", item_name),
    "distance"
  )
}

#' Create a data frame of pairwise angle distances
#'
#' Given a matrix with observations in the rows labeled with the row names
#' and numeric features in the columns, this produces a data frame of the
#' angle distances between all pairs of observations.
#' 
#' @param x           the matrix to compute similarities between rows.
#' @param item_name   name to give to the set of rows; defaults to "document"
#'
#' @return  a tibble in long format, with one row for each pair of observations.
#'
#' @examples
#' mat <- matrix(rnorm(26 * 5), ncol = 5)
#' rownames(mat) <- letters
#' hdir_tidy_angle_distance(mat)
#'
#' @export
hdir_tidy_angle_distance <- function(x, item_name = "document")
{
  x <- as.matrix(x)
  sim <- x / sqrt(rowSums(x * x))
  sim <- sim %*% t(sim)

  out <- hdir_tidy_matrix(
    sim, sprintf("%s1", item_name), sprintf("%s2", item_name), "distance"
  )
  out$distance[out$distance > 1] <- 1
  out$distance[out$distance < -1] <- -1
  out$distance <- acos(out$distance) / pi
  out
}

#' Create a data frame of principal components
#'
#' Given a matrix with observations in the rows labeled with the row names
#' and numeric features in the columns, this produces a data frame of the
#' principal components for each observation.
#'
#' @param x       the matrix to compute principal components from.
#' @param n       number of components to compute
#' @param scale   logical; should columns by scaled before PCA
#' @param item_name   name to give to the set of rows; defaults to "document"
#'
#' @examples
#' mat <- matrix(rnorm(26 * 5), ncol = 5)
#' rownames(mat) <- letters
#' hdir_tidy_pca(mat)
#'
#' @return  a tibble with one row for each observation.
#'
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr bind_cols
#' @importFrom irlba prcomp_irlba
#' @export
hdir_tidy_pca <- function(x, n = 3, scale = TRUE, item_name = "document")
{
  x <- as.matrix(x)
  df <- tibble::as_tibble(
    irlba::prcomp_irlba(t(x), n = n, scale. = scale)$rotation,
    .name_repair = "minimal"
  )
  names(df) <- sprintf("v%d", seq_len(ncol(df)))
  if (!is.null(rownames(x)))
  {
    df <- dplyr::bind_cols(tibble::tibble(rownames(x)), df)
    names(df)[1] <- item_name
  }
  df
}

#' Create a data frame of umap components
#'
#' Given a matrix with observations in the rows labeled with the row names
#' and numeric features in the columns, this produces a data frame of the
#' UMAP projection coordinates for each observation.
#'
#' @param x              the matrix to compute principal components from.
#' @param n              number of components to compute
#' @param random_state   integer to set the random state of the algorithm
#' @param item_name      name to give to the set of rows; defaults to "document"
#' @param ...            other options passed to the umap function
#'
#' @return  a tibble with one row for each observation.
#'
#' @examples
#' mat <- matrix(rnorm(26 * 5), ncol = 5)
#' rownames(mat) <- letters
#' hdir_tidy_umap(mat)
#'
#' @importFrom umap umap
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
hdir_tidy_umap <- function(x, n = 2, random_state = 1, item_name = "document", ...)
{
  x <- as.matrix(x)
  df <- umap::umap(x, n_components = n, random_state = random_state, ...)$layout
  suppressMessages({ df <- tibble::as_tibble(df, .name_repair = "minimal") })
  names(df) <- sprintf("v%d", seq_len(ncol(df)))
  names(df) <- tolower(names(df))
  if (!is.null(rownames(x)))
  {
    df <- dplyr::bind_cols(tibble::tibble(rownames(x)), df)
    names(df)[1] <- item_name
  }
  df
}

#' Minimal theme perfect for presentations
#'
#' A minimalistic theme based on Tufte's *Visual Display of Quantitative Information*.
#'
#' @return  a ggplot2 theme object.
#'
#' @importFrom ggplot2 theme_bw theme element_blank
#' @export
theme_tufte <- function()
{
    ret <- ggplot2::theme_bw(base_family = "sans", base_size = 11) +
        ggplot2::theme(
          legend.background = ggplot2::element_blank(),
          legend.key        = ggplot2::element_blank(),
          panel.background  = ggplot2::element_blank(),
          panel.border      = ggplot2::element_blank(),
          strip.background  = ggplot2::element_blank(),
          plot.background   = ggplot2::element_blank(),
          axis.line         = ggplot2::element_blank(),
          panel.grid        = ggplot2::element_blank(),
          axis.ticks        = ggplot2::element_blank()
        )
    ret
}


#' Tidy a Matrix Object
#'
#' Creates a data frame out of a Matrix that has column and row names. Name
#' defaults assume a term frequency matrix, but it is possible to give other
#' names.
#'
#' @param x           the matrix to tidy.
#' @param rows_to     names of the column containing the rownames; defaults to
#'                    "document"
#' @param cols_to     names of the column containing the rownames; defaults to
#'                    "term"
#' @param values_to   names of the column containing the rownames; defaults to
#'                    "count"
#'
#' @return  a tibble object in a long format.
#'
#' @examples
#' mat <- matrix(rnorm(26 * 5), ncol = 5)
#' colnames(mat) <- letters[seq_len(5)]
#' rownames(mat) <- letters
#' hdir_tidy_matrix(mat)
#'
#' @importFrom tibble tibble
#' @export
hdir_tidy_matrix <- function(
  x, rows_to = "document", cols_to = "term", values_to = "count"
) {
  if (is.null(rownames(x))) stop("input must have row names")
  if (is.null(colnames(x))) stop("input must have column names")

  x <- as.matrix(x)
  out <- tibble::tibble(
    var1 = rownames(x)[row(x)],
    var2 = colnames(x)[col(x)],
    var3 = as.numeric(x)
  )

  names(out) <- c(rows_to, cols_to, values_to)
  out
}

#' Create Long Format of Image Pixels 
#'
#' Give a set of paths to images, this function will create
#' a long-format version of the pixels in all the images, 
#' with own row per pixel. The rows contain the red, green,
#' and blue pixel intensities as well as the derivted hue,
#' saturation and value, and a hex description of the color.
#'
#' @param paths     a vector of paths to the images
#' @param names     a vector of names for the images that
#'                  serve as a key to the images; will be
#'                  equal to the paths if not provided 
#'
#' @returns         a tibble object with one row for every pixel
#'                  in every image.
#'
#' @importFrom dplyr bind_rows
#' @importFrom purrr map map2
#' @importFrom ggimg gimg_load_img
#' @importFrom grDevices rgb2hsv rgb
#' @export
hdir_pixels_long <- function(paths, names = NULL)
{
  if (is.null(names)) { names <- paths }
  stopifnot(length(paths) == length(names))

  imgs <- purrr::map(paths, ggimg::gimg_load_img)
  out <- purrr::map2(imgs, names, function(u, v) {
      df <- tibble::tibble(
        filename = v,
        row = as.numeric(row(u[,,1])),
        col = as.numeric(col(u[,,1])),
        nrow = dim(u)[1],
        ncol = dim(u)[2],
        red = as.numeric(u[,,1]),
        green = as.numeric(u[,,2]),
        blue = as.numeric(u[,,3])
      )
      hsv <- grDevices::rgb2hsv(df$red, df$green, df$blue, maxColorValue = 1)
      df$hue <- hsv[1,]
      df$saturation <- hsv[2,]
      df$value <- hsv[3,]
      df$hex <- grDevices::rgb(df$red, df$green, df$blue, 1)

      return(df)  
  }) 
  out <- dplyr::bind_rows(out)
  out
}

#' Save Set of Images as a Grid
#'
#' Given a set of paths to images, this function will create
#' an array of images that can be saved as a JPEG or PNG file.
#'
#' @param paths     a vector of paths to the images
#' @param ncol      number of columns in the output
#' @param nrow      number of rows in the output
#'
#' @returns         an array of pixel intensities.
#'
#' @importFrom ggimg gimg_load_img
#' @importFrom purrr map map_int
#' @export
hdir_plot_images <- function(paths, ncol = NULL, nrow = NULL)
{
  if (is.null(ncol) & is.null(nrow))
  {
    ncol <- ceiling(sqrt(length(paths)))
  }
  if (is.null(ncol))
  {
    ncol <- ceiling(length(paths) / nrow)
  }
  if (is.null(nrow))
  {
    nrow <- ceiling(length(paths) / ncol)
  }

  stopifnot(ncol*nrow >= length(paths))
  imgs <- purrr::map(paths, ggimg::gimg_load_img)

  nr <- purrr::map_int(imgs, ~ nrow(..1))
  nc <- purrr::map_int(imgs, ~ ncol(..1))
  nrm <- max(nr)
  ncm <- max(nc)

  out <- array(data = 0L, dim = c(nrm * nrow, ncm * ncol, 3L))
  idx <- (seq_along(paths) - 1L) %% ncol
  idy <- (seq_along(paths) - 1L) %/% ncol

  for (j in seq_along(paths))
  {
    out[
      seq_len(nr[j]) + idy[j] * nrm,
      seq_len(nc[j]) + idx[j] * ncm,
      1L:3L
    ] <- imgs[[j]]
  }

  out
}

#' Initialize the Pipeline of Annotations
#'
#' Sets up the annotations and makes sure that all of the 
#' models are setup and working.
#'
#' @param annotations     a vector of annotations to run. Possible
#'                        options are "object", "face", "pose", and
#'                        "embed".
#'
#' @returns         nothing
#' @export
hdir_init_pipeline <- function(annotations = c("object", "face", "pose", "embed"))
{
  # make sure that Python and dvt exist on the system and are
  # set up correctly
  check_python()

  # determine which annotations to load
  vol$pipeline <- list()
  annotations <- match.arg(
    annotations, 
    c("object", "face", "pose", "embed"),
    several.ok = TRUE
  )

  # load the annotations as Python objects
  if ("object" %in% annotations)
  {
    vol$pipeline[['object']] <- vol$dvt$AnnoDetect()
  }
  if ("face" %in% annotations)
  {
    vol$pipeline[['face']] <- vol$dvt$AnnoFaces()
  }
  if ("pose" %in% annotations)
  {
    vol$pipeline[['pose']] <- vol$dvt$AnnoKeypoints()
  }
  if ("embed" %in% annotations)
  {
    vol$pipeline[['embed']] <- vol$dvt$AnnoEmbed()
  }

  # nothing to return
  invisible(NULL)
}

#' Apply Image Algorithms to a Set of Images
#'
#' Applies a set of Python libraries to the set of images.
#'
#' @param paths       a vector of paths to the images
#' @param fnames      an optional set of filenames to use the metadata;
#'                    will be set to the full path names if not provided
#' @param outdir      directory to store the output in; otherwise will 
#'                    store in a temporary directory
#' @param verbose     should the function report its progress; defaults
#'                    to TRUE
#' @param prefix      optional prefix for the output filenames
#' @param embed_type  if running the embed annotation, what type of values
#'                    should be returned. Either "dimred", "full", or "all".
#'                    Defaults to "dimred" for more than 50 inputs and "full"
#'                    otherwise. 
#'
#' @returns           directory where the output was saved
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom irlba prcomp_irlba
#' @importFrom umap umap
#' @importFrom FNN get.knnx
#' @importFrom reticulate py_to_r
#' @importFrom readr write_csv
#'
#' @export
hdir_apply_pipeline <- function(
  paths,
  fnames = paths,
  outdir = tempdir(),
  verbose = TRUE,
  prefix = "",
  embed_type = NULL
)
{
  # check input
  .assert(
    !is.null(vol$dvt),
    "Initalize pipeline with `hdir_init_pipeline()` before running this function."
  )
  .assert(
    length(paths) == length(fnames),
    "`paths` and `fnames` do not have the same length."
  )
  .assert(
    all(file.exists(paths)),
    "some elements of `paths` do not exist."
  )
  if (prefix != "") { prefix <- paste0(prefix, "_", collapse = "")}
  if (is.null(embed_type)) { embed_type <- ifelse(length(paths) < 50L, "full", "dimred") }
  embed_type <- match.arg(embed_type, c("dimred", "full", "all"))
  
  # run annotations over each image
  df_object <- vector("list", length(paths))
  df_face <- vector("list", length(paths))
  df_pose <- vector("list", length(paths))
  if ('embed' %in% names(vol$pipeline))
  {
    mat_embed <- matrix(NA_real_, nrow = length(paths), ncol = 1280L)
  }

  df_embed <- vector("list", length(paths))
  for (j in seq_along(paths))
  {
    img <- vol$dvt$load_image(paths[j])
    idims <- dim(reticulate::py_to_r(img))

    if ('object' %in% names(vol$pipeline)) {
      obj <- vol$pipeline[['object']]$run(img)  
      obj <- reticulate::py_to_r(obj)

      if (length(obj$x))
      {
        df_object[[j]] <- tibble::tibble(
          filename = fnames[j],
          index = seq_along(obj$x) - 1L,
          height = idims[1],
          width = idims[2],
          class = as.character(obj$labels),
          prob = as.numeric(obj$scores),
          x0 = as.integer(obj$x),
          y0 = as.integer(obj$y),
          x1 = as.integer(obj$xend), 
          y1 = as.integer(obj$yend)
        )
      }
    }

    if ('face' %in% names(vol$pipeline)) {
      obj <- vol$pipeline[['face']]$run(img)   
      obj <- reticulate::py_to_r(obj)
      if (length(obj))
      {
        df_face[[j]] <- tibble::tibble(
          filename = fnames[j],
          index = seq_along(obj$boxes$x) - 1L,
          x0 = as.integer(obj$boxes$x),
          y0 = as.integer(obj$boxes$y),
          x1 = as.integer(obj$boxes$xend), 
          y1 = as.integer(obj$boxes$yend),
          prob = as.numeric(obj$boxes$prob),
          height = idims[1],
          width = idims[2]
        )
      }
    }

    hval <- wval <- 0
    if ('pose' %in% names(vol$pipeline)) {
      obj <- vol$pipeline[['pose']]$run(img)      
      obj <- reticulate::py_to_r(obj)
      if (length(obj$kpnt$person_id))
      {
        idx <- obj$kpnt$kpnt_id
        df_pose[[j]] <- tibble::tibble(
          filename = fnames[j],
          index = seq_along(obj$kpnt$person_id) - 1L,
          kpname = .knames[as.integer(idx) + 1L],
          x = as.integer(obj$kpnt$x),
          y = as.integer(obj$kpnt$y),
          score = as.numeric(obj$kpnt$prob),
          height = idims[1],
          width = idims[2]
        )
      }
    }
    if ('embed' %in% names(vol$pipeline)) {
      obj <- vol$pipeline[['embed']]$run(img)      
      mat_embed[j,] <- reticulate::py_to_r(obj)[[1]]
    }

    if (verbose) cat(sprintf("Finished with %s\n", fnames[j]))
  }

  df_object <- dplyr::bind_rows(df_object)
  df_face <- dplyr::bind_rows(df_face)
  df_pose <- dplyr::bind_rows(df_pose)
  
  if (nrow(df_object))
  {
    readr::write_csv(df_object, file.path(outdir, sprintf("%snn_inst.csv.bz2", prefix)))
  }
  if (nrow(df_face))
  {
    readr::write_csv(df_face, file.path(outdir, sprintf("%snn_face.csv.bz2", prefix)))
  }
  if (nrow(df_pose))
  {
    readr::write_csv(df_pose, file.path(outdir, sprintf("%snn_pose.csv.bz2", prefix)))
  }
  if (('embed' %in% names(vol$pipeline)) & embed_type != "full") {
    # PCA
    X <- irlba::prcomp_irlba(t(mat_embed), n = 25L, scale. = TRUE)$rotation
    suppressMessages({ df_pca <- tibble::as_tibble(X, .name_repair = "minimal") })
    names(df_pca) <- sprintf("pca_%02d", seq_len(ncol(df_pca)))
    df_pca <- bind_cols(tibble(filename = fnames), df_pca)
    readr::write_csv(df_pca, file.path(outdir, sprintf("%sembd_pca.csv.bz2", prefix)))

    # UMAP
    df_umap <- umap::umap(mat_embed, n_components = 2L, random_state = 1L)$layout
    suppressMessages({ df_umap <- tibble::as_tibble(df_umap, .name_repair = "minimal") })
    names(df_umap) <- sprintf("umap_%02d", seq_len(ncol(df_umap)))
    df_umap <- bind_cols(tibble(filename = fnames), df_umap)
    readr::write_csv(df_umap, file.path(outdir, sprintf("%sembd_umap.csv.bz2", prefix)))

    # KNN
    nn <- min(50L, nrow(X) - 1L)
    out <- FNN::get.knnx(X, X, k = nn + 1L)
    df_knn <- tibble::tibble(
      filename = rep(fnames, each = nn),
      rank = rep(seq_len(nn), length(fnames)),
      filename_n = fnames[as.integer(t(out$nn.index[,-1]))],
      distance = as.numeric(t(out$nn.dist[,-1]))
    )
    readr::write_csv(df_knn, file.path(outdir, sprintf("%sknn.csv.bz2", prefix)))
  }
  if (('embed' %in% names(vol$pipeline)) & embed_type != "dimred") {
    colnames(mat_embed) <- sprintf("embed_%04d", seq_len(ncol(mat_embed)))
    suppressMessages({ df_embed <- tibble::as_tibble(mat_embed) })
    df_embed <- bind_cols(tibble(filename = fnames), df_embed)
    readr::write_csv(df_embed, file.path(outdir, sprintf("%sembd_full.csv.bz2", prefix)))
  }

  return(outdir)
}

.knames <- c("nose", "left_eye", "right_eye", "left_ear", "right_ear",
  "left_shoulder", "right_shoulder", "left_elbow", "right_elbow",
  "left_wrist", "right_wrist", "left_hip", "right_hip", "left_knee",
  "right_knee", "left_ankle", "right_ankle")

