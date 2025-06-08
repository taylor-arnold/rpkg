
# Return extension of a file path
file_ext <- function (fpath)
{
    pos <- regexpr("\\.([[:alnum:]]+)$", fpath)
    ifelse(pos > -1L, substring(fpath, pos + 1L), "")
}

#' Load an image
#'
#' Given an input, either a URL, local file path, or a RasterImage itself,
#' return an array of the image file.
#'
#' @param input    the input as either a path, url, or the image itself
#'
#' @return         An array with the image.
#'
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @export
gimg_load_img <- function(input)
{
  if (is.list(input)) {
    img <- input[[1]]
  } else {
    fext <- tolower(file_ext(input))
    prefix <- substr(input, 1, 4)
    if (prefix == "http")
    {
      utils::download.file(input, tf <- tempfile(fileext = fext))
      img <- read_img(tf, fext)
      file.remove(tf)
    } else if (is.character(input) | is.factor(input)) {
      img <- read_img(as.character(input), fext)
    } else {
      stop("Cannot read image file.")
    }
  }

  img
}

# Given an input --- either a URL, local file path, or a RasterImage itself
# --- return an array of the image file
read_img <- function(path, fext)
{
  if ( fext == "png" )
  {
    img <- png::readPNG(path)
  } else if ( fext %in% c("peg", "jpg") ) {
    img <- jpeg::readJPEG(path)
  } else {
    stop(sprintf("Cannot open file %s", path))
  }

  img
}

# Takes an input image file (either an array or matrix), along with a scalar
# alpha value, and returns an array that will always have four channels; set
# alpha to a negative number to avoid overwriting any present opacity values
fix_img_dims <- function(img, alpha)
{
  # Black and white images may be given as a matrix
  if (length(dim(img)) == 2L) img <- array(img, dim = c(dim(img), 1L))

  nchannel <- dim(img)[3]
  if (nchannel == 1L)
  {
    alpha_channel <- array(alpha, dim = c(dim(img)[1:2], 1L))
    img <- abind::abind(img, img, img, alpha_channel, along = 3L)
  } else if (nchannel == 2L) {
    if (alpha >= 0) {
      alpha_channel <- array(alpha, dim = c(dim(img)[1:2], 1L))
    } else {
      alpha_channel <- img[, , 2L, drop=FALSE]
    }
    img <- img[, , 1L, drop=FALSE]
    img <- abind::abind(img, img, img, alpha_channel, along = 3L)
  } else if (nchannel == 3L) {
    alpha_channel <- array(alpha, dim = c(dim(img)[1:2], 1L))
    img <- abind::abind(img, alpha_channel, along = 3L)
  } else if (nchannel == 4L) {
    if (alpha >= 0) img[,,4L] <- alpha
  } else {
    stop(
      sprintf(
        "We do not know how to display an image with %d channels",
        nchannel
      )
    )
  }

  dimnames(img) <- NULL

  return(img)
}
