#' Compute and Download Map Tiles for Bounding Box
#'
#' Given a range of x and y values (which correspond to longitude and latitude,
#' respecitively), the function determines a set of map tiles that cover the
#' region. Tiles are downloaded from a URL and stored in a local cache. A
#' data frame suitable for plotting with \code{geom_image_bbox} is returned.
#'
#' @param xmin         minimum value of the bounding box to cover: degrees of
#'                     longitude
#' @param xmax         maximum value of the bounding box to cover: degrees of
#'                     longitude
#' @param ymin         minimum value of the bounding box to cover: degrees of
#'                     latitude
#' @param ymax         maximum value of the bounding box to cover: degrees of
#'                     latitude
#' @param zoom         integer giving the zoom value, from 0 (entire world) to
#'                     19; set to NULL to auto-compute an appropriate zoom level
#' @param zoom_adj     integer giving an adjustment to the default zoom level;
#'                     negative values zoom out from the default and positive
#'                     values zoom into the default. Useful to avoid
#'                     accidentally downloading a large number of tiles (by
#'                     setting a very large zoom value for a region) and for
#'                     adjusting facetted plots. Ignored when an explicit zoom
#'                     is supplied.
#' @param aspect_ratio positive numeric value giving the desired aspect ratio of
#'                     the tiles. When set to NULL, the default, tiles are
#'                     choosen to cover the data points without regard to the
#'                     aspect ratio.
#' @param cache_dir    string giving the location of a local path in which to
#'                     download the tile files; if set to NULL, will first check
#'                     for the option "mt_cache_dir"; if also missing,
#'                     will determine a suitable temporary location that
#'                     persistsduring the R session
#' @param alpha        optacity of the tiles; default is 1 (solid).
#' @param url          url of the tile server, given as a string with three
#'                     values of "\%d", which will be filled in with the zoom,
#'                     x, and y values; if missing, will taken from the option
#'                     "mt_url"; otherwise will be set to:
#'                     "http://tile.stamen.com/toner/\%d/\%d/\%d.png".
#' @param force        logical; should tiles be downloaded if they already
#'                     exist? Defaults to FALSE. Useful if the url is changed
#'                     or there was a network issue with the tiles.
#' @param quiet        logical; should a message be shown indicating what tiles
#'                     are being downloaded and used? Set to FALSE by default.
#'
#' @return             Returns a data frame with one row per tile, containing
#'                     a bounding box of the tile and the local location where
#'                     the file is located.
#'
#' @export
mt_map_extent_data <- function(
  xmin,
  xmax,
  ymin,
  ymax,
  zoom = NULL,
  zoom_adj = 0,
  aspect_ratio = NULL,
  cache_dir = NULL,
  alpha = 1,
  url = NULL,
  force = FALSE,
  quiet = FALSE
) {

  # grab or create values for zoom, cache_dir, and url
  if (is.null(cache_dir))
  {
    cache_dir <- getOption(
      "mt_cache_dir",
      tempfile("R.tile.")
    )
    options(mt_cache_dir = cache_dir)
  }
  if (is.null(url))
  {
    url <- getOption(
      "mt_url",
      "https://tiles.stadiamaps.com/tiles/stamen_toner_lite/%d/%d/%d@2x.png"
    )
  }
  if (is.null(zoom))
  {
    zoom <- log(5 * 360 / abs(xmax - xmin)) / log(2) + zoom_adj
    zoom <- as.integer(max(c(min(c(zoom, 19L)), 0)))
  }

  out_ext <- substr(url, nchar(url) - 2L, nchar(url))
  cache_path <- file.path(
    cache_dir,
    paste(c("%d/%d/%d.", out_ext), collapse = "")
  )
  xtile_min <- as.integer(floor(lon_to_xtile(xmin, zoom = zoom)))
  xtile_max <- as.integer(ceiling(lon_to_xtile(xmax, zoom = zoom)))
  ytile_min <- as.integer(floor(lat_to_ytile(ymax, zoom = zoom)))
  ytile_max <- as.integer(ceiling(lat_to_ytile(ymin, zoom = zoom)))

  if (!is.null(aspect_ratio))
  {
    x_size <- xtile_max - xtile_min
    y_size <- ytile_max - ytile_min
    current_asp <- x_size / y_size

    if (current_asp < aspect_ratio) # need to add to the number of x tiles
    {
      x_bump <- (((y_size * aspect_ratio) - x_size) / 2)
      xtile_min <- xtile_min - floor(x_bump)
      xtile_max <- xtile_max + ceiling(x_bump)
    }
    if (current_asp > aspect_ratio) # need to add to the number of y tiles
    {
      y_bump <- (((x_size / aspect_ratio) - y_size) / 2)
      ytile_min <- ytile_min - floor(y_bump)
      ytile_max <- ytile_max + ceiling(y_bump)
    }
  }

  gr <- expand.grid(seq(xtile_min, xtile_max), seq(ytile_min, ytile_max))
  path_url <- sprintf(url, zoom, gr$Var1, gr$Var2)
  path_cache <- sprintf(cache_path, zoom, gr$Var1, gr$Var2)
  path_cache <- gsub("@2x", "", path_cache)

  num_tiles <- length(path_cache)
  num_tiles_to_grab <- sum(!file.exists(path_cache))
  if (!quiet)
  {
    message(
      sprintf(
        paste(c(
          "Grabbing tiles:\n",
          "  => url:             %s\n",
          "  => zoom:            %d\n",
          "  => total tiles:     %d\n",
          "  => num to download: %d"
        ), collapse = ""),
        url,
        zoom,
        num_tiles,
        num_tiles_to_grab
      )
    )
  }
  download_paths(path_url, path_cache, force = force)

  df <- data.frame(
    xmin = xtile_to_lon(gr$Var1, zoom = zoom),
    xmax = xtile_to_lon(gr$Var1 + 1, zoom = zoom),
    ymin = ytile_to_lon(gr$Var2 + 1, zoom = zoom),
    ymax = ytile_to_lon(gr$Var2, zoom = zoom),
    alpha = alpha,
    img = path_cache,
    stringsAsFactors = FALSE
  )

  return(df)
}

# download file paths from "path_url" into "path_url"; skip if file exists
# unless force = TRUE
download_paths <- function(path_url, path_lcl, force = FALSE)
{
  for (i in seq_along(path_url))
  {
    if (!dir.exists(dirname(path_lcl[i])))
    {
      dir.create(dirname(path_lcl[i]), recursive=TRUE)
    }
    if (!file.exists(path_lcl[i]) | force)
    {
      puf <- paste0(path_url[i], "?api_key=", Sys.getenv("GGMAP_API_KEY"))
      utils::download.file(
        puf, path_lcl[i], quiet = TRUE
      )
    }
  }
}

# compute map x-tile (as float) for a longitude in degrees
lon_to_xtile <- function(lon, zoom)
{
  floor(((lon + 180) / 360) * (2 ^ zoom))
}

# compute map y-tile (as float) for a latitude in degrees
lat_to_ytile <- function(lat, zoom)
{
  lat_rad <- lat / 180 * pi
  floor((1 - (log(tan(lat_rad) + 1 / cos(lat_rad)) / pi)) / 2 * (2 ^ zoom))
}

# compute latitude in degrees of a map tile's lower left corner
xtile_to_lon <- function(x, zoom)
{
  n <- 2 ^ zoom
  x / n * 360.0 - 180.0
}

# compute longitude in degrees of a map tile's lower left corner
ytile_to_lon <- function(y, zoom)
{
  n <- 2 ^ zoom
  atan(sinh(pi * (1 - 2 * y / n))) * 180.0 / pi
}
