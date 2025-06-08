#' Compute Map Tiles for the Plot Region
#'
#' @param mapping      Set of aesthetic mappings created by [aes()] or
#'                     [aes_()]. If specified and `inherit.aes = TRUE` (the
#'                     default), it is combined with the default mapping at the
#'                     top level of the plot. You must supply `mapping` if there
#'                     is no plot mapping.
#' @param data         The data to be displayed in this layer. There are three
#'                     options:
#'
#'                     If `NULL`, the default, the data is inherited from the
#'                     plot data as specified in the call to [ggplot()].
#'
#'                     A `data.frame`, or other object, will override the plot
#'                     data. All objects will be fortified to produce a data
#'                     frame.
#'
#'                     A `function` will be called with a single argument,
#'                     the plot data. The return value must be a `data.frame`,
#'                     and will be used as the layer data. A `function` can be
#'                     created from a `formula` (e.g. `~ head(.x, 10)`).
#' @param geom         The geometric object to use display the data
#' @param position     Position adjustment, either as a string, or the result of
#'                     a call to a position adjustment function.
#' @param show.legend  logical. Should this layer be included in the legends?
#'                     `NA`, the default, includes if any aesthetics are mapped.
#'                     `FALSE` never includes, and `TRUE` always includes.
#'                     It can also be a named logical vector to finely select
#'                     the aesthetics to display.
#' @param inherit.aes  If `FALSE`, overrides the default aesthetics,
#'                     rather than combining with them. This is most useful for
#'                     helper functions that define both data and aesthetics
#'                     and shouldn't inherit behaviour from the default plot
#'                     specification, e.g. [borders()].
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
#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' french_city %>%
#'   ggplot(aes(lng, lat)) +
#'     stat_maptiles(
#'       cache_dir = system.file("extdata", package="ggmaptile")
#'     ) +
#'     geom_point(color = "orange") +
#'     theme_void() +
#'     mapview()
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @importFrom ggimg GeomRectImage
#' @export
stat_maptiles <- function(
  mapping = NULL,
  data = NULL,
  geom = ggimg::GeomRectImage,
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  zoom = NULL,
  zoom_adj = 0,
  aspect_ratio = NULL,
  cache_dir = NULL,
  alpha = 1,
  url = NULL,
  force = FALSE,
  quiet = FALSE
) {
  c(
    ggplot2::layer(
      stat = StatMapTiles,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        zoom = zoom,
        zoom_adj = zoom_adj,
        aspect_ratio = aspect_ratio,
        cache_dir = cache_dir,
        alpha = alpha,
        url = url,
        force = force,
        quiet = quiet
      )
    ),
    ggplot2::coord_quickmap()
  )
}

#' @export
#' @rdname stat_maptiles
StatMapTiles <- ggplot2::ggproto(
  "StatMapTiles",
  ggplot2::Stat,

  compute_group = function(
    data,
    scales,
    zoom = NULL,
    zoom_adj = 0,
    aspect_ratio = NULL,
    cache_dir = NULL,
    alpha = 1,
    url = NULL,
    force = FALSE,
    quiet = FALSE
  ) {

    mt_map_extent_data(
      min(scales$x$get_limits()),
      max(scales$x$get_limits()),
      min(scales$y$get_limits()),
      max(scales$y$get_limits()),
      zoom = zoom,
      zoom_adj = zoom_adj,
      aspect_ratio = aspect_ratio,
      cache_dir = cache_dir,
      alpha = alpha,
      url = url,
      force = force,
      quiet = quiet
    )
  },

  required_aes = c("x", "y")
)
