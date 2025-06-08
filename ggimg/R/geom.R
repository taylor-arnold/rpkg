#' Display Images from Bounding Boxes
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
#' @param stat         The statistical transformation to use on the data for
#'                     this layer, as a string.
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
#' @param ...          Other arguments passed on to [layer()]. These are often
#'                     aesthetics, used to set an aesthetic to a fixed value.
#' @param interpolate  A logical value indicating whether to linearly
#'                     interpolate the image (the alternative is to use
#'                     nearest-neighbour interpolation, which gives a more
#'                     blocky result).
#'@examples
#'
#' library(ggplot2)
#' posters$path <- file.path(
#'   system.file("extdata", package="ggimg"), posters$img
#' )
#' p_paths <- ggplot(posters) +
#'  geom_rect_img(aes(
#'     xmin = year - 0.5,
#'     xmax = year + 0.5,
#'     ymin = stars - 0.5,
#'     ymax = stars + 0.5,
#'     img = path
#'   ))
#'
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @export
geom_rect_img <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  interpolate = TRUE
) {
  ggplot2::layer(
    geom = GeomRectImage,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(interpolate = interpolate, ...)
  )
}

#' @export
#' @rdname geom_rect_img
GeomRectImage <- ggplot2::ggproto(
  "GeomRectImage",
  ggplot2::Geom,
  required_aes = c("xmin", "xmax", "ymin", "ymax", "img"),
  non_missing_aes = c("alpha"),
  default_aes = ggplot2::aes(
    alpha = 1
  ),

  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_params, coord, interpolate = TRUE) {
    coords <- coord$transform(data, panel_params)
    grobs <- vector("list", length(coords$img))
    class(grobs) <- "gList"
    for (i in seq_along(grobs))
    {
      img <- gimg_load_img(coords$img[i])
      img <- fix_img_dims(img, alpha = coords$alpha[i])

      grobs[[i]] <- grid::rasterGrob(
        img,
        coords$xmin[i],
        coords$ymin[i],
        coords$xmax[i] - coords$xmin[i],
        coords$ymax[i] - coords$ymin[i],
        hjust = 0,
        vjust = 0,
        interpolate = interpolate
      )
    }
    return(grobs)
  }
)


#' Display Images from Bounding Boxes
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
#' @param stat         The statistical transformation to use on the data for
#'                     this layer, as a string.
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
#' @param ...          Other arguments passed on to [layer()]. These are often
#'                     aesthetics, used to set an aesthetic to a fixed value.
#' @param hjust        A numeric vector specifying horizontal justification. 0
#'                     means left alignment and 1 means right alignment. The
#'                     default of 0.5 gives center alignment.
#' @param vjust        A numeric vector specifying vertical justification. 0
#'                     means left alignment and 1 means right alignment. The
#'                     default of 0.5 gives center alignment.
#' @param along        Either 'width' (default) or 'height'. For the point
#'                     method, the aspect ratio of the image will be preserved.
#'                     Specifies whether size should be relative to the plot's
#'                     with or height.
#' @param interpolate  A logical value indicating whether to linearly
#'                     interpolate the image (the alternative is to use
#'                     nearest-neighbour interpolation, which gives a more
#'                     blocky result).
#'@examples
#'
#' library(ggplot2)
#' posters$path <- file.path(
#'   system.file("extdata", package="ggimg"), posters$img
#' )
#' p_paths <- ggplot(posters) +
#'  geom_point_img(aes(
#'     x = year,
#'     y = stars,
#'     img = path
#'   ), size = 1.1)
#'
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @export
geom_point_img <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  hjust = 0.5,
  vjust = 0.5,
  along = "width",
  interpolate = TRUE
) {
  ggplot2::layer(
    geom = GeomPointImage,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      hjust = hjust,
      vjust = vjust,
      along = along,
      interpolate = interpolate,
      ...
    )
  )
}


#' @export
#' @rdname geom_point_img
GeomPointImage <- ggplot2::ggproto(
  "GeomPointImage",
  ggplot2::Geom,
  required_aes = c("x", "y", "img"),
  non_missing_aes = c("alpha", "size"),
  default_aes = ggplot2::aes(
    alpha = 1,
    size = 1
  ),

  draw_key = ggplot2::draw_key_point,

  draw_panel = function(
    data,
    panel_params,
    coord,
    hjust = 0.5,
    vjust = 0.5,
    along = "width",
    interpolate = TRUE
  ) {
    along <- match.arg(along, c("width", "height"))

    coords <- coord$transform(data, panel_params)
    grobs <- vector("list", length(coords$img))
    class(grobs) <- "gList"
    for (i in seq_along(grobs))
    {
      img <- gimg_load_img(coords$img[i])
      img <- fix_img_dims(img, alpha = coords$alpha[i])

      if (along == "width")
      {
        grobs[[i]] <- grid::rasterGrob(
          img,
          coords$x[i],
          coords$y[i],
          width = coords$size[i] / 20,  # points 5% of the x-axis
          hjust = hjust,
          vjust = vjust,
          interpolate = interpolate
        )
      } else {
        grobs[[i]] <- grid::rasterGrob(
          img,
          coords$x[i],
          coords$y[i],
          height = coords$size[i] / 20,  # points 5% of the y-axis
          hjust = hjust,
          vjust = vjust,
          interpolate = interpolate
        )
      }

    }
    return(grobs)
  }
)
