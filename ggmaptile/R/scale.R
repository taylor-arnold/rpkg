#' Set Scales for Map View
#'
#' Add this as a layer to a ggplot object, along with \code{theme_void}, in
#' order to show only the map and none of the axes.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @export
mapview <- function() {
  c(
    ggplot2::scale_x_continuous(expand = c(0, 0)),
    ggplot2::scale_y_continuous(expand = c(0, 0)),
    ggplot2::labs(x = NULL, y = NULL)
  )
}
