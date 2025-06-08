vol <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname)
{
  vol$dvt <- NULL
  vol$pipeline <- list()
}

.assert <- function(statement, msg="")
{
  if (!statement)
  {
    stop(msg, call.=(msg==""))
  }
}

#' @importFrom reticulate py_module_available import
check_python <- function() {

  .assert(
    reticulate::py_module_available("dvt"),
    "Python module 'dvt' not found."
  )

  if (is.null(vol$dvt))
  {
    vol$dvt <- reticulate::import("dvt", convert = FALSE)
  }
}