volatiles <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname)
{
  volatiles$stringi <- list(init = FALSE, setup = FALSE)
  volatiles$spacy   <- list(init = FALSE, setup = FALSE)
  volatiles$udpipe  <- list(init = FALSE, setup = FALSE)

  volatiles$model_init_last <- ""
}

assert <- function(statement, msg="")
{
  if (!statement)
  {
    stop(msg, call.=(msg==""))
  }
}

ifnull <- function(value, default)
{
  if (is.null(value)) { return(default) }

  return(value)
}

cmsg <- function(verbose, fmt, ...)
{
  if (verbose)
  {
    cat(sprintf(fmt, ...))
  }
}
