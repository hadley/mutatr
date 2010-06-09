#' Environment name.
#' Extract the name of an environment from its printed output.
#'
#' @param env environment
#' @keywords internal
envname <- function(env) {
  gsub("<environment: |>", "", utils::capture.output(print(env))[1])
}

#' Is this a mutatr object?
#'
#' @param x object to test
is.mutatr <- function(x) inherits(x, "mutatr")

core <- function(x) x[[1]]