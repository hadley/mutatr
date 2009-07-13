envname <- function(env) {
  gsub("<environment: |>", "", utils::capture.output(print(env))[1])
}

envlist <- function(env) {
  if (is.emptyenv(env)) return()
  if (is.globalenv(env)) return(list(env))
  
  c(env, envlist(parent.env(env)))
}
is.emptyenv <- function(env) identical(env, emptyenv())
is.globalenv <- function(env) identical(env, globalenv())
is.baseend <- function(env) identical(env, baseenv())

as.io <- function(x) UseMethod("as.io")
as.io.environment <- function(x) {
  # Return unchanged if not really an io object
  # if (!exists("get_slot", x))  return(x)

  structure(list(x), class = "io")
}

is.io <- function(x) inherits(x, "io")