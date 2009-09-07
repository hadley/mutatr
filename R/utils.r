envname <- function(env) {
  gsub("<environment: |>", "", utils::capture.output(print(env))[1])
}

is.mutatr <- function(x) inherits(x, "mutatr")
