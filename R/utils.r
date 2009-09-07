envname <- function(env) {
  gsub("<environment: |>", "", utils::capture.output(print(env))[1])
}

is.io <- function(x) inherits(x, "io")
