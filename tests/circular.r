d <- Object$clone()$do({self$name <- "d"})
e <- Object$clone()$do({self$name <- "e"})
e$append_proto(d)
d$append_proto(e)
e$e <- TRUE
d$d <- TRUE
stopifnot(identical(d$name, "d"))
stopifnot(identical(e$name, "e"))
stopifnot(e$d)
stopifnot(d$e)