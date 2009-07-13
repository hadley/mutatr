expect_error <- function(x) {
  res <- try(force(x), TRUE)
  stopifnot(inherits(res, "try-error"))
}

source("object.r"); source("observer.r")

a <- Object$clone()
expect_error(a$signal())

a$append_proto(Observable)
a$add_listener(function() print("Hi"), "hi")
a$signal()

a$remove_proto(Observable)
expect_error(a$signal())


hello <- Observable$clone()
hello$add_listener(function() print("Wassup"), "yo")
hello$signal()


a$append_proto(hello)
a$signal()
a$remove_slot("listeners")
a$signal()
a$add_listener(function() print("Testing"), "123")
a$signal()
