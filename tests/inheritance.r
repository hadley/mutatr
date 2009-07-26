a <- Object$clone()$do({
  self$get_hadley <- function() runif(10)
  self$set_hadley <- function(value) stop("Not allowed")
})
b <- a$clone()
c <- b$clone()
a$a <- 1

stopifnot(identical(b$a, 1))
stopifnot(identical(c$a, 1))

b$a <- 2
stopifnot(identical(a$a, 1))
stopifnot(identical(b$a, 2))
stopifnot(identical(c$a, 2))

# 
# source("../examples/observer.r", chdir = T)
# 
# a <- Object$clone()
# expect_error(a$signal())
# 
# a$append_proto(Observable)
# a$add_listener(function() print("Hi"), "hi")
# a$signal()
# 
# a$remove_proto(Observable)
# expect_error(a$signal())
# 
# sup <- Observable$clone()
# sup$add_listener(function() print("Wassup"), "yo")
# sup$signal()
# 
# a$append_proto(sup)
# a$signal()
# a$remove_slot("listeners")
# a$signal()
# a$add_listener(function() print("Testing"), "123")
# a$signal()
