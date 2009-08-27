all_ancestors <- function(obj) {
  i <- ancestor_iterator(obj)
  i$get_next() # Skip self
  
  ancestors <- c()
  while(i$has_next()) {
    ancestors <- c(ancestors, i$get_next()$name)
  }
  ancestors
}

assert_has_ancestors <- function(obj, ancestors) {
  stopifnot(identical(all_ancestors(obj), ancestors))
}

# Test simple hierarchical case
Object$name <- "Object"
a <- Object$clone()$do({self$name <- "a"})
b <- a$clone()$do({self$name <- "b"})
c <- b$clone()$do({self$name <- "c"})

assert_has_ancestors(a, "Object")
assert_has_ancestors(b, c("a", "Object"))
assert_has_ancestors(c, c("b", "a", "Object"))

# Test circular case
d <- Object$clone()$do({self$name <- "d"})
e <- Object$clone()$do({self$name <- "e"})
e$append_proto(d)
d$append_proto(e)

assert_has_ancestors(e, c("Object", "d"))
assert_has_ancestors(d, c("Object", "e"))
