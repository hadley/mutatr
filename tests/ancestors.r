Object$name <- "Object"
a <- Object$clone()$do({self$name <- "a"})
b <- a$clone()$do({self$name <- "b"})
c <- b$clone()$do({self$name <- "c"})

i <- ancestor_iterator(c)
while(i$has_next()) {
  print(i$get_next()$name)
}

d <- Object$clone()$do({self$name <- "d"})
e <- Object$clone()$do({self$name <- "e"})
e$append_proto(d)
d$append_proto(e)

i <- ancestor_iterator(d)
while(i$has_next()) {
  print(i$get_next()$name)
}
i <- ancestor_iterator(e)
while(i$has_next()) {
  print(i$get_next()$name)
}
