source("object-inheritance.r")

Ancestors <- Object$clone()$do({
  self$init <- function(proto) {
    self$todo <- list(proto)
    self$done <- list()
  }
  
  self$has_next <- function() {
    length(self$todo) > 0
  }
  
  self$get_next <- function() {
    if (!self$has_next()) return(NULL)
    
    nxt <- self$pop()
    
    self$done <- c(self$done, list(nxt))
    self$todo <- c(nxt$protos, self$todo)
    self$remove_seen()
    
    nxt
  }
  
  self$remove_seen <- function() {
    if (length(self$todo) == 0) return()
    seen <- unlist(lapply(self$todo, self$seen))
    self$todo <- self$todo[!seen]
  }
  
  self$seen <- function(proto) {
    any(sapply(self$done, identical, proto))
  }
  
  self$pop <- function() {
    pop <- self$todo[[1]]
    self$todo <- self$todo[-1]
    pop
  }
})

Object$name <- "Object"
a <- Object$clone()$do({self$name <- "a"})
b <- a$clone()$do({self$name <- "b"})
c <- b$clone()$do({self$name <- "c"})

i <- Ancestors$clone(c)
while(i$has_next()) {
  print(i$get_next()$name)
}

d <- Object$clone()$do({self$name <- "d"})
e <- Object$clone()$do({self$name <- "e"})
e$append_proto(d)
d$append_proto(e)

i <- Ancestors$clone(d)
while(i$has_next()) {
  print(i$get_next()$name)
}
i <- Ancestors$clone(e)
while(i$has_next()) {
  print(i$get_next()$name)
}
