ancestor_iterator <- function(proto) {
  todo <- list(proto)
  done <- list()
  
  has_next <- function() {
    length(todo) > 0
  }
  
  get_next <- function() {
    if (!has_next()) return(NULL)
    
    nxt <- pop()
    
    done <<- c(done, list(nxt))
    todo <<- c(nxt$protos, todo)
    remove_seen()
    
    nxt
  }
  
  remove_seen <- function() {
    if (length(todo) == 0) return()
    seen <- unlist(lapply(todo, seen))
    todo <<- todo[!seen]
  }
  
  seen <- function(proto) {
    any(sapply(done, identical, proto))
  }
  
  pop <- function() {
    pop <- todo[[1]]
    todo <<- todo[-1]
    pop
  }
  
  list(has_next = has_next, get_next = get_next)
}

# Object$name <- "Object"
# a <- Object$clone()$do({self$name <- "a"})
# b <- a$clone()$do({self$name <- "b"})
# c <- b$clone()$do({self$name <- "c"})
# 
# i <- ancestor_iterator(c)
# while(i$has_next()) {
#   print(i$get_next()$name)
# }
# 
# d <- Object$clone()$do({self$name <- "d"})
# e <- Object$clone()$do({self$name <- "e"})
# e$append_proto(d)
# d$append_proto(e)
# 
# i <- ancestor_iterator(d)
# while(i$has_next()) {
#   print(i$get_next()$name)
# }
# i <- ancestor_iterator(e)
# while(i$has_next()) {
#   print(i$get_next()$name)
# }
