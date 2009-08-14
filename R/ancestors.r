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
    todo <<- c(core(nxt)$get_local_slot("protos"), todo)
    remove_seen()
    
    nxt
  }
  
  # Could make all more efficienct by naming based on environment hash
  # Then operations would be setdiffs based on names
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