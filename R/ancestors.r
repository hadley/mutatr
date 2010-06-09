#' Ancestor iterator.
#' This iterator has two methods \code{has_next} and \code{get_next} and
#' iterates deep-first through the ancestry graph of a mutatr object.
#' 
#' @param proto the object with the ancestors
#' @examples
#' A <- Object$clone()
#' B <- A$clone()
#' C <- B$clone()
#' ai <- mutatr:::ancestor_iterator(C)
#' while(ai$has_next()) print(ai$get_next())
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