all_ancestors <- function(obj) {
  i <- ancestor_iterator(obj)
  i$get_next() # Skip self
  
  ancestors <- list()
  while(i$has_next()) {
    ancestors <- c(ancestors, list(i$get_next()))
  }
  ancestors
}
 
has_ancestors <- function(...) {
  ancestors <- list(...)
  function(obj) {
    expectation(
      identical(all_ancestors(obj), ancestors),
      paste("didn't have correct ancestors")
    )
  }
}