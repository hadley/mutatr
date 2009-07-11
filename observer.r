Observable <- Object$clone()$do({
  add_listener <- function(f, name = digest(f)) {
    self$listeners[[name]] <- f
  }
  remove_listener <- function(name) {
    self$listeners[[name]] <- NULL    
  }

  signal <- function(...) {
    for(l in self$listeners) l(...)
  }    
  
  init <- function() {
    self$listeners <- list()
  }
})