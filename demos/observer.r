source("../object-inheritance.r", chdir = TRUE)

Observable <- Object$clone()$do({
  .name <- "Observable"
  listeners <- list()
  add_listener <- function(f, name = digest::digest(f)) {
    self$listeners[[name]] <- f
  }

  signal <- function(...) {
    for(l in self$listeners) l(...)
  }    
  
  init <- function() {
    self$listeners <- list()
  }
})