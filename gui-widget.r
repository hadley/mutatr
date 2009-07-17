library(gWidgets)
source("object-inheritance.r")

Widget <- Object$clone()$do({
  init <- function() {
    self$expand <- FALSE
    self$anchor <- c(0, 0)
  }
  
  set_slot <- function(name, value) {
    # If a widget, add to container widget
    if (is.io(value) && value$has_slot("parent")) {
      self$add(value, self$expand, self$anchor)
    } 
    core(self)$set_slot(name, value)
  }
  
  
  set_visible <- function(value) {
    visible(self$widget) <- value
    self
  }
  
  get_visible <- function(value) {
    visible(self$widget)
  }
  
  set_enabled <- function(value) {
    enabled(self$widget) <- value
    self
  }
  get_enabled <- function() {
    enabled(self$widget)
  }
  
  get_widget <- function() {
    if (self$has_slot("_widget")) return(self$get_slot("_widget"))
    self$`_widget` <- self$build_widget()
  }
  
  set_tooltip <- function(value) {
    tooltip(self$widget) <- value
    self
  }
  get_tooltip <- function(value) {
    tooltip(self$widget)
  }
  
})