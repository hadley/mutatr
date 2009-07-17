library(gwidgets)

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
    parent$set_slot(name, value)      
  }
  
  
  __set_visible <- function(value) {
    visible(self$widget) <- value
    self
  }
  
  __get_visible <- function(value) {
    visible(self$widget)
  }

  __set_enabled <- function(value) {
    enabled(self$widget) <- value
    self
  }
  __get_enabled <- function() {
    enabled(self$widget)
  }
  
  __get_widget <- function() {
    if (!is.null(self$_widget)) return(self$_widget)
    self$_widget <- self$build_widget()
  }
  
  __set_tooltip <- function(value) {
    tooltip(self$widget) <- value
    self
  }
  __get_tooltip <- function(value) {
    tooltip(self$widget)
  }
  
})