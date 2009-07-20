Widget <- Object$clone()$do({
  self$init <- function() {
    self$expand <- FALSE
    self$anchor <- c(0, 0)
    self$widget <- self$build_widget()
  }
  
  #' @TODO Fix scoping issue here
  self$add <- function(widget, expand = widget$expand, anchor = widget$anchor) {
    gWidgets::add(self$widget, widget$widget, expand = expand, anchor = anchor)
    self
  }
  
  # Should be override in children to create the appropriate widget.
  self$build_widget <- function() {}
  
  self$set_slot <- function(name, value) {
    # If the object is identical, don't need to set it.  This happens when
    # processing a$b$c <- "d"
    if (identical(value, self$get_slot(name))) return()
    
    # If a widget, add to container widget
    if (is.io(value) && value$has_slot("build_widget")) {
      self$add(value, self$expand, self$anchor)
    }

    # Use usual setting process - 
    # FIXME: use parent not copy and paste
    settor <- paste("set", name, sep = "_")
    if (self$has_slot(settor)) {
      self$get_slot(settor)(value)
    } else {
      core(self)$set_slot(name, value)      
    }
    self
  }
  
  self$set_visible <- function(value) {
    visible(self$widget) <- value
    self
  }
  
  self$get_visible <- function(value) {
    visible(self$widget)
  }
  
  self$set_enabled <- function(value) {
    enabled(self$widget) <- value
    self
  }
  self$get_enabled <- function() {
    enabled(self$widget)
  }
  
  self$set_tooltip <- function(value) {
    tooltip(self$widget) <- value
    self
  }
  self$get_tooltip <- function(value) {
    tooltip(self$widget)
  }
  
})
