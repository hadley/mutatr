source("gui-widget.r")

Window <- Widget$clone()$do({
    
  self$build_widget <- function() {
    gwindow(visible = FALSE)
  }
  
  self$set_title <- function(title) {
    svalue(self$widget) <- title
  }

  self$get_title <- function() {
    svalue(self$widget)
  }
  
  self$set_size <- function(width, height) {
    size(self$widget) <- c(width, height)
  }
  
  self$get_size <- function() {
    size(self$widget)
  }
  
  self$set_default <- function(widget) {
    defaultWidget(self$widget) <- widget$widget
  }
  
  # Makes the window visible by setting visible to true.
  self$show <- function() {
    self$set_visible(TRUE)
  }
  
  # Close window.  
  self$close <- function() {
    dispose(self$widget)
  }
  
  # Probably need to munge these events to ensure that the callback gets
  # a reference to this object - that is what they should be working with.
  self$add_handler <- function(event, callback) {
    switch(event, 
      on_destroy = addhandlerdestroy(self$widget, callback),
      on_unrealize = addhandleunrealise(self$widget, callback),
      stop("Unknown event", event, call. = FALSE)
    )
    self 
  }
})
