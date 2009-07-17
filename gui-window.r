source("gui-widget.r")

Window <- Widget$clone()$do({
    
  build_widget <- function() {
    gwindow(visible = FALSE)
  }
  
  set_title <- function(title) {
    svalue(self$widget) <- title
  }

  get_title <- function() {
    svalue(self$widget)
  }
  
  set_size <- function(width, height) {
    size(self$widget) <- c(width, height)
  }
  
  get_size <- function() {
    size(self$widget)
  }
  
  set_default <- function(widget) {
    defaultWidget(self$widget) <- widget$widget
  }
  
  # Makes the window visible by setting visible to true.
  show <- function() {
    self$set_visible(TRUE)
  }
  
  # Close window.  
  close <- function() {
    dispose(self$widget)
  }
  
  # Probably need to munge these events to ensure that the callback gets
  # a reference to this object - that is what they should be working with.
  add_handler <- function(event, callback) {
    switch(event, 
      on_destroy = addhandlerdestroy(self$widget, callback),
      on_unrealize = addhandleunrealise(self$widget, callback),
      stop("Unknown event", event, call. = FALSE)
    )
    self 
  }
})
