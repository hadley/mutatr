Window <- Widget$clone()$do({
  init <- function() {
    self$handlers <- NULL
  }
  
  events <- c("destroy", "unrealize")
  handlers <- list()
  
  __set_title <- function(title) {
    svalue(self$widget) <- title
  }
    
  build_widget <- function() {
    gwindow(self$title, self$visible,
      width = self$width, height = self$height, parent = self$parent)
  }
  
  __set_size <- function(width, height) {
    size(self$widget) <- c(self$width, self$height)
  }
  
  __get_size <- function() {
    size(self$widget)
  }
  
  __set_default <- function(widget) {
    defaultWidget(self$widget) <- widget$widget
  }
  
  # Makes the window visible by setting visible to true.
  show <- function() {
    self$visible <- TRUE
    self
  }
  
  # Close window.  
  close <- function() {
    dispose(self$widget)
    self$_widget <- NULL
  }
  
  # Probably need to munge these events to ensure that the callback gets
  # a reference to this object - that is what they should be working with.
  add_handler <- function(event, callback) {
    switch(event, 
      on_destroy = addhandlerdestroy(self$widget, callback),
      on_unrealize = addhandleunrealise(self$widget, callback),
      stop("Unknown event", event, call. = FALSE)
    })
  }
  
  
})