Button <- Widget$clone()$do({
  
  self$init <- function(label = "") {
    self$widget <- self$build_widget()
    self$label <- label
  }
  
  self$build_widget <- function() {
    gbutton()
  }
  
  self$set_border <- function(value) {
    border(self$widget) <- value
  }
  self$get_border <- function() {
    border(self$widget)
  }
  
  self$set_label <- function(value) {
    svalue(self$widget) <- value
  }
  self$get_label <- function(){
    svalue(self$widget)
  }
  
  self$click <- function() {
    # Is it possible to do this with gwidgets?
    # Or should I just manually run all of the matching handlers (if I can
    # find them), and then set svalue appropriately for each widget
  }
  
  self$add_handler <- function(event, callback) {
    switch(event, 
      on_click = addHandlerClicked(self$widget, callback),
      stop("Unknown event", event, call. = FALSE)
    )
    self
  }
  
})