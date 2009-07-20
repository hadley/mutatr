StatusBar <- Widget$clone()$do({
  self$build_widget <- function() {
    gstatusbar()
  }
  
  self$set_label <- function(value) {
    svalue(self$widget) <- value
  }
  self$get_label <- function(){
    svalue(self$widget)
  }
  
  self$add_handler <- function(event, callback) {
    switch(event, 
      on_change = addHandlerChanged(self$widget, callback),
      stop("Unknown event", event, call. = FALSE)
    )
    self
  }
  
})