Combobox <- Widget$clone()$do({
  
  self$init <- function(items = c(), editable = FALSE) {
    self$widget <- gcombobox(items, editable = editable)
  }

  self$get_value <- function(value) {
    svalue(self$widget) <- value
  }
  self$set_value <- function(){
    svalue(self$widget)
  }
  
  self$length <- function() {
    length(self$widget)
  }
  
  self$add_handler <- function(event, callback) {
    switch(event, 
      on_change = addHandlerChanged(self$widget, callback),
      stop("Unknown event", event, call. = FALSE)
    )
    self
  }
  
})