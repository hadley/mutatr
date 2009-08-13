Label <- Widget$clone()$do({
  
  self$init <- function(label = "") {
    self$widget <- self$build_widget()
    self$label <- label
  }

  self$build_widget <- function() {
    glabel(markup = TRUE)
  }
  
  self$set_label <- function(value) {
    svalue(self$widget) <- value
  }
  self$get_label <- function(){
    svalue(self$widget)
  }
  
  self$add_handler <- function(event, callback) {
    switch(event, 
      on_click = addHandlerClicked(self$widget, callback),
      stop("Unknown event", event, call. = FALSE)
    )
    self
  }
  
})