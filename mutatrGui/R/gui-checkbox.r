Checkbox <- Widget$clone()$do({
  
  self$init <- function(label = "") {
    self$widget <- self$build_widget()
    # self$label <- label
  }
  
  self$build_widget <- function() {
    gcheckbox("Hi")
  }

  self$set_value <- function(value) {
    svalue(self$widget) <- value
  }
  self$get_value <- function(){
    svalue(self$widget)
  }
  
  self$set_label <- function(value) {
    self$widget[1] <- value
  }
  self$get_label <- function(){
    self$widget[1]
  }
  
  self$length <- function() {
    length(self$widget)
  }
  
  self$add_handler <- function(event, callback) {
    switch(event, 
      on_click = addHandlerClicked(self$widget, callback),
      stop("Unknown event", event, call. = FALSE)
    )
    self
  }
  
})