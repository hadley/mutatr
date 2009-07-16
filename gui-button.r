Button <- Object$clone()$do({
  
  __set_border <- function(value) {
    border(self$widget) <- value
  }
  __get_border <- function() {
    border(self$widget)
  }
  
  __set_label <- function(value) {
    svalue(self$widget) <- value
  }
  __get_label <- function(){
    svalue(self$widget)
  }
  
  
  add_handler <- function(event, callback) {
    switch(event, 
      on_click = addhandler(self$widget, callback),
      stop("Unknown event", event, call. = FALSE)
    })
  }
  
})