Group <- Widget$clone()$do({
  init <- function(dir = "h") {
    dir <- match.arg(dir, c("horizontal", "vertical"))
    self$horizontal <- dir == "horizontal"
    self$widget <- self$build_widget()    
  }
  
  build_widget <- function() {
    ggroup(hor = self$horizontal)
  }

  set_padding <- function(value) {
    svalue(self$widget) <- value
  }
  get_padding <- function() {
    svalue(self)
  }
  
  add_space <- function(amount) {
    addSpace(self$widget, amount)
  }
  
  add_spring <- function() {
    addSpring(self$widget)
  }
 
})