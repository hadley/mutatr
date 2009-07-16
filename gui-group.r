Group <- Widget$clone()$do({
  init <- function(dir = "h") {
    dir <- match.args(dir, c("horizontal", "vertical"))
    self$horizontal <- dir == "horizontal"    
  }

  __set_padding <- function(value) {
    svalue(self$widget) <- value
    self
  }
  __get_padding <- function() {
    svalue(self)
  }
  
  add_space <- function(amount) {
    addSpace(self$widget, amount)
  }
  
  add_spring <- function() {
    addSpring(self$widget)
  }
  
  make_widget <- function() {
    ggroup(self$horizontal)
  }
  
})