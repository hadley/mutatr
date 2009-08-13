Group <- Widget$clone()$do({
  self$init <- function(dir = "h") {
    dir <- match.arg(dir, c("horizontal", "vertical"))
    self$horizontal <- dir == "horizontal"
    self$widget <- self$build_widget()    
  }
  
  self$build_widget <- function() {
    ggroup(hor = self$horizontal)
  }

  self$set_padding <- function(value) {
    svalue(self$widget) <- value
  }
  self$get_padding <- function() {
    svalue(self)
  }
  
  self$add_space <- function(amount) {
    addSpace(self$widget, amount)
    self
  }
  
  self$add_spring <- function() {
    addSpring(self$widget)
    self
  }
 
})