Counter <- Object$clone()$do{(
  init <- function() self$count <- 1
  count <- function() {
    self$count <- self$count + 1
    self$count
  }
)}