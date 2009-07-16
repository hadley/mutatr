w <- Window$clone()$do({
  title <- "Tour"

  
  # If you want to share state across the gui:
  tabs <- tabs_object_created_else_where
  # If you want unique state
  tabs <- tabs_object_created_else_where$clone()
  
  # When you don't want to access later
  self$add(Label$clone("This is a label"))
  
  # When you do want to access later
  stop <- Button$clone()$do({
    state <- FALSE
    label <- "Pause"
    
    self$add_handler("on_push", function() self$state <- !self$state)    
  })
  
})


w$show()
w$stop$push()
w$stop$state



